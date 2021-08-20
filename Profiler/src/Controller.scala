package masterthesis
package profiler

import java.util.concurrent.TimeUnit
import java.lang.{Runtime => JRuntime}

import scala.compiletime.codeOf

import better.files._

import zio.*
import zio.console.*
import zio.clock.*
import zio.blocking.*
import zio.duration.*

import zio.process.Command

import upickle.default.write

import tasks.PredefinedTask
import scala.collection.immutable.SortedSet
import scala.util.matching.Regex
import zio.process.CommandError
import zio.blocking.Blocking.Service

object Controller {

  /** Path to objdump for the analyzed elf. */
  val objdump = "riscv32-unknown-elf-objdump"

  /** Displays the name of a boolean if it is true, else an empty string. */
  inline def literalBoolean(inline cond: Boolean): String =
    if (cond) codeOf(cond).split('.').last else ""

  def apply(args: Seq[String]): ZIO[Console & Blocking & Clock, String, Unit] = {
    val config = Config(args)

    (config.reportUselessConfig *> buildProfiler(config) *> execute(config))
      .catchAll(e => reportError(e))
  }

  /** Calls the profiler's makefile with the given arguments if profiling is needed. */
  def buildProfiler(config: Config): ZIO[Console & Blocking, String, Unit] =
    ZIO.when(config.doProfile) {
      // Invoke makefile to build profiler and request additional targets
      for {
        _ <- reportStatus("Building verilator simulation")
        r <- runForReturn("make", "all", config.profilerMakeFlags.mkString(" "))
          .mapError(e => s"The profiler could not be build: $e")
      } yield ()
    }

  /** Executes all tasks in parallel. */
  def execute(config: Config): URIO[Console & Blocking & Clock, Unit] = {
    import config.*

    // Create CLI tasks
    // TODO: Errror handling
    val manualTasks = for (file <- manualInputs)
      yield ProfilingTask(file, file, "", config = config)

    // Create predefined tasks
    val predefinedTasks = config.predefinedTasks.flatMap(_.generateTasks(config))

    val tasks = manualTasks ::: predefinedTasks

    for {
      // Execute in parallel
      data <- if (doAnalysis || doProfile) executeTasks(tasks, config) else ZIO.succeed(Nil)
      _ <- ZIO.when(doBenchmark)(benchmark(data, config))
        .catchAll(e => reportError(s"During benchmark, an error occurred: $e"))
      _ <- ZIO.when(!doAnalysis && !doProfile && !doBenchmark)(reportStatus("No tasks to execute"))
    } yield ()
  }

  /** */
  def executeTasks(
    tasks: List[ProfilingTask],
    config: Config
  ): URIO[Console & Blocking & Clock, List[ProfilingTask -> AnalysisResult]] = for {
    // The fiber ref is set to be only modied by the current fiber and is not inherited, used by the logger
    ref <- Ref.make[Map[ProfilingTask, TaskState]](Map())

    // Semaphores for controlling the number of tasks in the same phase to prevent RAM overflows etc.
    semProfile <- Semaphore.make(config.profileThreads.getOrElse(JRuntime.getRuntime().availableProcessors() - 1))
    semAnalyse <- Semaphore.make(config.analysisThreads.getOrElse(JRuntime.getRuntime().availableProcessors() - 1))

    // Actual execution in parallel
    executor <- ZIO.partitionPar(tasks)(_.execute(config, ref, semProfile, semAnalyse)).fork

    // Start logging
    start <- currentTime(TimeUnit.SECONDS)
    logger <- reportFibreStatus(ref, start, tasks.length)
      .repeat(Schedule.fixed(1.seconds)).ignore.fork
    _ <- reportStatus(s"Start execution of ${tasks.length} tasks")

    // Wait for completion
    res <- (logger *> executor).join

    // Report errors after execution
    (errors, success) = res
    _ <- ZIO.collectAll(errors.map(e => reportError(e)))
    _ <- logger.interrupt
  } yield success.collect { case Some(a) => a }.toList

  /** Prints a self-overwriting line that tells in which phase each fiber is currently in. */
  def reportFibreStatus(
    ref: Ref[Map[ProfilingTask, TaskState]],
    begin: Long,
    tasks: Int
  ): ZIO[Clock & Console, String, Unit] = for {
    // Unwrap data
    states <- ref.get

    // Count states
    countedStates = states.foldLeft(Map[TaskState, Int]()) {
      case (map, (_, state)) =>
        map.updatedWith(state) {
          case None    => Some(1)
          case Some(c) => Some(c + 1)
        }
    }
    printString = countedStates.toList.sortBy(_._1.ordinal).map((a, b) => s"$a [$b]").mkString(" ==> ")

    // Print status
    now <- currentTime(TimeUnit.SECONDS)
    elapsed = now - begin
    _ <- reportUpdatedStatus(s"$elapsed seconds elaspsed, Current State: $printString")
    _ <-
      if (states.nonEmpty && states.forall((_, state) => state == TaskState.Finished || state == TaskState.Failed) && elapsed > 2)
        putStr("\n").ignore *> ZIO.fail("")
      else ZIO.unit
  } yield ()

  /** Performs the profiling measurements, expects the executable to be properly built. */
  def profile(hexFile: String, dataFile: String, config: Config): ZIO[Blocking, String, Unit] = for {
    // Verify existence of files
    _ <- IO.when(!hexFile.endsWith("hex"))(IO.fail(s"The given file '$hexFile' has no .hex extension."))
    _ <- IO.effect(File(hexFile).exists).flatMap(if (_) ZIO.unit else ZIO.fail(s"File '$hexFile' does not exist."))
      .mapError(_ => s"File '$hexFile' is not accessible or does not exist.")

    // Get symbol table and dump it for later
    symbolTable <- makeSymbolTable(hexFile.replace(".hex", ".elf"))
    _ <- dumpSymbolTable(symbolTable, s"$dataFile-sym.json").mapError(e => s"Symbol table could not be dumped because: $e")

    // Do the actual profiling
    _ <- config.debuggedFunction.match {
      case None =>
        // Call profiler
        if (!config.experimentalProfiling)
          runForFileOutput(dataFile)(
            "./obj_dir/VVexRiscv",
            hexFile,
            config.bootAt,
            "1"
          ).mapError(e => s"The profiler exited with a non zero exit value: $e")
        else
          runForFileOutput(dataFile)(
            (List(
              "./obj_dir/VVexRiscv",
              hexFile,
              config.bootAt,
              "3"
            ) ++ symbolTable.map(_._1).toList.sorted.reverse)*
          ).mapError(e => s"The profiler exited with a non zero exit value: $e")

      case Some(func) =>
        symbolTable.find(_._2 == func) match {
          case None               => ZIO.fail(s"The given symbol '$func' could not be found in the symbol table.")
          case Some(address -> _) =>
            // Call profiler
            runForFileOutput(dataFile)(
              "./obj_dir/VVexRiscv",
              hexFile,
              config.bootAt,
              "2",
              address,
              config.desiredCalls.toString
            ).mapError(_ => "The profiler exited with a non zero exit value")
        }
    }
  } yield ()

  /** Generate a comparison of the absolute execution time of the measured task. */
  def benchmark(data: List[ProfilingTask -> AnalysisResult], config: Config) = {
    // Get timing data from analysis result
    val extractedData = ZIO.collectAll(data.map {
      case task -> (data: CallTreeData)        => ZIO.succeed(task -> data._1.totalTime)
      case task -> (data: GroupedInstructions) => ZIO.fail("Benchmark on low level analysis is not implemented")
    })

    // Extract the clock cycles
    extractedData.map { data =>

      // Group data: Line / Variant => Column / Version => Data
      val groupedByVariant: List[(Int, List[(String, Long)])] = data
        .groupBy(_._1.variant.get)
        .map((key, value) => key -> value.map((t, data) => t.version -> data).sortBy(_._1)).toList.sortBy(_._1)

      // Group data: Version => Variant => Data
      val groupedByVersion: List[(String, List[(Int, Long)])] = data
        .groupBy(_._1.version)
        .map((key, value) => key -> value.map((t, data) => t.variant.get -> data).sortBy(_._1)).toList.sortBy(_._1)

      // Build report
      val numVersions = groupedByVersion.length
      val numVariants = groupedByVariant.length
      val sep = "+-----" + (("+" + "-" * 15) * numVersions) + "+\n"
      val header = "| Var | " + groupedByVersion.map((v, _) => f"$v%13s").mkString(" | ") + " |\n"
      val table = (for ((variant, data) <- groupedByVariant)
        yield f"| $variant%3s | ${data.map(d => if (d._2 == -1) f"      -      " else f"${d._2}%13s").mkString(" | ")}" + " |")
        .mkString("", "\n", "\n")

      // Table with all measurments
      val resultTable = sep + header + sep + table + sep

      // Table for each version with variants compared
      val compareTables = for ((version, data) <- groupedByVersion)
        yield {
          val sep = "+-----" + (("+" + "-" * 9) * numVariants) + "+\n"
          val header = "| Var | " + groupedByVariant.map((v, _) => f"$v%7s").mkString(" | ") + " |\n"
          val table = (for ((variant, value1) <- data)
            yield f"| $variant%3s | ${data.map((va, value2) =>
              if (value1 == -1 || value2 == -1) "   -   "
              else f"${value1 * 100.0 / value2}%7.2f"
            ).mkString(" | ")}" + " |")
            .mkString("", "\n", "\n")

          s"$version\n" + sep + header + sep + table + sep
        }

      resultTable + "\n\n" + compareTables.mkString("\n")
    } >>= writeToFile(s"results/${config.prepostfixed("Benchmark")}.txt")
  }

  /** Converts the symbol table into JSON and writes it in the data folder. */
  def dumpSymbolTable(data: Map[String, String], json: String): Task[Unit] =
    writeToFile(json)(write[Map[String, String]](data))

  /** Creates the symbol table of a given elf and return two mappings from addresses to symbols, one with longs and one with
    * strings.
    */
  def makeSymbolTable(elf: String): ZIO[Blocking, String, Map[String, String]] =
    for {
      // Verify existence of file
      _ <- IO.effect(File(elf).exists).flatMap(if (_) ZIO.unit else ZIO.fail(s"File '$elf' does not exist."))
        .mapError(_ => s"File '$elf' is not accessible or does not exist.")

      // Read symbol and create mapping
      result <- runForReturn(Controller.objdump, "-t", elf).mapError(_ => "The symbol table could not be created")
      _ <- ZIO.when(result._1 != 0)(ZIO.fail("The symbol table could not be created"))
    } yield {
      val (code, data, error) = result

      // Split lines, split columns
      val stringToSymbols = data.split("\n").map(_.split(" "))
      // take address and symbol name
        .filter(line => line.length > 2 && line.contains("F")).map(a => raw"[0-9,a-f,A-F]{8}".r.findFirstIn(a.head) -> a.last)
        .collect { case (Some(head), last) => head.toUpperCase -> last.strip.replace(".", "dot") }.toMap

      stringToSymbols
    }
}
