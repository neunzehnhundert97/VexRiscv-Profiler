package masterthesis
package profiler

import java.util.concurrent.TimeUnit
import java.lang.{Runtime => JRuntime}

import scala.compiletime.codeOf
import scala.collection.immutable.SortedSet
import scala.util.matching.Regex

import better.files._

import zio.*
import zio.console.*
import zio.clock.*
import zio.blocking.*
import zio.blocking.Blocking.Service
import zio.duration.*
import zio.process.*

import upickle.default.write

import tasks.PredefinedTask

object Controller {

  /** Path to objdump for the analyzed elf. */
  val objdump = "riscv32-unknown-elf-objdump"

  /** Displays the name of a boolean if it is true, else an empty string. */
  inline def literalBoolean(inline cond: Boolean): String =
    if (cond) codeOf(cond).split('.').last else ""

  def apply(args: Seq[String]): ZIO[Console & Blocking & Clock, String, Unit] = {
    val config = Config(args)

    ((config.reportConfig >>= reportStatus("Config")) *> buildProfiler(config) *> execute(config))
      .catchAll(reportError("Config"))
  }

  /** Calls the profiler's makefile with the given arguments if profiling is needed. */
  def buildProfiler(config: Config): ZIO[Console & Blocking, String, Unit] =
    ZIO.when(config.doProfile && !config.doPreflight) {
      // Invoke makefile to build profiler and request additional targets for each variant
      for {
        _ <- reportStatus("Profiler")("Building verilator simulation")
        r <- runForReturn("make", "all", config.profilerMakeFlags.mkString(" "))
          .mapError(e => s"The profiler could not be build: $e")
      } yield ()
    }

  /** Builds the simulation with a given dependency. */
  def buildProfilerWithDeps(depHash: String, config: Config) =
    runForReturn(
      "make",
      "all",
      config.profilerMakeFlags.mkString(" "),
      s"DEP_HASH=$depHash",
      "PREFLIGHT=Y"
    ).mapError(e => s"The profiler could not be build: $e")

  /** Executes all tasks in parallel. */
  def execute(config: Config): URIO[Console & Blocking & Clock, Unit] = {
    import config.*

    // Create CLI tasks
    // TODO: Errror handling
    val manualTasks = for (file <- manualInputs)
      yield ProfilingTask("manual", file, file, "", config = config)

    // Create predefined tasks
    val predefinedTasks = config.predefinedTasks.flatMap(_.generateTasks(config))

    val tasks = manualTasks ::: predefinedTasks

    for {
      // Execute in parallel
      data <- if (doAnalysis || doProfile) executeTasks(tasks, config) else ZIO.succeed(Nil)
      _ <- ZIO.when(doBenchmark)(if (data.nonEmpty) benchmark(data, config)
      else ZIO.fail("No benchmark possible as all tasks failed."))
        .catchAll(e => reportError("Profiler")(s"During benchmark, an error occurred: $e"))
      _ <- ZIO.when(!doAnalysis && !doProfile && !doBenchmark)(reportStatus("Profiler")("No tasks to execute"))
    } yield ()
  }

  /** Handles the execution of all requested tasks. */
  def executeTasks(
    tasks: List[ProfilingTask],
    config: Config
  ): URIO[Console & Blocking & Clock, List[ProfilingTask -> AnalysisResult]] = for {

    // Prepare a count down latch in pieces
    countDown <- CountDownLatch.make[String](tasks.length)

    // The fiber ref is set to be only modied by the current fiber and is not inherited, used by the logger
    ref <- Ref.make[SharedState](SharedState(tasks.map(_ -> TaskInformation()).toMap, TaskCommon(countDown = countDown)))

    // Semaphores for controlling the number of tasks in the same phase to prevent RAM overflows etc.
    semBuild <- Semaphore.make(JRuntime.getRuntime().availableProcessors() >> 1 - 1)
    semProfile <- Semaphore.make(config.profileThreads.getOrElse(JRuntime.getRuntime().availableProcessors() - 1))
    semAnalyse <- Semaphore.make(config.analysisThreads.getOrElse(JRuntime.getRuntime().availableProcessors() - 1))

    // Actual execution in parallel
    executor <- ZIO.partitionPar(tasks)(_.execute(config, ref, semBuild, semProfile, semAnalyse)).fork

    // Start logging
    start <- currentTime(TimeUnit.SECONDS)
    logger <- reportFibreStatus(ref, start, tasks.length)
      .repeat(Schedule.fixed(1.seconds)).ignore.fork
    _ <- reportStatus("Profiler")(s"Start execution of ${tasks.length} tasks")

    // Wait for completion
    res <- (logger *> executor).join

    // Cleanup shares resources
    _ <- runForEffect("make", "cleanAll").ignore

    // Report errors after execution
    (errors, success) = res
    _ <- ZIO.collectAll(errors.map { (task, error) =>
      val lines = error.split("\n")
      for {
        state <- ref.map(_.individual(task).state).get
        report = if (state == TaskState.Stopped) reportWarning(task.name) else reportError(task.name)
        _ <-
          if (lines.length == 1)
            report(error)
          else
            report(s"Error message with ${lines.length} lines was dumped to ${task.resultFile}.error")
              *> writeToFile(task.resultFile + ".error")(error).ignore
      } yield ()
    })
    _ <- logger.interrupt
  } yield success.collect { case Some(a) => a }.toList

  /** Prints a self-overwriting line that tells in which phase each fiber is currently in. */
  def reportFibreStatus(
    ref: SharedRef,
    begin: Long,
    tasks: Int
  ): ZIO[Clock & Console, String, Unit] = for {
    // Unwrap data
    states <- ref.map(_.individual).get

    // Count states
    countedStates = states.foldLeft(Map[TaskState, Int]()) {
      case (map, (_, info)) =>
        map.updatedWith(info.state) {
          case None    => Some(1)
          case Some(c) => Some(c + 1)
        }
    }
    printString = countedStates.toList.sortBy(_._1.ordinal).map((a, b) => s"$a [$b]").mkString(" ==> ")

    // Print status
    now <- currentTime(TimeUnit.SECONDS)
    elapsed = now - begin
    _ <- reportUpdatedStatus("Profiler")(s"$elapsed seconds elaspsed, Current State: $printString")
    // When all states are final, signal end by setting an error
    _ <- ZIO.when(states.nonEmpty && states.forall((_, state) => state._1.isFinal))(putStr("\n").ignore *> ZIO.fail(""))
  } yield ()

  /** Performs the profiling measurements, expects the executable to be properly built. */
  def profile(hexFile: String, dataFile: String, config: Config, variant: Option[String]): ZIO[Blocking, String, Unit] = for {
    // Verify existence of files
    _ <- IO.when(!hexFile.endsWith("hex"))(IO.fail(s"The given file '$hexFile' has no .hex extension."))
    _ <- IO.effect(File(hexFile).exists).flatMap(if (_) ZIO.unit else ZIO.fail(s"File '$hexFile' does not exist."))
      .mapError(_ => s"File '$hexFile' is not accessible or does not exist.")

    // Get symbol table and dump it for later
    symbolTable <- makeSymbolTable(hexFile.replace(".hex", ".elf"))
    _ <- dumpSymbolTable(symbolTable, s"$dataFile-sym.json").mapError(e => s"Symbol table could not be dumped because: $e")

    // Assemble the path to the simulation executable
    executable =
      if (config.doPreflight) s"./verilator/obj_dir${variant.map(v => s"_$v").getOrElse("")}/VVexRiscv"
      else "./verilator/obj_dir/VVexRiscv"

    // Do the actual profiling
    _ <- config.debuggedFunction.match {
      case None =>
        // Call profiler
        if (config.detailed)
          runForFileOutput(dataFile)(
            executable,
            hexFile,
            config.bootAt,
            "1"
          ).mapError(e => s"The profiler exited with a non zero exit value: $e")
        else
          runForFileOutput(dataFile)(
            (List(
              executable,
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
              executable,
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
      case task -> (data: GroupedInstructions) => ZIO.succeed(task -> data.map(_._2.map(_._2.sum.toLong).sum).sum)
    })

    val synthesisData =
      if (config.doSynthesis) (
        for {
          taskToHash <-
            ZIO.collectAllPar(data.map(p => readFromFile(s"${p._1.dataFile}-coreHash").map(f => p._1 -> f.toList.head)))
          synth <- Synthesis.requestSynthesisResults(taskToHash.map(_._2))
        } yield taskToHash.map((t, h) => t -> (synth.get(h))).collect { case (p -> Some(v)) => p -> v }.toMap
      ).mapError(e => s"Could not retrieve synthesis data because: $e")
      else ZIO.succeed(Map())

    // Extract the clock cycles
    (synthesisData <*> extractedData).map { (synth, data) =>

      val variantToIndex = config.variants.zipWithIndex.toMap

      // Group data: Line / Variant => Column / Version => Data
      val groupedByVariant: List[(String, List[(String, Long)], Option[((Double, Int), (Double, Int))])] = data
        .groupBy(_._1.variant.get)
        .map((key, value) => (key, value.map((t, data) => (t.version, data)).sortBy(_._1), synth.get(value.head._1)))
        .toList.sortBy((v, _, _) => variantToIndex(v))

      // Group data: Version => Variant => Data
      val groupedByVersion: List[(String, List[(String, Long)])] = data
        .groupBy(_._1.version)
        .map((key, value) => key -> value.map((t, data) => t.variant.get -> data).sortBy((v, _) => variantToIndex(v)))
        .toList.sortBy(_._1)

      // Build report
      val maxVariantLength = math.max(groupedByVariant.map(_._1.length).max, 6)
      val numVersions = groupedByVersion.length
      val numVariants = groupedByVariant.length

      val sep =
        "+" + ("-" * (maxVariantLength + 2)) + (("+" + "-" * 15) * numVersions)
          + (if (config.doSynthesis) ("+" + "-" * 10 + "+" + "-" * 11 + "+" + "-" * 11 + "+" + "-" * 12) else "") + "+\n"
      val header = "| " + ("Var" ^ maxVariantLength) + " | " + groupedByVersion.map((v, _) => f"$v%13s").mkString(" | ")
        + (if (config.doSynthesis) " | Area MHz | Area BELs | Speed Mhz | Speed BELs" else "") + " |\n"
      val table = (for ((variant, data, synth) <- groupedByVariant)
        yield s"| %${maxVariantLength}s | %s |".format(
          variant,
          data.map((_, cycles) => if (cycles == -1) "-" ^ 13 else f"$cycles%13s").mkString(" | "),
          synth.map(_.toString).getOrElse("-" * 13)
        ) + (if (config.doSynthesis) synth match {
               case Some((areaFreq, areaSize), (speedFreq, speedSize)) =>
                 f" ${areaFreq}%8.3f | ${areaSize}%9d | ${speedFreq}%9.3f | ${speedSize}%10d |"
               case None => f" ${"-" * 8}%s | ${"-" * 9}%s | ${"-" * 9}%s | ${"-" * 10}%s |"
             }
             else ""))
        .mkString("", "\n", "\n")

      // Table with all measurments
      val resultTable = sep + header + sep + table + sep

      // Table for each version with variants compared
      val compareTables = for ((version, data) <- groupedByVersion)
        yield {
          val sep = "+" + ("-" * (maxVariantLength + 2)) + (("+" + "-" * (maxVariantLength + 2)) * numVariants) + "+\n"
          val header =
            "| " + ("Var" ^ maxVariantLength) + " | " + groupedByVariant.map((v, _, _) => v ^ maxVariantLength).mkString(
              " | "
            ) + " |\n"
          val table = (for ((variant, value1) <- data)
            yield s"| %${maxVariantLength}s | %s |".format(
              variant,
              data.map((va, value2) =>
                if (value1 == -1 || value2 == -1) "-" ^ 7
                else s"%${maxVariantLength}.2f".format(value1 * 100.0 / value2)
              ).mkString(" | ")
            )).mkString("", "\n", "\n")
          s"$version\n" + sep + header + sep + table + sep
        }

      resultTable + "\n\n" + compareTables.mkString("\n")
    } >>= writeToFile(s"results/${data.head._1.group}/${config.prefixed("Benchmark")}.txt")
  }.mapError(e => s"Wiriting the benchmark failed because: $e")

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
