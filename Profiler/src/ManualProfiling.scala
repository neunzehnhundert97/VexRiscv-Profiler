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

import upickle.default.write

import tasks.PredefinedTask

object ManualProfiling {

  /** Path to objdump for the analyzed elf. */
  val objdump = "riscv32-unknown-elf-objdump"

  val everySecond = Schedule.spaced(1000.milliseconds)

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
      ref <- FiberRef.make[String -> TaskState]("" -> TaskState.Initial)
      supervisor <- Supervisor.track(true)
      start <- currentTime(TimeUnit.SECONDS)
      logger <- reportFibreStatus(supervisor, ref, start, tasks.length).schedule(everySecond).ensuring(for {
        now <- currentTime(TimeUnit.SECONDS)
        _ <- reportUpdatedStatus(s"${now - start} seconds elaspsed, Current State Finished: ${tasks.length}\n")
      } yield ()).fork
      sem <- Semaphore.make(JRuntime.getRuntime().availableProcessors())

      // Execute in parallel
      results <-
        if (tasks.nonEmpty || doBenchmark) {
          for {
            _ <- reportStatus(s"Start execution of ${tasks.length} tasks")
            res <- ZIO.when(doAnalysis || doProfile)(
              ZIO.partitionPar(tasks)(_.execute(config, ref, sem)).supervised(supervisor).flatMap { (errors, successes) =>
                ZIO.collectAll(errors.map(e => reportError(e)))
              }
            )
            _ <- ZIO.when(doBenchmark)(benchmark(tasks, config)).catchAll(e =>
              reportError(s"During benchmark, an error occurred: $e")
            )
          } yield ()
        } else
          reportStatus("No tasks to execute")
      _ <- logger.interrupt
    } yield ()
  }

  /** Prints a self-overwriting line that tells in which phase each fiber is currently in. */
  def reportFibreStatus(
    supervisor: Supervisor[Chunk[Fiber.Runtime[Any, Any]]],
    ref: FiberRef[String -> TaskState],
    begin: Long,
    tasks: Int
  ) = for {
    fibres <- supervisor.value
    wrappedStates = fibres.map(_.getRef(ref))
    states <- ZIO.collectAll(wrappedStates)
    countedStates = states.filter(!_._1.isEmpty).foldLeft(Map[TaskState, Int]()) {
      case (map, (_, state)) =>
        map.updatedWith(state) {
          case None    => Some(1)
          case Some(c) => Some(c + 1)
        }
    }
    finished = tasks - countedStates.map(_._2).sum
    fullStates = if (finished > 0) countedStates.updated(TaskState.Finished, finished) else countedStates
    now <- currentTime(TimeUnit.SECONDS)
    _ <- ZIO.when(countedStates.map(_._2).sum != 0)(reportUpdatedStatus(
      s"${now - begin} seconds elaspsed, Current State: ${fullStates.map((a, b) => s"$a: $b").mkString(", ")}"
    ))
  } yield ()

  /** Performs the profiling measurements, expects the executable to be properly built. */
  def profile(hexFile: String, dataFile: String, config: Config): ZIO[Blocking & Console, String, Unit] = for {
    // Verify existence of file
    _ <- IO.when(!hexFile.endsWith("hex"))(IO.fail(s"The given file '$hexFile' has no .hex extension."))
    _ <- IO.effect(File(hexFile).exists).flatMap(if (_) ZIO.unit else ZIO.fail(s"File '$hexFile' does not exist."))
      .mapError(_ => s"File '$hexFile' is not accessible or does not exist.")
    symbolTable <- makeSymbolTable(hexFile.replace(".hex", ".elf"))
    _ <- dumpSymbolTable(symbolTable, s"$dataFile-sym.json").mapError(e => s"Symbol table could not be dumped because: $e")
    _ <- config.debuggedFunction.match {
      case None =>
        // Call profiler
        runForFileOutput(dataFile)("./obj_dir/VVexRiscv", hexFile, config.bootAt, "1")
          .mapError(_ => "The profiler exited with a non zero exit value")
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
  def benchmark(tasks: List[ProfilingTask], config: Config): Task[Unit] = {
    // Use grep to find the line which prints the cycles
    val grepedData =
      ZIO.collectAllPar(for (task <- tasks)
        yield IO {
          val head = os.proc("tail", "-n", 10, task.dataFile).spawn()
          val grep = os.proc("grep", "SUCCESS").spawn(stdin = head.stdout)
          grep.waitFor()
          task -> grep.stdout.text
        })

    val timeRegex = raw"^SUCCESS, (\d+) clock cycles ".r

    // Extract the clock cycles
    grepedData.map(_.map((name, output) =>
      name -> timeRegex.findFirstMatchIn(output).map(_.group(1).toLong(16)).getOrElse(-1L)
    )).map { data =>

      // Group data: Line / Variant => Column / Version => Data
      val groupedByVariant = data
        .groupBy(_._1.variant.get)
        .map((key, value) => key -> value.map((t, data) => t.version -> data).sortBy(_._1)).toList.sortBy(_._1)

      // Group data: Version => Variant => Data
      val groupedByVersion = data
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
    } >>= writeToFile(s"results/${config.prepostfixed("Benchmark.txt")}")
  }

  /** Converts the symbol table into JSON and writes it in the data folder. */
  def dumpSymbolTable(data: Map[String, String], json: String): Task[Unit] =
    writeToFile(json)(write[Map[String, String]](data))

  /** Creates the symbol table of a given elf and return two mappings from addresses to symbols, one with longs and one with
    * strings.
    */
  def makeSymbolTable(elf: String): ZIO[Blocking & Console, String, Map[String, String]] =
    for {
      // Verify existence of file
      _ <- IO.effect(File(elf).exists).flatMap(if (_) ZIO.unit else ZIO.fail(s"File '$elf' does not exist."))
        .mapError(_ => s"File '$elf' is not accessible or does not exist.")
      // Read symbol and create mapping
      result <- runForReturn(ManualProfiling.objdump, "-t", elf).mapError(_ => "The symbol table could not be created")
      _ <- ZIO.when(result._1 != 0)(ZIO.fail("The symbol table could not be created"))
    } yield {
      val (code, data, error) = result
      // Split lines, split columns
      val stringToSymbols = data.split("\n").map(_.split(" "))
      // take address and symbol name
        .filter(_.length > 2).map(a => raw"[0-9,a-f,A-F]{8}".r.findFirstIn(a.head) -> a.last)
        .collect { case (Some(head), last) => head.toUpperCase -> last.strip.replace(".", "dot") }.toMap

      stringToSymbols
    }
}
