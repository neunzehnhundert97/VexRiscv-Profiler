package masterthesis
package profiler

import scala.compiletime.codeOf

import better.files._

import zio.*
import zio.console.*
import zio.clock.*
import zio.blocking.*
import zio.duration.*

import upickle.default.write

import tasks.PredefinedTask
import java.util.concurrent.TimeUnit

object ManualProfiling {

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
        r <- runForReturn("make", "all", config.profilerMakeFlags)
        ret <- ZIO.when(r.exitCode != 0)(ZIO.fail(s"The profiler could not be build: ${r.out.text}"))
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

    val policy = Schedule
      .spaced(1000.milliseconds)

    val tasks = manualTasks ::: predefinedTasks

    for {
      supervisor <- Supervisor.track(true)
      ref <- FiberRef.make[String -> TaskState]("" -> TaskState.Initial)
      start <- currentTime(TimeUnit.SECONDS)
      logger <- reportFibreStatus(supervisor, ref, start, tasks.length).schedule(policy).ensuring(for {
        now <- currentTime(TimeUnit.SECONDS)
        _ <- reportUpdatedStatus(s"${now - start} seconds elaspsed, Current State Finished: ${tasks.length}\n")
      } yield ()).fork

      // Execute in parallel
      _ <-
        if (tasks.nonEmpty || doBenchmark) {
          reportStatus(s"Start execution of ${tasks.length} tasks")
            *> ZIO.when(doAnalysis || doProfile)(
              ZIO.collectAllParN(12)(tasks.map(t => t.execute(config, ref))).supervised(supervisor).map(_ => ())
            )
            *> ZIO.when(doBenchmark)(benchmark(tasks)).catchAll(e => reportError(s"During benchmark, an error occurred: $e"))
        } else
          reportStatus("No tasks to execute")
      _ <- logger.interrupt
    } yield ()
  }

  /** */
  def reportFibreStatus(
    supervisor: Supervisor[Chunk[Fiber.Runtime[Any, Any]]],
    ref: FiberRef[String -> TaskState],
    begin: Long,
    tasks: Int
  ) = for {
    wrappedStates <- supervisor.value.map(_.map(_.getRef(ref)))
    states <- ZIO.collectAll(wrappedStates)
    countedStates = states.filter((name, _) => !name.isBlank).foldLeft(Map[TaskState, Int]()) {
      case (map, (_, state)) =>
        map.updatedWith(state) {
          case None    => Some(1)
          case Some(c) => Some(c + 1)
        }
    }
    fullStates = countedStates.updated(TaskState.Finished, tasks - countedStates.map(_._2).sum)
    now <- currentTime(TimeUnit.SECONDS)
    _ <- ZIO.when(countedStates.map(_._2).sum != 0)(reportUpdatedStatus(
      s"${now - begin} seconds elaspsed, Current State: ${fullStates.map((a, b) => s"$a: $b").mkString(", ")}"
    ))
  } yield ()

  /** Performs the profiling measurements, expects the executable to be properly built. */
  def profile(hexFile: String, dataFile: String, config: Config): ZIO[Blocking, String, Unit] = for {
    // Verify existence of file
    _ <- IO.when(!hexFile.endsWith("hex"))(IO.fail(s"The given file '$hexFile' has no .hex extension."))
    _ <- IO.effect(File(hexFile).exists).flatMap(if (_) ZIO.unit else ZIO.fail(s"File '$hexFile' does not exist."))
      .mapError(_ => s"File '$hexFile' is not accessible or does not exist.")
    symbolTable <- makeSymbolTable(hexFile.replace(".hex", ".elf"))
    _ <- dumpSymbolTable(symbolTable, s"$dataFile-sym.json").mapError(e => s"Symbol table could not be dumped because: $e")
    _ <- config.debuggedFunction.match {
      case None =>
        // Call profiler
        runForFileOutput(os.pwd / os.RelPath(dataFile))("./obj_dir/VVexRiscv", hexFile, config.bootAt, 1)
          .mapError(_ => "The profiler exited with a non zero exit value")
      case Some(func) =>
        symbolTable.find(_._2 == func) match {
          case None               => ZIO.fail(s"The given symbol '$func' could not be found in the symbol table.")
          case Some(address -> _) =>
            // Call profiler
            runForFileOutput(os.pwd / os.RelPath(dataFile))(
              "./obj_dir/VVexRiscv",
              hexFile,
              config.bootAt,
              2,
              address,
              config.desiredCalls
            ).mapError(_ => "The profiler exited with a non zero exit value")
        }
    }
  } yield ()

  def benchmark(tasks: List[ProfilingTask]) = {
    // Read data from files
    val files = tasks.map(t => t -> t.dataFile)
    val dataIterator = ZIO.collectAll(files.map((name, file) => readFromFile(file).map(res => name -> res)))

    // TODO Use grep
    val timeRegex = raw"Had simulate (\d+) clock cycles".r

    // Match the last line where the overall execution time is print
    dataIterator.map(_.map((name, output) =>
      name -> output.map(line => timeRegex.findFirstMatchIn(line))
        .collect { case Some(m) => m.group(1).toInt }.toList
    )).map { data =>
      // Group data: Line / Variant => Column / Version => Data
      val groupedByVariant = data
        .groupBy(_._1.variant.get)
        .map((key, value) => key -> value.map((t, data) => t.version -> data.head).sortBy(_._1)).toList.sortBy(_._1)

      // Group data: Version => Variant => Data
      val groupedByVersion = data
        .groupBy(_._1.version)
        .map((key, value) => key -> value.map((t, data) => t.variant.get -> data.head).sortBy(_._1)).toList.sortBy(_._1)

      // Build report
      val numVersions = groupedByVersion.length
      val numVariants = groupedByVariant.length
      val sep = "+-----" + (("+" + "-" * 12) * numVersions) + "+\n"
      val header = "| Var | " + groupedByVersion.map((v, _) => f"$v%10s").mkString(" | ") + " |\n"
      val table = (for ((variant, data) <- groupedByVariant)
        yield f"| $variant%3s | ${data.map(d => f"${d._2}%10s").mkString(" | ")}" + " |").mkString("", "\n", "\n")

      // Table with all measurments
      val resultTable = sep + header + sep + table + sep

      // Table for each version with variants compared
      val compareTables = for ((version, data) <- groupedByVersion)
        yield {
          val sep = "+-----" + (("+" + "-" * 9) * numVariants) + "+\n"
          val header = "| Var | " + groupedByVariant.map((v, _) => f"$v%7s").mkString(" | ") + " |\n"
          val table = (for ((variant, value1) <- data)
            yield f"| $variant%3s | ${data.map((va, value2) => f"${value1 * 100.0 / value2}%7.2f").mkString(" | ")}" + " |")
            .mkString("", "\n", "\n")

          s"$version\n" + sep + header + sep + table + sep
        }

      resultTable + "\n\n" + compareTables.mkString("\n")
    } >>= writeToFile("results/Benchmark.txt")
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
      data <- runForReturn(ManualProfiling.objdump, "-t", elf)
      _ <- ZIO.when(data.exitCode != 0)(ZIO.fail("The symbol table could not be created"))
    } yield {
      // Split lines, split columns
      val stringToSymbols = data.out.text.split("\n").map(_.split(" "))
      // take address and symbol name
        .filter(_.length > 2).map(a => raw"[0-9,a-f,A-F]{8}".r.findFirstIn(a.head) -> a.last)
        .collect { case (Some(head), last) => head.toUpperCase -> last.strip.replace(".", "dot") }.toMap

      stringToSymbols
    }
}
