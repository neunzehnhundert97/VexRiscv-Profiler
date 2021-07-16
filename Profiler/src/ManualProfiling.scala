package masterthesis
package profiler

import scala.collection.parallel.CollectionConverters._
import scala.compiletime.codeOf

import better.files._

import tasks.PredefinedTask

object ManualProfiling {

  /** Path to objdump for the analyzed elf. */
  val objdump = "riscv32-unknown-elf-objdump"

  /** Displays the name of a boolean if it is true, else an empty string. */
  inline def literalBoolean(inline cond: Boolean): String =
    if (cond) codeOf(cond).split('.').last else ""

  def apply(args: Seq[String]): Unit = {
    val config = Config(args)
    import config.*

    // Build profiler
    buildProfiler(config) match {
      case Error(msg) =>
        reportError(msg)
      case Success(_) =>
        // Create tasks
        val manualTasks = for (file <- manualInputs)
          yield Task(file, file)

        val predefinedTasks = config.predefinedTasks.flatMap { pre =>
          // Handle failed tasks descriptions
          val tasks = pre.generateTasks.flatMap {
            case Right(task) => List(task)
            case Left(msg) =>
              reportError(msg, pre.name)
              Nil
          }
          // Handle takes and drops
          (config.take, config.drop).match {
            case None -> None             => tasks
            case Some(take) -> None       => tasks.take(take)
            case None -> Some(drop)       => tasks.drop(drop)
            case Some(take) -> Some(drop) => tasks.drop(drop).take(take)
          }
        }

        // Excute in parallel
        val tasks = manualTasks ::: predefinedTasks
        if (tasks.nonEmpty) {
          reportStatus(s"Start execution of ${tasks.length} tasks")
          tasks.par.foreach(_.execute(config))
        } else
          reportStatus("No tasks to execute")
    }
  }

  /** Calls the profiler's makefile with the given arguments if profiling is needed. */
  def buildProfiler(config: Config): ErrorOrSuccess = {
    import config.*
    if (doProfile) {
      reportStatus("Building verilator simulation")

      // Invoke makefile to build profiler and request additional targets
      val r = os.proc(
        "make",
        config.predefinedTasks.map(_.name).mkString(" "),
        "all",
        s"PROFILE=Y ${profilerMakeFlags.mkString(" ")}"
      ).call(check = false, mergeErrIntoOut = true)
      if (r.exitCode == 0)
        Success
      else Error(s"The profiler could not be build: ${r.out.text}")
    } else Success
  }

  /** Performs the profiling measurements, expects the executable to be properly built. */
  def profile(hexFile: String, dataFile: String, config: Config): ErrorOrSuccess =
    config.debuggedFunction match {
      case None =>
        // Call profiler
        try {
          os.proc("./obj_dir/VVexRiscv", hexFile, config.bootAt, 1).call(stdout = os.Path(dataFile))
          Success
        } catch case e: os.SubprocessException => Error("The profiler exited with a non zero exit value")
      case Some(func) =>
        // Get function address from symbol table
        try {
          val symbolTable = os.proc(objdump, "-t", s"${hexFile.replace(".hex", ".elf")}").spawn()
          // Match with grep the function name as a single word
          val grep = os.proc("grep", "-P", raw"'\b$func\b'").call(stdin = symbolTable.stdout)
          val functionAddress = grep.out.text.split(" ").head.strip

          // Call profiler
          try {
            os.proc("./obj_dir/VVexRiscv", hexFile, config.bootAt, 2, functionAddress, config.desiredCalls)
              .call(stdout = os.Path(dataFile))
            Success
          } catch case e: os.SubprocessException => Error("The profiler exited with a non zero exit value")

        } catch case e: os.SubprocessException => Error(s"The given symbol '$func' could not be found in the symbol table.")
    }
}
