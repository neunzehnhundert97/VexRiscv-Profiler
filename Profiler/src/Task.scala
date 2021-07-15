package masterthesis
package profiler

/** Class to handle the profiling tasks at one place */
final case class Task(file: String) {

  /** Perform the wanted actions. */
  def execute(implicit config: Config): Unit = {
    import config.*

    val name = file.split("/").last.split(".hex").head
    val dataFile = s"data/$name"

    // Evaluate task
    val result = profile(dataFile, config)
      .flatMap(_ => Right(reportSuccess("Done profiling", name)))
      .flatMap(_ => analyze(dataFile, name, config))

    // Report result
    result match {
      case Error(msg) =>
        reportError(msg, file)
      case Success(_) =>
        reportSuccess("Done analyzing", name)
    }
  }

  def profile(dataFile: String, config: Config) = {
    import config.*
    if (doProfile) {
      ManualProfiling.profile(file, dataFile, debuggedFunction, desiredCalls, bootAt)
    } else Right(())
  }

  def analyze(dataFile: String, name: String, config: Config) = {
    import config.*
    if (doAnalysis)
      ManualProfiling.analyse(
        dataFile,
        s"${file.split(".hex").head}.elf",
        s"results/$name",
        config
      )
    else
      Right(())
  }
}

// Define type aliases for either and its subtypes
// I'm slowly evolving this project in a more functional direction
type ErrorOrSuccess = Either[String, Unit]
type Success = Right[String, Unit]
type Error = Left[String, Unit]
