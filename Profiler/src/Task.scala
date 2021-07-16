package masterthesis
package profiler

/** Class to handle the profiling tasks at one place */
final case class Task(name: String, file: String) {

  /** Perform the wanted actions. */
  def execute(implicit config: Config): Unit = {
    import config.*

    val fileName = prepostfixed(file.split("/").last.split(".hex").head)

    // Evaluate task
    val result = profile(s"data/$fileName", config)
      .flatMap(_ => Right(reportSuccess("Done profiling", name)))
      .flatMap(_ => analyze(s"data/$fileName", fileName, config))

    // Report result
    result match {
      case Error(msg) =>
        reportError(msg, name)
      case Success(_) =>
        reportSuccess("Done analyzing", name)
    }
  }

  def profile(dataFile: String, config: Config) = {
    import config.*
    if (doProfile) {
      ManualProfiling.profile(file, dataFile, config)
    } else Success
  }

  def analyze(dataFile: String, name: String, config: Config) = {
    import config.*
    if (doAnalysis)
      Analysis(
        dataFile,
        s"${file.split(".hex").head}.elf",
        s"results/$name",
        config
      )
    else
      Success
  }
}

// Define type aliases for either and its subtypes
// I'm slowly evolving this project in a more functional direction
type ErrorOrSuccess = Either[String, Unit]
type Success = Right[String, Unit]
def Success = Right(())
type Error = Left[String, Unit]
def Error = Left
