package masterthesis
package profiler

import java.util.concurrent.TimeUnit

import zio.{IO, UIO, ZIO, URIO, clock, Ref, Semaphore}
import zio.blocking.Blocking

/** Class to handle the profiling tasks at one place */
final case class ProfilingTask(
  group: String,
  name: String,
  file: String,
  version: String,
  makeTarget: Option[String] = None,
  cleanTarget: Option[String] = None,
  variant: Option[String] = None,
  config: Config
) {
  def fileName =
    s"$group/${config.prepostfixed(file.split("/").last.split(".hex").head + variant.map(v => s"-V$v").getOrElse(""))}"
  def elfFile = s"${file.split(".hex").head}.elf"
  def dataFile = s"data/$fileName"
  def resultFile = s"results/$fileName"

  /** Perform the wanted actions. */
  def execute(
    config: Config,
    ref: Ref[Map[ProfilingTask, TaskState]],
    semProfile: Semaphore,
    semAnalyse: Semaphore
  ): ZIO[Blocking, ProfilingTask -> String, Option[ProfilingTask -> AnalysisResult]] =
    // Run profiling and analyzis and report occurring errors
    (build(ref) *> ref.update(_.updated(this, TaskState.ProfilingReady))
      *> profile(fileName, ref, semProfile) *> ref.update(_.updated(this, TaskState.AnalysisReady))
      *> analyze(fileName, ref, semAnalyse) <* ref.update(_.updated(this, TaskState.Finished)))
      .ensuring(clean.ignore)
      .mapError(e => this -> e)
      .tapError(_ => ref.update(_.updated(this, TaskState.Failed)))

  /** Build the exeutable. */
  def build(ref: Ref[Map[ProfilingTask, TaskState]]): ZIO[Blocking, String, Unit] = makeTarget match {
    case Some(target) =>
      // Proceed only when a make target exists and profiling is wanted
      ZIO.when(config.doProfile) {
        for {
          _ <- ref.update(_.updated(this, TaskState.Building))
          r <- runForReturn(
            "make",
            target,
            s"INSTRUMENT_FUNCTIONS=${if (config.detailed) "Y" else "N"}",
            s"VERSION=$version",
            config.profilerMakeFlags.mkString(" "),
            variant.map(v => s"VARIANT=$v").getOrElse("")
          ).mapError(e => s"The profiler could not be build: $e")
          (code, _, error) = r
          _ <- ZIO.when(code != 0)(ZIO.fail(s"The profiler could not be build: $error"))
        } yield ()
      }
    case None =>
      ZIO.unit
  }

  /** Run the verilog simulation to create log data. */
  def profile(fileName: String, ref: Ref[Map[ProfilingTask, TaskState]], sem: Semaphore): ZIO[Blocking, String, Unit] =
    ZIO.when(config.doProfile)(
      sem.withPermit(ref.update(_.updated(this, TaskState.Profiling)) *> Controller.profile(file, dataFile, config))
    )

  /** Build the exeutable. */
  def clean: ZIO[Blocking, String, Unit] = cleanTarget match {
    case Some(target) =>
      // Proceed only when a make target exists, profiling was wanted, and cleaning is enbaled
      ZIO.when(config.doProfile) {
        for {
          r <- runForReturn(
            "make",
            target,
            s"VERSION=$version",
            config.profilerMakeFlags.mkString(" "),
            variant.map(v => s"VARIANT=$v").getOrElse("")
          ).mapError(e => s"The profiler could not be build: $e")
          _ <- ZIO.when(r._1 != 0)(ZIO.fail("Profiler returned non zero exit code."))
        } yield ()
      }
    case None =>
      ZIO.unit
  }

  /** Analyze the gathered data. */
  def analyze(
    fileName: String,
    ref: Ref[Map[ProfilingTask, TaskState]],
    sem: Semaphore
  ): ZIO[Blocking, String, Option[ProfilingTask -> AnalysisResult]] =
    if (config.doAnalysis)
      sem.withPermit(ref.update(_.updated(this, TaskState.Analysing)) *> Analysis(dataFile, elfFile, resultFile, config)
        .map(a => Some(this -> a)))
    else
      ZIO.none

  override def toString: String =
    s"[ProfilingTask $name]"
}
