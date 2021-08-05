package masterthesis
package profiler

import java.util.concurrent.TimeUnit

import zio.{IO, UIO, ZIO, URIO, clock, FiberRef, Semaphore}
import zio.blocking.Blocking

/** Class to handle the profiling tasks at one place */
final case class ProfilingTask(
  name: String,
  file: String,
  version: String,
  makeTarget: Option[String] = None,
  cleanTarget: Option[String] = None,
  variant: Option[Int] = None,
  config: Config
) {

  def fileName = config.prepostfixed(file.split("/").last.split(".hex").head + variant.map(v => s"-V$v").getOrElse(""))
  def elfFile = s"${file.split(".hex").head}.elf"
  def dataFile = s"data/$fileName"
  def resultFile = s"results/$fileName"

  /** Perform the wanted actions. */
  def execute(
    config: Config,
    ref: FiberRef[String -> TaskState],
    sem: Semaphore
  ): ZIO[Blocking, String, Unit] = {
    // Run profiling and analyzis and report occurring errors
    val a = (build(config, ref) *> profile(fileName, config, ref, sem) *> analyze(fileName, config, ref))
      .ensuring(clean(config).ignore)
    a
  }

  /** Build the exeutable. */
  def build(config: Config, ref: FiberRef[String -> TaskState]) = makeTarget match {
    case Some(target) =>
      // Proceed only when a make target exists and profiling is wanted
      ZIO.when(config.doProfile) {
        for {
          _ <- ref.set(name -> TaskState.Building)
          r <- runForReturn(
            "make",
            target,
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
  def profile(
    fileName: String,
    config: Config,
    ref: FiberRef[String -> TaskState],
    sem: Semaphore
  ): ZIO[Blocking, String, Unit] =
    ZIO.when(config.doProfile)(
      sem.withPermit(ref.set(name -> TaskState.Profiling) *> ManualProfiling.profile(file, dataFile, config))
    )

  /** Build the exeutable. */
  def clean(config: Config) = cleanTarget match {
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
        } yield ()
      }
    case None =>
      ZIO.unit
  }

  /** Analyze the gathered data. */
  def analyze(
    fileName: String,
    config: Config,
    ref: FiberRef[String -> TaskState]
  ): ZIO[Blocking, String, Unit] =
    ZIO.when(config.doAnalysis)(
      ref.set(name -> TaskState.Analysing) *> Analysis(dataFile, elfFile, resultFile, config)
    )

  override def toString: String =
    s"[ProfilingTask $name]"
}
