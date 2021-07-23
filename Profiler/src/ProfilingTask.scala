package masterthesis
package profiler

import java.util.concurrent.TimeUnit

import zio.{IO, UIO, ZIO, URIO, clock, FiberRef}
import zio.console.Console
import clock.Clock
import zio.blocking.Blocking

/** Class to handle the profiling tasks at one place */
final case class ProfilingTask(
  name: String,
  file: String,
  version: String,
  makeTarget: Option[String] = None,
  variant: Option[Int] = None,
  config: Config
) {

  def fileName = config.prepostfixed(file.split("/").last.split(".hex").head + variant.map(v => s"-V$v").getOrElse(""))
  def elfFile = s"${file.split(".hex").head}.elf"
  def dataFile = s"data/$fileName"
  def resultFile = s"results/$fileName"

  /** Perform the wanted actions. */
  def execute(config: Config, ref: FiberRef[String -> TaskState]): URIO[Console & Blocking & Clock, Unit] = {
    // Run profiling and analyzis and report occurring errors
    (build(config, ref) *> profile(fileName, config, ref) *> analyze(fileName, config, ref) *> ref.set(
      name -> TaskState.Finished
    ))
      .catchAll(msg => reportError(msg, name))
  }

  /** Build the exeutable. */
  def build(config: Config, ref: FiberRef[String -> TaskState]) = makeTarget match {
    case Some(target) =>
      // Proceed only when a make target exists and profiling is wanted
      ZIO.when(config.doProfile) {
        for {
          _ <- ref.set(name -> TaskState.Building)
          start <- clock.currentTime(TimeUnit.SECONDS)
          r <- runForReturn(
            "make",
            target,
            s"VERSION=$version",
            config.profilerMakeFlags,
            // Usage of list to let a empty result vanish
            // Empyt strings cause make to fail
            variant.map(v => List(s"VARIANT=$v")).getOrElse(Nil)
          )
          ret <- ZIO.when(r.exitCode != 0)(ZIO.fail(s"The profiler could not be build: ${r.out.text}"))
          end <- clock.currentTime(TimeUnit.SECONDS)
          // _ <- reportSuccess(s"Done building in ${end - start} seconds", name)
        } yield ()
      }
    case None =>
      ZIO.unit
  }

  /** Run the verilog simulation to create log data. */
  def profile(
    fileName: String,
    config: Config,
    ref: FiberRef[String -> TaskState]
  ): ZIO[Console & Blocking & Clock, String, Unit] =
    ZIO.when(config.doProfile)(
      for {
        _ <- ref.set(name -> TaskState.Profiling)
        start <- clock.currentTime(TimeUnit.SECONDS)
        _ <- ManualProfiling.profile(file, dataFile, config)
        end <- clock.currentTime(TimeUnit.SECONDS)
        // _ <- reportSuccess(s"Done profiling in ${end - start} seconds", name)
      } yield ()
    )

  /** Analyze the gathered data. */
  def analyze(
    fileName: String,
    config: Config,
    ref: FiberRef[String -> TaskState]
  ): ZIO[Console & Clock & Blocking, String, Unit] =
    ZIO.when(config.doAnalysis)(
      for {
        _ <- ref.set(name -> TaskState.Analysing)
        start <- clock.currentTime(TimeUnit.SECONDS)
        _ <- Analysis(dataFile, elfFile, resultFile, config)
        end <- clock.currentTime(TimeUnit.SECONDS)
        // _ <- reportSuccess(s"Done analyzing in ${end - start} seconds", name)
      } yield ()
    )

  override def toString: String =
    s"[ProfilingTask $name]"
}
