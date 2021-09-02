package masterthesis
package profiler

import java.util.concurrent.TimeUnit

import zio.{IO, UIO, ZIO, URIO, clock, Ref, Semaphore}
import zio.blocking.Blocking

import better.files.File

/** Class to handle the profiling tasks at one place */
final case class ProfilingTask(
  group: String,
  name: String,
  file: String,
  version: String,
  makeTarget: Option[String] = None,
  cleanTarget: Option[String] = None,
  variant: Option[String] = None,
  preflight: Boolean = false,
  config: Config
) {
  def fileName = s"${file.split("/").last.split(".hex").head + variant.map(v => s"-V$v").getOrElse("")}"
  def elfFile = s"${file.split(".hex").head}.elf"
  def dataFile = s"data/$group/${config.postfixed(fileName)}"
  def resultFile = s"results/$group/${config.prefixed(fileName)}"

  /** Perform the wanted actions. */
  def execute(
    config: Config,
    ref: Ref[Map[ProfilingTask, TaskState -> String]],
    semProfile: Semaphore,
    semAnalyse: Semaphore
  ): ZIO[Blocking, ProfilingTask -> String, Option[ProfilingTask -> AnalysisResult]] =
    // Run profiling and analyzis and report occurring errors
    (preflighting(variant, semProfile, ref) *> setState(ref, TaskState.ProfilingReady) *> recordAndCheckHash(ref)
      *> profile(fileName, ref, semProfile) *> setState(ref, TaskState.AnalysisReady)
      *> analyze(fileName, ref, semAnalyse) <* setState(ref, TaskState.Finished))
      .ensuring(clean.ignore *> cleanPreflight.ignore)
      .mapError(e => this -> e)
      .tapError(_ => setState(ref, TaskState.Failed))

  /** Performs an initial build request to let the program communicate dependencies on the used core. */
  def preflighting(variant: Option[String], sem: Semaphore, ref: Ref[Map[ProfilingTask, TaskState -> String]]) =
    for {
      _ <- ZIO.when(preflight && config.doPreflight)(setState(ref, TaskState.Preflight) *> cleanPreflight)
      result <- build(sem, ref).either
      _ <- result match {
        // Handle preflight requirements
        case Left(e) if preflight && config.doPreflight =>
          val missingRequirements = e.split("\n").map(_.split("Missing: "))
            .collect { case Array(_, info) => info }.toList.distinct

          Controller.buildProfilerWithDeps(missingRequirements, variant.getOrElse(""), config) *> build(sem, ref)
        // If no preflighting is wanted, just propagate the error
        case Left(e) =>
          ZIO.fail(e)
        // A successful build requires no additonal actions
        case Right(_) =>
          ZIO.unit
      }
    } yield ()

  /** Build the exeutable. */
  def build(sem: Semaphore, ref: Ref[Map[ProfilingTask, TaskState -> String]]): ZIO[Blocking, String, Unit] = makeTarget match {
    case Some(target) =>
      // Proceed only when a make target exists and profiling is wanted
      ZIO.when(config.doProfile) {
        for {
          _ <- setState(ref, TaskState.Building)
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

  /** Computes the hash of the given file and verify that it is currently unique. */
  def recordAndCheckHash(ref: Ref[Map[ProfilingTask, TaskState -> String]]): ZIO[Blocking, String, Unit] =
    ZIO.when(config.doProfile)(for {
      hash <- ZIO.effect(File(file).sha512).mapError(_ => ())
      mapping <- ref.getAndUpdate(_.updatedWith(this) {
        case None         => Some(TaskState.Initial -> hash)
        case Some(s -> h) => Some(s -> hash)
      })
      lookAlikes = mapping.filter((_, d) => d._2 == hash).map(_._1)
      _ <- ZIO.when(lookAlikes.nonEmpty)(
        ZIO.fail(s"Other variants (${lookAlikes.mkString(", ")}) have the same hash, this one is therefore stopped")
      )
    } yield ()).catchAll {
      case s: String => ZIO.fail(s)
      case _         => ZIO.unit
    }

  /** Run the verilog simulation to create log data. */
  def profile(fileName: String, ref: Ref[Map[ProfilingTask, TaskState -> String]], sem: Semaphore): ZIO[Blocking, String, Unit] =
    ZIO.when(config.doProfile)(
      sem.withPermit(setState(ref, TaskState.Profiling) *> Controller.profile(file, dataFile, config, variant))
    )

  /** Clean the exeutable. */
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
          ).mapError(e => s"The executable could not be cleaned: $e")
          _ <- ZIO.when(r._1 != 0)(ZIO.fail("Profiler returned non zero exit code."))
        } yield ()
      }
    case None =>
      ZIO.unit
  }

  def cleanPreflight: ZIO[Blocking, String, Unit] = cleanTarget match {
    case Some(_) =>
      // Proceed only when a make target exists, profiling was wanted, and cleaning is enbaled
      ZIO.when(config.doProfile) {
        for {
          r <- runForReturn(
            "make",
            "clean",
            config.profilerMakeFlags.mkString(" "),
            variant.map(v => s"VARIANT=$v").getOrElse("")
          ).mapError(e => s"The profiler could not be cleaned: $e")
          _ <- ZIO.when(r._1 != 0)(ZIO.fail("Profiler returned non zero exit code."))
        } yield ()
      }
    case None =>
      ZIO.unit
  }

  /** Analyze the gathered data. */
  def analyze(
    fileName: String,
    ref: Ref[Map[ProfilingTask, TaskState -> String]],
    sem: Semaphore
  ): ZIO[Blocking, String, Option[ProfilingTask -> AnalysisResult]] =
    if (config.doAnalysis)
      sem.withPermit(setState(ref, TaskState.Analysing) *> Analysis(dataFile, elfFile, resultFile, config)
        .map(a => Some(this -> a)))
    else
      ZIO.none

  def setState(ref: Ref[Map[ProfilingTask, TaskState -> String]], state: TaskState): UIO[Unit] =
    ref.update(_.updatedWith(this) {
      case None         => Some(state -> "")
      case Some(s -> h) => Some(state -> h)
    })

  override def toString: String =
    s"[ProfilingTask $name]"
}
