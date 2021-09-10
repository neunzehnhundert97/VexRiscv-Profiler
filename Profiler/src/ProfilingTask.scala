package masterthesis
package profiler

import java.util.concurrent.TimeUnit

import zio.{IO, UIO, ZIO, URIO, clock, Ref, Semaphore, Promise}
import zio.blocking.Blocking
import zio.clock.Clock

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
    ref: SharedRef,
    semBuild: Semaphore,
    semProfile: Semaphore,
    semAnalyse: Semaphore
  ): ZIO[Blocking & Clock, ProfilingTask -> String, Option[ProfilingTask -> AnalysisResult]] =
    // Run profiling and analyzis and report occurring errors
    (preflighting(variant, semBuild, ref) *> setState(ref, TaskState.ProfilingReady) *> recordAndCheckHash(ref)
      *> profile(fileName, ref, semProfile) *> setState(ref, TaskState.AnalysisReady)
      *> analyze(fileName, ref, semAnalyse) <* setState(ref, TaskState.Finished))
      .ensuring(clean.ignore)
      .mapError(e => this -> e)
      // When the task fails, set it to failed if its current state is not already a final state
      .tapError(_ => ZIO.whenM(ref.map(!_.individual(this).state.isFinal).get)(setState(ref, TaskState.Failed)))

  /** Performs an initial build request to let the program communicate dependencies on the used core. */
  def preflighting(variant: Option[String], sem: Semaphore, ref: SharedRef) = {
    val wantsPreflight = config.doPreflight && preflight
    for {
      _ <- ZIO.when(wantsPreflight)(setState(ref, TaskState.Preflight)).ignore
      result <- build(wantsPreflight, None, ref).either
      _ <- result match {
        // Handle preflight requirements
        case Left(e) if wantsPreflight =>
          val missingRequirements = e.split("\n").map(_.split("Missing: "))
            .collect { case Array(_, info) => info }.toList.distinct.sorted

          ref.update(s =>
            s.copy(individual =
              s.individual.updatedWithDefault(
                this,
                TaskInformation(depHash = missingRequirements.##.abs.toString),
                _.copy(depHash = missingRequirements.##.abs.toString)
              )
            )
          ) *> prepareCoreConstruction(sem, ref, missingRequirements)

        // If no preflighting is wanted, just propagate the error
        case Left(e) =>
          ZIO.fail(e)
        // A successful build requires no additonal actions
        case Right(_) if wantsPreflight =>
          ref.update(s =>
            s.copy(individual =
              s.individual.updatedWithDefault(
                this,
                TaskInformation(depHash = Nil.##.abs.toString),
                _.copy(depHash = Nil.##.abs.toString)
              )
            )
          ) *> prepareCoreConstruction(sem, ref, Nil)
        case Right(_) =>
          ZIO.unit
      }
    } yield ()
  }

  /** Build a custom core for every variant. */
  def prepareCoreConstruction(sem: Semaphore, ref: SharedRef, reqs: List[String]) = for {
    // Put dependencies into the mapping
    shared <- ref.updateAndGet(s =>
      s.copy(individual =
        s.individual.updatedWithDefault(this, TaskInformation(dependencies = reqs), _.copy(dependencies = reqs)))
    )

    // Wait until all variants commited there needs and then build all at once
    map = shared.individual
    _ <- shared.common.countDown.awaitForEffect(constructCores(sem, ref))

    // Build profiler and executable
    _ <- setState(ref, TaskState.Building)
    _ <- build(true, Some(reqs.##.abs.toString), ref)
      .mapError(e =>
        if (e.contains("#error Missing:")) s"After preflight there are still unmet dependencies:\n$e"
        else s"Executable could not be build because $e"
      )

    // Note core hash
    _ <- (ZIO.effect(File(s"cores/${reqs.##.abs.toString}/VexRiscv.v").sha256) >>= writeToFile(s"$dataFile-coreHash"))
      .mapError(e => s"Could not save core hash because of: $e")

  } yield ()

  /** Construct cores and simulations. */
  def constructCores(sem: Semaphore, ref: SharedRef) = for {
    shared <- ref.get
    map = shared.individual
    deps = map.values.map(_.dependencies).toList.distinct
    // Build cores
    _ <- runForReturn(
      "make",
      "allCores",
      s"CORE_INSTRUCTION=${deps.map(l => s"cores/${l.##.abs}+${l.mkString(":")}").mkString(" ")}"
    ).mapError(e => s"Could not build cores because: $e")
    // Build profiler
    _ <- ZIO.foreachPar_(deps)(d => sem.withPermit(Controller.buildProfilerWithDeps(d.##.abs.toString, config)))
    _ <- ZIO.when(config.doSynthesis)(Synthesis.requestSynthesis(deps.map(l => s"cores/${l.##.abs}")))
  } yield ()

  /** Build the exeutable. */
  def build(preflight: Boolean, depHash: Option[String], ref: SharedRef): ZIO[Blocking, String, Unit] =
    makeTarget match {
      case Some(target) =>
        // Proceed only when a make target exists and profiling is wanted
        ZIO.when(config.doProfile)(
          for {
            _ <- ZIO.when(!preflight)(setState(ref, TaskState.Building))
            r <- runForReturn(
              "make",
              target,
              s"INSTRUMENT_FUNCTIONS=${if (config.detailed) "Y" else "N"}",
              s"VERSION=$version",
              (if (preflight) "PREFLIGHT=Y" else ""),
              config.profilerMakeFlags.mkString(" "),
              variant.map(v => s"VARIANT=$v").getOrElse(""),
              depHash.map(v => s"DEP_HASH=$v").getOrElse("")
            ).mapError(e => s"The executable could not be build: $e")
            (code, out, error) = r
            _ <- ZIO.when(code != 0)(ZIO.fail(s"The executable could not be build: \nStdout:\n$out\nStderr:\n$error"))
          } yield ()
        )
      case None =>
        ZIO.unit
    }

  /** Computes the hash of the given file and verify that it is currently unique. */
  def recordAndCheckHash(ref: SharedRef): ZIO[Blocking, String, Unit] =
    ZIO.when(config.doProfile)(for {
      hash <- ZIO.effect(File(file).sha512).mapError(_ => ())
      shared <- ref.getAndUpdate(s =>
        s.copy(individual = s.individual.updatedWithDefault(this, TaskInformation(elfHash = hash), _.copy(elfHash = hash)))
      )
      lookAlikes = shared.individual.filter((_, d) => d._2 == hash).map(_._1)
      _ <- ZIO.when(lookAlikes.nonEmpty)(
        setState(ref, TaskState.Stopped)
          *> ZIO.fail(s"Other variants (${lookAlikes.mkString(", ")}) have the same hash, this one is therefore stopped")
      )
    } yield ()).catchAll {
      case s: String => ZIO.fail(s)
      case _         => ZIO.unit
    }

  /** Run the verilog simulation to create log data. */
  def profile(fileName: String, ref: SharedRef, sem: Semaphore): ZIO[Blocking, String, Unit] =
    ZIO.when(config.doProfile)(
      if (preflight && config.doPreflight) for {
        shared <- ref.get
        hash = shared.individual(this).depHash
        _ <- sem.withPermit(setState(ref, TaskState.Profiling) *> Controller.profile(
          file,
          dataFile,
          config,
          Some(hash)
        ))
      } yield ()
      else
        sem.withPermit(setState(ref, TaskState.Profiling) *> Controller.profile(
          file,
          dataFile,
          config,
          variant
        ))
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

  /** Analyze the gathered data. */
  def analyze(
    fileName: String,
    ref: SharedRef,
    sem: Semaphore
  ): ZIO[Blocking, String, Option[ProfilingTask -> AnalysisResult]] =
    if (config.doAnalysis)
      sem.withPermit(setState(ref, TaskState.Analysing) *> Analysis(dataFile, elfFile, resultFile, config)
        .map(a => Some(this -> a)))
    else
      ZIO.none

  def setState(ref: SharedRef, state: TaskState): UIO[Unit] =
    ref.update(s =>
      s.copy(individual = s.individual.updatedWithDefault(this, TaskInformation(state = state), _.copy(state = state)))
    )

  override def toString: String =
    s"[ProfilingTask $name]"
}

type SharedRef = Ref[SharedState]

/** Runtime information shared between all states and the supervisor. */
final case class SharedState(individual: Map[ProfilingTask, TaskInformation], common: TaskCommon = TaskCommon())

/** Information for / over all tasks. */
final case class TaskCommon(countDown: CountDownLatch[String] = null)

/** Information for a individual task. */
final case class TaskInformation(
  state: TaskState = TaskState.Initial,
  elfHash: String = "",
  depHash: String = "",
  dependencies: List[String] = Nil
)
