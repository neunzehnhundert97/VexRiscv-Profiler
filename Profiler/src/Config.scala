package masterthesis
package profiler

import zio.IO

import tasks.PredefinedTask

final case class Config(
  doProfile: Boolean,
  doAnalysis: Boolean,
  doBenchmark: Boolean,
  manualInputs: List[String],
  visualize: Boolean,
  take: Option[Int],
  drop: Option[Int],
  select: List[Int],
  variants: List[String],
  debuggedFunction: Option[String],
  desiredCalls: Int,
  profilerMakeFlags: List[String],
  bootAt: String,
  exclude: List[String],
  include: List[String],
  threshold: Double,
  zoom: Option[String],
  predefinedTasks: List[PredefinedTask],
  postfix: Option[String],
  prefix: Option[String],
  profileThreads: Option[Int],
  analysisThreads: Option[Int],
  detailed: Boolean,
  imageFormat: String
) {

  /** Verifies that this config was filled in a useful manner. */
  def reportConfig: IO[String, String] = {
    if (!doProfile && !doAnalysis && !doBenchmark)
      IO.fail("Nothing to do, you should either profile or analyse.")
    else if (doBenchmark && variants.length <= 1)
      IO.fail("A benchmark can only be usefull with multiple variants to compare.")
    else if (doBenchmark && manualInputs.nonEmpty)
      IO.fail("A benchmark is currently only supported for predefined tasks.")
    else if (doBenchmark && predefinedTasks.length != 1)
      IO.fail("A benchmark may currently only be conducted for one target at a time.")
    else if (doBenchmark && !doAnalysis)
      IO.fail("A benchmark may currently only be conducted together with an analysis.")
    else if ((take.nonEmpty || drop.nonEmpty) && select.nonEmpty)
      IO.fail("Select can only be used without drop and take.")
    else {
      val actions =
        (if (doProfile) List("profiling") else Nil) ++
          (if (doAnalysis) List("analysis") else Nil) ++
          (if (doBenchmark) List("benchmark") else Nil)

      IO.succeed(s"Conducting ${actions.mkString(", ")} on ${predefinedTasks.length + manualInputs.length} targets"
        + ((prefix, postfix) match {
          case None -> None            => ""
          case Some(pre) -> None       => s"\nUsing prefix '$pre'"
          case None -> Some(post)      => s"\nUsing pstfix '$post'"
          case Some(pre) -> Some(post) => s"\nUsing prefix '$pre' and postfix '$post'"
        })
        + (if (doProfile) if (detailed) "\nProfiling in detailed mode" else "\nProfiling in normal mode" else ""))
    }
  }

  def postfixed(name: String): String =
    (debuggedFunction, postfix) match {
      case (None, None)             => name
      case (Some(func), None)       => s"$name-$func"
      case (None, Some(post))       => s"$name-$post"
      case (Some(func), Some(post)) => s"$name-$func-$post"
    }

  def prefixed(name: String): String =
    prefix match {
      case Some(pre) => s"$pre/$name"
      case None      => name
    }
}

object Config {

  /** Reads arguments of the form "name=" and returns them as an option, optionally converted. */
  def extractArgumentOption[A](args: Seq[String], name: String, conversion: String => A = identity): Option[A] =
    args.find(_.startsWith(s"$name=")).map(_.substring(name.length + 1)).map(conversion)

  /** Create configuration from command line arguments. The order does not matter. */
  def apply(args: Seq[String]): Config = {
    // Boolean arguments
    val doAnalysis = args.contains("analyse") || args.contains("analyze") || args.contains("process")
    val doProfile = args.contains("profile") || args.contains("process")
    val visualize = args.contains("graph") || args.contains("visualize") || args.contains("process")

    // Reduce the number of versions in predefined tasks to profile
    val take = extractArgumentOption(args, "take", _.toInt)
    val drop = extractArgumentOption(args, "drop", _.toInt)
    val select = extractArgumentOption(args, "select", _.split(",").map(_.toInt).toList).getOrElse(Nil)

    // Variants for benchmarks
    val variants = extractArgumentOption(args, "variants", _.split(",").toList).getOrElse(Nil)
    val doBenchmark = args.contains("benchmark") || (variants.length > 1 && args.contains("process"))

    // Arguments for the instruction analysis
    val debuggedFunction = extractArgumentOption(args, "func")
    val desiredCalls = extractArgumentOption(args, "calls", _.toInt).getOrElse(1)

    // Direct input of files to debug
    val manualInputs = extractArgumentOption(args, "input", _.split(',').toList).getOrElse(Nil)

    // Flags for the profiler's makefile
    val profilerMakeFlags = extractArgumentOption(args, "profilerFlags", _.split(',').toList).getOrElse(Nil)

    // Address to start execution at
    val bootAt = extractArgumentOption(args, "bootAt").getOrElse("80000000")

    // A list of function names to exclude
    val exclude = extractArgumentOption(args, "exclude", _.split(",").map(_.toLowerCase).toList).getOrElse(Nil)
    val include = extractArgumentOption(args, "include", _.split(",").map(_.toLowerCase).toList).getOrElse(Nil)
    val threshold = extractArgumentOption(args, "threshold", _.toDoubleOption).flatten.getOrElse(0.05)
    val zoom = extractArgumentOption(args, "zoom")

    // Find calls to predefined tasks
    val customTasks = PredefinedTask.getTasksByNames(args.toList)

    // Post/Prefix for outputs in order to prevent overwriting
    val postfix = extractArgumentOption(args, "postfix")
    val prefix = extractArgumentOption(args, "prefix")

    // Thread limits
    val profileThreads = extractArgumentOption(args, "profileThreads", _.toInt)
    val analysisThreads = extractArgumentOption(args, "analysisThreads", _.toInt)

    // Alternative profiling mode
    val detailed = args.contains("detailed")

    val imageFormat = extractArgumentOption(args, "imageFormat").getOrElse("png")

    // Put all in one object to ease passing around
    Config(
      doProfile,
      doAnalysis,
      doBenchmark,
      manualInputs,
      visualize,
      take,
      drop,
      select,
      variants,
      debuggedFunction,
      desiredCalls,
      profilerMakeFlags,
      bootAt,
      exclude,
      include,
      threshold,
      zoom,
      customTasks,
      postfix,
      prefix,
      profileThreads,
      analysisThreads,
      detailed,
      imageFormat
    )
  }
}
