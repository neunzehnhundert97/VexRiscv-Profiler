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
  variants: List[Int],
  debuggedFunction: Option[String],
  desiredCalls: Int,
  profilerMakeFlags: List[String],
  bootAt: String,
  exclude: List[String],
  predefinedTasks: List[PredefinedTask],
  postfix: Option[String],
  prefix: Option[String]
) {

  /** Verifies that this config was filled in a useful manner. */
  def reportUselessConfig: IO[String, Unit] = {
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
    else
      IO.unit
  }

  /** Add pre- and postfixes to the given string. */
  def prepostfixed(name: String): String = {
    val postFixed = (debuggedFunction, postfix) match {
      case (None, None)             => name
      case (Some(func), None)       => s"$name-$func"
      case (None, Some(post))       => s"$name-$post"
      case (Some(func), Some(post)) => s"$name-$func-$post"
    }

    prefix match {
      case None      => postFixed
      case Some(pre) => s"$pre/$postFixed"
    }
  }
}

object Config {

  /** Reads arguments of the form "name=" and returns them as an option, optionally converted. */
  def extractArgumentOption[A](args: Seq[String], name: String, conversion: String => A = identity): Option[A] =
    args.find(_.startsWith(s"$name=")).map(_.substring(name.length + 1)).map(conversion)

  /** Create configuration from command line arguments. The order does not matter. */
  def apply(args: Seq[String]): Config = {
    // Boolean arguments
    val doAnalysis = args.contains("analyse") || args.contains("analyze")
    val doProfile = args.contains("profile")
    val doBenchmark = args.contains("benchmark")
    val visualize = args.contains("graph") || args.contains("visualize")

    // Reduce the number of versions in predefined tasks to profile
    val take = extractArgumentOption(args, "take", _.toInt)
    val drop = extractArgumentOption(args, "drop", _.toInt)
    val select = extractArgumentOption(args, "select", _.split(",").map(_.toInt).toList).getOrElse(Nil)

    // Variants for benchmarks
    val variants = extractArgumentOption(args, "variants", _.split(",").toList.map(_.toInt)).getOrElse(Nil)

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

    // Find calls to predefined tasks
    val customTasks = PredefinedTask.getTasksByNames(args.toList)

    // Post/Prefix for outputs in order to prevent overwriting
    val postfix = extractArgumentOption(args, "postfix")
    val prefix = extractArgumentOption(args, "prefix")

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
      customTasks,
      postfix,
      prefix
    )
  }
}
