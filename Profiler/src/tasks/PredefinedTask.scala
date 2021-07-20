package masterthesis
package profiler
package tasks

import scala.collection.mutable.ListBuffer

import better.files.File

import masterthesis.profiler.tasks.PredefinedTask

object PredefinedTask {
  lazy val registered = customTasks

  def getTasksByNames(names: List[String]): List[PredefinedTask] =
    registered.toList.filter(p => names.contains(p.name))

  /** Alternative constructor for only one element.
    * @param Name
    *   Displayed in messages and used for optional building.
    * @param hexFile
    *   Path to the hexfile to execute.
    * @param build
    *   Indicates that this task has a target in the profiler's makefile which should be executed.
    */
  def apply(name: String, hexFile: String, build: Boolean): PredefinedTask =
    PredefinedTask(name, (_, _) => hexFile, List(""), build)
}

/** Case class for a shortcut for often tested files.
  * @param name
  *   Name displayed in messages and used for optional building.
  * @param hexFile
  *   Function to map version to paths.
  * @param versions
  *   List of versions of this task.
  * @param build
  *   Indicates that this task has a target in the profiler's makefile which should be executed.
  */
final case class PredefinedTask(
  name: String,
  hexFile: (String, String) => String,
  versions: List[String],
  build: Boolean = false
) {

  /** Generates tasks for the profiler to execute. */
  def generateTasks(config: Config): List[ProfilingTask] = {
    // Handle takes and drops, or select
    val selectedVersions = (config.take, config.drop, config.select).match {
      case (None, None, Nil)             => versions
      case (Some(take), None, Nil)       => versions.take(take)
      case (None, Some(drop), Nil)       => versions.drop(drop)
      case (Some(take), Some(drop), Nil) => versions.drop(drop).take(take)
      case (None, None, numbers)         => numbers.map(i => versions(i))
        // Error is not catched, as this case should never happen
      case _ => ???
    }

    // Generate tasks
    if (config.variants.isEmpty)
      for (version <- selectedVersions)
        yield ProfilingTask(s"$name $version", version, hexFile(version, ""), Some(name), None, config)
    else
      for (version <- selectedVersions; variant <- config.variants)
        yield {
          ProfilingTask(
            s"$name $version $variant",
            hexFile(version, variant.toString),
            version,
            Some(name),
            Some(variant),
            config
          )
        }
  }

}
