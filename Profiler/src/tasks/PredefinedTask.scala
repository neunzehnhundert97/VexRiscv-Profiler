package masterthesis
package profiler
package tasks

import scala.collection.mutable.ListBuffer
import masterthesis.profiler.tasks.PredefinedTask

object PredefinedTask {
  lazy val registered = customTasks

  def getTasksByNames(names: List[String]): List[PredefinedTask] =
    registered.toList.filter(p => names.contains(p.name))
}

/** Case class for a shortcut for often tested files. */
final case class PredefinedTask(name: String, hexFile: String => String, versions: List[String]) {

  /** Alternative constructor for only one element. */
  def this(name: String, hexFile: String) =
    this(name, _ => hexFile, List(""))

  /** Generates tasks for the profiler to execute. */
  def generateTasks: List[Task] =
    for (version <- versions)
      yield Task(hexFile(version))
}
