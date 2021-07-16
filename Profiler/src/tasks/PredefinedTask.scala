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
    PredefinedTask(name, _ => hexFile, List(""), build)
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
final case class PredefinedTask(name: String, hexFile: String => String, versions: List[String], build: Boolean = false) {

  /** Generates tasks for the profiler to execute. */
  def generateTasks: List[Either[String, Task]] =
    for (version <- versions)
      yield {
        val hex = hexFile(version)
        val file = File(hex)
        // Perform check on files
        if (file.exists)
          if ((file.parent / (file.nameWithoutExtension + ".elf")).exists)
            Right(Task(s"$name $version", hex))
          else
            Error(s"There is no elf file.")
        else
          Error(s"Given file $hex does not exist.")
      }
}
