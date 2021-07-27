package masterthesis
package profiler
package tasks

/** Define your own tasks in this function. */
def customTasks: List[PredefinedTask] = Nil

/**
 * Replace Nil with a list of your own predefined tasks
 * Tasks come in two variants, single or multiple
 * 
 * To just define a task for one executable just do the following
 * PredefinedTask(
 *   "name", // This name will be recognized by the command args and will be used for outputs and building (optional)
 *   "path/to/your/file.hex", // .hex is required and a .elf with the same name also (for symbol table and disassembly),
 *   false // Indicate, if you added a build target in the makefile and you wish the profiler to call it for you
 * )
 * 
 * If you have multiple target which represent version of the same scheme (e.g AES-128, AES-192, AES-256) and you want to
 * profile them together, you can do the following
 * PredefinedTask(
 *   "name", // Same as above, but will trigger all version. See take, drop, or select for only work some of them
 *   (version, variant) => s"path/to/$version/and/$variant/of/your/file.elf", // Same as above, but a function
 *   List("Version1", "Version2"), // List of version, which will be directly inserted in the function above
 *   false // Indicate, if you added a build target in the makefile and you wish the profiler to call it for you
 * )
 * 
*/
