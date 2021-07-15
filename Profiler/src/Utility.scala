package masterthesis
package profiler

/** Prints a blue status message to stdout. */
def reportStatus(msg: String, reporter: String = "Profiler"): Unit =
  println(Console.BLUE + reporter + Console.RESET + ": " + msg)

/** Prints a red error message to stdout. */
def reportError(msg: String, reporter: String = "Profiler"): Unit =
  println(Console.RED + reporter + Console.RESET + ": " + msg)

/** Prints a green success message to stdout. */
def reportSuccess(msg: String, reporter: String = "Profiler"): Unit =
  println(Console.GREEN + reporter + Console.RESET + ": " + msg)

type ->[+A, +B] = Tuple2[A, B]
