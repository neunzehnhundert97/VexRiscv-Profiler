package masterthesis
package profiler

def reportStatus(msg: String, reporter: String = "Profiler"): Unit =
  println(Console.BLUE + reporter + Console.RESET + ": " + msg)

def reportError(msg: String, reporter: String = "Profiler"): Unit =
  println(Console.RED + reporter + Console.RESET + ": " + msg)

def reportSuccess(msg: String, reporter: String = "Profiler"): Unit =
  println(Console.GREEN + reporter + Console.RESET + ": " + msg)

type ->[+A, +B] = Tuple2[A, B]
