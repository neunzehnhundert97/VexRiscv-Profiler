def reportStatus(msg: String, reporter: String = "Profiler"): Unit =
  println(Console.BLUE + reporter + Console.RESET + ": " + msg)

def reportError(msg: String, reporter: String = "Profiler"): Unit =
  println(Console.RED + reporter + Console.RESET + ": " + msg)

def reportSuccess(msg: String, reporter: String = "Profiler"): Unit =
  println(Console.GREEN + reporter + Console.RESET + ": " + msg)

extension (s: String) {
  def toLong(radix: Int): Long =
    java.lang.Long.parseLong(s, radix)
}

extension [N: Numeric](list: List[N]) {
  def median: Double = list.length match {
    case 0 => 0
    case 1 => summon[Numeric[N]].toDouble(list.head)
    case l if l % 2 == 0 =>
      val ls = list.sorted
      val num = summon[Numeric[N]]
      num.toDouble((num.plus(ls(l / 2), ls(l / 2 - 1)))) / 2.0
    case l if l % 2 != 0 => summon[Numeric[N]].toDouble(list(l / 2 + 1))
  }
}

type ->[+A, +B] = Tuple2[A, B]
