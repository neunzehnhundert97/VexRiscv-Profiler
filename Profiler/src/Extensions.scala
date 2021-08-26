package masterthesis
package profiler

import zio.ZIO
import scala.util.NotGiven
import scala.annotation.implicitNotFound

extension (s: String) {
  def toLong(radix: Int): Long =
    java.lang.Long.parseLong(s, radix)

  /** Pads a string to be centered. */
  def centered(length: Int, filler: String = " "): String =
    if (length < s.length)
      s.substring(0, length)
    else if ((length - s.length) % 2 == 0)
      filler * ((length - s.length) / 2) + s + filler * ((length - s.length) / 2)
    else
      filler * ((length - s.length) / 2) + s + filler * (((length - s.length) / 2) + 1)

  /** Alias for centered. */
  def ^(length: Int): String =
    centered(length)
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

extension (i: Int) {
  def downto(start: Int): Range.Inclusive = Range.inclusive(i, start, -1)
}

extension [R, E, A](zio: ZIO[R, E, A]) {

  /** Creates a effect which ignores the result of the previous one. */
  def discard(using NotGiven[A =:= Unit]): ZIO[R, E, Unit] =
    zio.map(_ => ())

  def >>[R2, E2, A2, D](next: ZIO[R2, E2, A2]) =
    zio.flatMap(_ => next)
}
