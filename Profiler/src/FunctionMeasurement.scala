final case class FunctionMeasurement(start: Boolean, address: String, counter: Long)

object FunctionMeasurement {
  // Regex for the measurements
  private val lineRegex = raw"(\s*)(?<type>E|L):(?<address>\S+):(?<counter>\d+)".r

  /** Parses the given line and return the option of a measurement. */
  def createFromTrace(input: String): Option[FunctionMeasurement] =
    lineRegex.findFirstMatchIn(input) match {
      case None => None
      case Some(m) =>
        Some(FunctionMeasurement(
          start = m.group("type") == "E",
          address = m.group("address"),
          counter = m.group("counter").toLong
        ))
    }
}
