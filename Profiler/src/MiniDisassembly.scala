enum EncodingType() {
  case I extends EncodingType()
  case R extends EncodingType()
  case S extends EncodingType()
  case M extends EncodingType()
}

final case class InstructionEncoding(mnemonic: String, pattern: String, encodingType: EncodingType) {

  /** Tries to decode the given hexstring. */
  def decode(hex: String): Option[String] = {
    val asLong = hex.substring(2).toLong(16)
    val binary = asLong.toBinaryString
    val bin = "0" * (32 - binary.length) + binary

    // Check pattern and decode if it matches
    if (bin.zip(pattern).forall((real, pat) => pat == '-' || real == pat))
      Some(s"$mnemonic ${encodingType match {
        case EncodingType.I =>
          s"x${rd(bin)}, x${rs1(bin)}, ${readNumber(bin, 31, 20)}"
        case EncodingType.R =>
          s"x${rd(bin)}, x${rs1(bin)}, x${rs2(bin)}"
        case EncodingType.S =>
          s"x${rs2(bin)}, ${rd(bin) | readNumber(bin, 31, 25) << 5}(${rs1(bin)})"
        case EncodingType.M =>
          s"x${rd(bin)}, x${rs1(bin)}, x${rs2(bin)}, ${immM(bin)}"
      }}")
    else None
  }

  def rd(bin: String): Long =
    readNumber(bin, 11, 7)

  def rs1(bin: String): Long =
    readNumber(bin, 19, 15)

  def rs2(bin: String): Long =
    readNumber(bin, 24, 20)

  def immI(bin: String): Long =
    readNumber(bin, 31, 20)

  def immM(bin: String): Long =
    readNumber(bin, 31, 25)

  /** Reads a number from the binary string with the given X downto Y notation. */
  def readNumber(bin: String, upper: Int, lower: Int): Long =
    bin.drop(32 - upper - 1).take(upper - lower + 1).toLong(2)
}
