package masterthesis
package profiler

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
          s"${registerMapping(rd(bin))}, ${registerMapping(rs1(bin))}, ${readNumber(bin, 31, 20)}"
        case EncodingType.R =>
          s"${registerMapping(rd(bin))}, ${registerMapping(rs1(bin))}, ${registerMapping(rs2(bin))}"
        case EncodingType.S =>
          s"${registerMapping(rs2(bin))}, ${rd(bin) | readNumber(bin, 31, 25) << 5}(${registerMapping(rs1(bin))})"
        case EncodingType.M =>
          s"${registerMapping(rd(bin))}, ${registerMapping(rs1(bin))}, ${registerMapping(rs2(bin))}, ${immM(bin)}"
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

val registerMapping = Map(
  0 -> "zero",
  1 -> "ra",
  2 -> "sp",
  3 -> "gp",
  4 -> "tp",
  5 -> "t0",
  6 -> "t1",
  7 -> "t2",
  8 -> "s0",
  9 -> "s1",
  10 -> "a0",
  11 -> "a1",
  12 -> "a2",
  13 -> "a3",
  14 -> "a4",
  15 -> "a5",
  16 -> "a6",
  17 -> "a7",
  18 -> "s2",
  19 -> "s3",
  20 -> "s4",
  21 -> "s5",
  22 -> "s6",
  23 -> "s7",
  24 -> "s8",
  25 -> "s9",
  26 -> "s10",
  27 -> "s11",
  28 -> "t3",
  29 -> "t4",
  30 -> "t5",
  31 -> "t6"
).map((key, value) => key.toLong -> value)
