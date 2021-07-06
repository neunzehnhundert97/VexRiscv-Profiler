final case class Config(
  doAnalysis: Boolean,
  doProfile: Boolean,
  manualInputs: List[String],
  visualize: Boolean,
  take: Int,
  debuggedFunction: Option[String],
  desiredCalls: Int,
  profilerMakeFlags: List[String],
  bootAt: String,
  exclude: List[String],
  hqc: Boolean,
  bike: Boolean,
  mceliece: Boolean
)
