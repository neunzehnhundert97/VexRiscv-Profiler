package masterthesis
package profiler

import tasks.PredefinedTask

final case class Config(
  doAnalysis: Boolean,
  doProfile: Boolean,
  manualInputs: List[String],
  visualize: Boolean,
  take: Option[Int],
  drop: Option[Int],
  debuggedFunction: Option[String],
  desiredCalls: Int,
  profilerMakeFlags: List[String],
  bootAt: String,
  exclude: List[String],
  predefinedTasks: List[PredefinedTask]
)
