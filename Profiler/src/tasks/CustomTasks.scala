package masterthesis
package profiler
package tasks

/** Define your own tasks in this function. */
def customTasks: List[PredefinedTask] = List(
  PredefinedTask(
    "hqc",
    (version, variant) => s"../hqc/hqc-$version/bin${if (variant.isEmpty) "" else s"-$variant"}/hqc-$version.hex",
    List("128", "192", "256")
  ),
  PredefinedTask(
    "mceliece",
    (version, variant) => s"../McEliece/mceliece$version/bin${if (variant.isEmpty) "" else s"-$variant"}/mceliece$version.hex",
    List("348864", "460896", "348864f", "460896f")
  ),
  PredefinedTask(
    "bike",
    (version, variant) => s"../BIKE-Additional/BIKE-$version/bin${if (variant.isEmpty) "" else s"-$variant"}/bike-$version.hex",
    List("1", "3")
  ),
  PredefinedTask(
    "test",
    "/home/lab/F/EigeneDateien/Studium/5_SS_21/MA/Code/TrinityCore/RegressionTests/ExtendedRegTest/build/ExtendedRegTest.hex",
    true
  )
)
