package masterthesis
package profiler
package tasks

/** Define your own tasks in this function. */
def customTasks: List[PredefinedTask] = List(
  PredefinedTask(
    "hqc",
    version => s"../hqc/hqc-$version/bin/hqc-$version.hex",
    List("128", "192", "256")
  ),
  PredefinedTask(
    "mceliece",
    version => s"../McEliece/mceliece$version/bin/mceliece$version.hex",
    List("348864", "460896", "348864f", "460896f")
  ),
  PredefinedTask(
    "bike",
    version => s"../BIKE-Additional/BIKE-$version/bin/bike-$version.hex",
    List("1", "3")
  ),
  PredefinedTask(
    "test",
    "/home/lab/F/EigeneDateien/Studium/5_SS_21/MA/Code/TrinityCore/RegressionTests/ExtendedRegTest/build/ExtendedRegTest.hex",
    true
  )
)
