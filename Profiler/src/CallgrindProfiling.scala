import scala.sys.process._

import better.files._

object CallgrindProfiling {

  val hqcVersions = List("128", "192", "256")
  val mcelieceVersions = List("348864", "460896", "6688128")
  val bikeVersions = List("1", "3")

  def callgrind_main(args: Array[String]): Unit = {

    val hqc = args.contains("hqc")
    val mcEliece = args.contains("mceliece")
    val bike = args.contains("bike")
    val gprof = args.contains("gprof")

    // HQC
    if (hqc)
      for (version <- hqcVersions) {
        println(f"HQC-$version")

        if (gprof) {
          f"make -C ../hqc/hqc-$version clean all VEX=No PROFILE=Y TARGET=linux-gnu ARCH=ima".!
          f"../hqc/hqc-$version/bin/hqc-$version.elf".!
          (f"gprof ../hqc/hqc-$version/bin/hqc-$version.elf" #>> File(f"data/hqc-$version").toJava).!
        } else {
          f"make -C ../hqc/hqc-$version clean all RISCV=No VEX=No".!
          callgrind(f"../hqc/hqc-$version/bin/hqc-$version.elf", f"data/hqc-$version")
        }
      }

    // McEliece
    if (mcEliece)
      for (vers <- mcelieceVersions; variant <- Seq("", "f")) {
        val version = f"$vers$variant"
        println(f"McEliece$version")

        if (gprof) {
          f"make -C ../McEliece/mceliece$version VEX=No PROFILE=Y TARGET=linux-gnu ARCH=ima".!
          f"../McEliece/mceliece$version/bin/mceliece$version.elf".!
          (f"gprof ../McEliece/mceliece$version/bin/mceliece$version.elf" #>> File(f"data/mceliece$version").toJava).!
        } else {
          f"make -C ../McEliece/mceliece$version clean all RISCV=No VEX=No".!
          callgrind(f"../McEliece/mceliece$version/bin/mceliece$version.elf", f"data/mceliece$version")
        }
      }

    // BIKE
    if (bike)
      for (version <- bikeVersions) {
        println(f"BIKE-$version")

        if (gprof) {
          f"make -C ../BIKE-Additional clean bin/bike-$version.elf VEX=No PROFILE=Y TARGET=linux-gnu ARCH=ima".!
          f"../BIKE-Additional/bin/bike-$version.elf".!
          (f"gprof ../BIKE-Additional/bin/bike-$version.elf" #>> File(f"data/BIKE-$version").toJava).!
        } else {
          f"make -C ../BIKE-Additional clean bin/bike-$version.elf RISCV=No VEX=No".!
          callgrind(f"../BIKE-Additional/bin/bike-$version.elf", f"data/BIKE-$version")
        }
      }

    // Build graphs
    for (file <- File("./data").glob("*")) {
      (f"gprof2dot --format=${if (gprof) "prof" else "callgrind"} $file --root=main --edge-thres=1" #|
        f"dot -Tpng -o graphs/${file.nameWithoutExtension}.png").!
    }
  }

  def callgrind(target: String, output: String): Unit = {
    f"valgrind --tool=callgrind --callgrind-out-file=$output $target".!
  }
}
