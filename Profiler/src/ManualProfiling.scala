import scala.sys.process._

import scala.collection.parallel.CollectionConverters._
import better.files._
import scala.compiletime.codeOf
import java.util.Date
import java.text.SimpleDateFormat

import scalatags.Text.all._

object ManualProfiling {

  // Version of schemes
  val hqcVersions = List("128", "192", "256")
  val mcelieceVersions = List("348864", "460896")
  val bikeVersions = List("1", "3")

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  /** Path to objdump for the analyzed elf. */
  val objdump = "riscv32-unknown-elf-objdump"

  /** Displays the name of a boolean if it is true, else an empty string. */
  inline def literalBoolean(inline cond: Boolean): String =
    if (cond) codeOf(cond).split('.').last else ""

  /** Reads arguments of the form "name=" and returns them as an option, optionally converted. */
  def extractArgumentOption[A](args: Seq[String], name: String, conversion: String => A = identity): Option[A] =
    args.find(_.startsWith(s"$name=")).map(_.substring(name.length + 1)).map(conversion)

  def apply(args: Seq[String]): Unit = {
    val config = readCLIParameters(args)
    import config.*

    // Build profiler
    buildProfiler(config) match {
      case Error(msg) =>
        reportError(msg)
      case Success(_) =>
        // HQC
        val hqcTasks =
          if (hqc)
            for (version <- hqcVersions.take(take))
              yield Task(s"../hqc/hqc-$version/bin/hqc-$version.hex", config)
          else Nil

        // McEliece
        val mcElieceTasks =
          if (mceliece)
            for (vers <- mcelieceVersions.take(take); variant <- Seq("", "f").take(take))
              yield Task(s"../McEliece/mceliece$vers$variant/bin/mceliece$vers$variant.hex", config)
          else Nil

        // BIKE
        val bikeTasks =
          if (bike)
            for (version <- bikeVersions.take(take))
              yield Task(s"../BIKE-Additional/bin/bike-$version.hex", config)
          else Nil

        // Create tasks
        val manualTasks = for (file <- manualInputs)
          yield Task(file, config)

        // Excute in parallel
        val tasks = hqcTasks ::: mcElieceTasks ::: bikeTasks ::: manualTasks
        reportStatus(s"Start execution of ${tasks.length} tasks")
        tasks.par.foreach(_.execute())
    }
  }

  def readCLIParameters(args: Seq[String]): Config = {
    // Boolean arguments
    val hqc = args.contains("hqc")
    val mceliece = args.contains("mceliece")
    val bike = args.contains("bike")
    val doAnalysis = args.contains("analyse")
    val doProfile = args.contains("profile")
    val visualize = args.contains("graph") || args.contains("visualize")

    // Reduce the number of version to profile, used for testing
    val take = extractArgumentOption(args, "take", _.toInt).getOrElse(10)

    // Arguments for the instruction analysis
    val debuggedFunction = extractArgumentOption(args, "func")
    val desiredCalls = extractArgumentOption(args, "calls", _.toInt).getOrElse(1)

    // Direct input of files to debug
    val manualInputs = extractArgumentOption(args, "input", _.split(',').toList).getOrElse(Nil)

    // Flags for the profiler's makefile
    val profilerMakeFlags = extractArgumentOption(args, "profilerFlags", _.split(',').toList).getOrElse(Nil)

    // Address to start execution at
    val bootAt = extractArgumentOption(args, "bootAt").getOrElse("80000000")

    // A list of function names to exclude
    val exclude = extractArgumentOption(args, "exclude", _.split(",").map(_.toLowerCase).toList).getOrElse(Nil)

    // Put all in one object to ease passing around
    Config(
      doAnalysis,
      doProfile,
      manualInputs,
      visualize,
      take,
      debuggedFunction,
      desiredCalls,
      profilerMakeFlags,
      bootAt,
      exclude,
      hqc,
      bike,
      mceliece
    )
  }

  /** Calls the profiler's makefile with the given arguments if profiling is needed. */
  def buildProfiler(config: Config): ErrorOrSuccess = {
    import config.*
    if (doProfile) {
      reportStatus("Building verilator simulation")
      try {
        (s"make ${literalBoolean(hqc)} ${literalBoolean(mceliece)} ${literalBoolean(bike)} all " +
          s"PROFILE=Y ${profilerMakeFlags.mkString(" ")}").!!(ProcessLogger(_ => ()))
        Right(())
      } catch case e: RuntimeException => Left(s"The profiler could not be build: ${e.getLocalizedMessage}")
    } else Right(())
  }

  /** Performs the profiling measurements, expects the executable to be properly built. */
  def profile(
    hexFile: String,
    dataFile: String,
    debuggedFunction: Option[String],
    calls: Int,
    bootAt: String
  ): ErrorOrSuccess =
    debuggedFunction match {
      case None =>
        // Call profiler
        try {
          (s"./obj_dir/VVexRiscv $hexFile $bootAt 1" #> File(dataFile).toJava).!
          Right(())
        } catch case e: RuntimeException => Left("The profiler exited with a non zero exit value")
      case Some(func) =>
        // Get function address from symbol table
        try {
          val logger = ProcessLogger(_ => (), _ => ())
          val functionAddress =
            (s"$objdump -t ${hexFile.replace(".hex", ".elf")}" #|
              // Match with grep the function name as a single word
              raw"grep -P '\b$func\b'").!!(logger).split(" ").head.strip

          // Call profiler
          try {
            (s"./obj_dir/VVexRiscv $hexFile $bootAt 2 $functionAddress $calls" #> File(s"$dataFile-$func").toJava).!
            Right(())
          } catch case e: RuntimeException => Left("The profiler exited with a non zero exit value")

        } catch case e: RuntimeException => Left(s"The given symbol '$func' could not be found in the symbol table.")
    }

  def analyse(log: String, elf: String, out: String, config: Config): ErrorOrSuccess =
    try {
      config.debuggedFunction match {
        case None =>
          highLevel(log, elf, out, config)
        case Some(func) =>
          lowLevel(s"$log-$func", elf, s"$out-$func", config)
      }
      Right(())
    } catch case e: Exception => Left(s"An exception ocurred during analysis: ${e.getMessage}")

  def highLevel(log: String, elf: String, out: String, config: Config): Unit = {
    import config.*

    // Read measurements and parse them into case classes
    val profilingData = File(log)
    val data = profilingData.lineIterator.map(FunctionMeasurement.createFromTrace).collect { case Some(m) => m }

    // Read symbol and create mapping
    val (symbols, _) = parseSymbolTable(elf)

    // Build call tree
    val rootNode = accumulateSophisticated(data, symbols).cutOut(exclude)
    // Accumulate times
    val accData = rootNode.collectSum().map((a, b) => (a, b._1, b._2)).toList

    // Find maximal entry (the boot function which is about the whole program)
    val max = accData.maxBy(_._2)
    val maxTotal = max._2.toDouble
    // Compute percentages
    val relData = accData.map((f, tot, own) => (f, tot, tot / maxTotal, own, own / maxTotal))

    // Get call map
    val callMap = rootNode.callMap()

    val sumOfCalls = callMap.valuesIterator.flatMap(_.valuesIterator).map(_._1).sum
    // At minimum, each instruction uses two instructions to signal entry and exit
    // These instructions (csrw and csrwi) both take three cycles
    // Additionally, an instructions needed to get the PC of the function and entry
    // But this is not needed every time in case of inlined functions
    val minOverhead = sumOfCalls * 3 * 2
    val maxOverhead = sumOfCalls * (3 * 2 + 1)
    // In truth, I don't know how much calls are actually inlined, and I won't bother finding out
    // So the average is used
    val avgOverhead = Math.round((minOverhead + maxOverhead) / 2.0)

    val totalTime = rootNode.totalTime
    val estimatedCycles = totalTime - avgOverhead
    val derivation = avgOverhead / 2

    // Create output string
    var output =
      f"| ${"Function name"}%-43s | ${"Total abs"}%13s | ${"Total rel"}%13s | ${"Own abs"}%13s | ${"Own abs"}%13s | ${"Total/Own rel"}%13s |%n" +
        (for ((func, total, totalRel, own, ownRel) <- relData.toList.sortBy(-_._4))
          yield f"| ${func}%-43s | ${total}%13d | ${totalRel * 100}%12.2f%% | ${own}%13d | ${ownRel * 100}%12.2f%% | ${own * 100.0 / total}%12.2f%% |")
          .mkString("\n") + s"\n\nOverall $totalTime cycles in ${accData.length} functions\n" +
        s"Of these cycles between $minOverhead to $maxOverhead are caused by the measurement\n" +
        f"The real cycle count is $estimatedCycles (${estimatedCycles * 100.0 / totalTime}%.02f%%) ± $derivation (${derivation * 100.0 / estimatedCycles}%.02f%%)%n" +
        s"Created at ${dateFormat.format(new Date())} under revision ${"git log -n 1 --pretty=format:%H -- Profiler/src/ManualProfiling.scala".!!}" +
        s"Profiling data was measured at ${dateFormat.format(new Date(profilingData.lastModifiedTime.toEpochMilli))}\n"
    // And write it to file
    File(out).write(output)

    // If requested, do the graph
    if (visualize)
      generateGraph(rootNode, relData, callMap, out)
  }

  def lowLevel(log: String, elf: String, out: String, config: Config): Unit = {
    import config.*

    // Read measurements and parse them
    val profilingData = File(log)
    val data = profilingData.lineIterator.map(raw"(\S{8}):(\d+)".r.findFirstMatchIn)
      .collect { case Some(mat) => mat.group(1) -> mat.group(2).toInt }

    // Read symbol and create mapping
    val (stringToSymbols, longToSymbols) = parseSymbolTable(elf)

    val disassemblyRegex = raw"([0-9,a-f]+):\s+([0-9,a-f]{8})\s+(.+)".r

    // Construct a mapping of a function to its instructions and cycle counts
    // Put it into a block to free memory of local variables
    val groupedInstructions = {
      // Read disassembly and create mapping from address to mnemonic and operands
      val instructions = s"riscv32-unknown-elf-objdump -d $elf".lazyLines
        .map(disassemblyRegex.findFirstMatchIn).collect { case Some(mat) => mat.group(1).toUpperCase -> mat.group(3) }
        .map((addr, ins) => (("0" * (8 - addr.length)) + addr, ins)).toMap

      // Compute cycle counts of groups
      val cycleCounts = data.foldLeft(Map[String, List[Int]]()) {
        case (map, (addr, cycles)) => map.updatedWith(addr) {
            case None       => Some(List(cycles))
            case Some(list) => Some(cycles :: list)
          }
      }

      // Group by parent function
      cycleCounts.map((addr, cycles) => (addr, cycles, instructions(addr)))
        .groupBy((addr, cycles, ins) =>
          longToSymbols.map((num, sym) => (addr.toLong(16) - num, sym)).filter((num, _) => num > 0).minBy(_._1)._2
        ).map((func, data) => (func, data.toList.sortBy(_._1))).toMap
    }

    // Create text output
    val output =
      (for ((func, data) <- groupedInstructions)
        yield s"$func:\n| Address  |     Cycles |   Calls | C / exe (median) | Instruction\n" + (for ((addr, cycles, ins) <- data)
          yield {
            val count = cycles.sum
            val min = cycles.min
            val max = cycles.max
            val median = cycles.median
            val perExecution =
              if (min == max)
                s"$min"
              else f"$min - $max ($median%.0f)"
            f"| $addr | $count%10d | ${count * 1.0 / max}%7.0f | $perExecution%16s | $ins"
          }).mkString(
          "\n"
        ) + "\n").mkString("\n")

    // Write text output
    File(out).write(output)

    if (visualize)
      generateHotnessHTML(groupedInstructions, out)
  }

  def generateGraph(
    root: CallNode,
    timings: List[(String, Long, Double, Long, Double)],
    callMap: Map[String, Map[String, (Long, Long)]],
    out: String
  ): Unit = {
    // Flatten call map
    val allCalls = callMap.values.toSeq.flatMap(_.toSeq).groupMapReduce(_._1)(_._2._1)(_ + _)

    // Process timings
    val timeMapping = timings.map((a, b, c, d, e) => (a, (b, c, d, e))).toMap
    val maxRel = timings.maxBy(_._4)._5

    // Create nodes of the graph
    val nodes = timeMapping
      // Take only functions with at least 5% runtime
      .filter(_._2._2 > 0.05)
      .map((func, data) =>
        f"""$func [shape=box,label="$func\n${data._2 * 100}%4.2f%%\n(${data._4 * 100}%4.2f%%)${
          val calls = allCalls.getOrElse(func, 1L)
          if (calls > 1) f"\n${calls}x" else ""
        }",fillcolor="${gradient(
          data._4 / maxRel
        )}",style=filled];"""
      ).mkString("\n")

    // Create edges of the graph
    val edges = callMap
      // Take only functions with at least 5% runtime
      .filter((func, _) => timeMapping(func)._2 > 0.05)
      .map((caller, calles) =>
        // Take only functions with at least 5% runtime
        calles.filter(c => timeMapping(c._1)._2 > 0.05)
          .map((calle, count) => s"""$caller -> $calle [label="${if (count._2 > 1) s"${count._2}x" else ""}"];""")
          .mkString("\n")
      )
      .mkString("\n")

    // Write to file
    File(out + ".png").write(s"""digraph {bgcolor="transparent" \n $nodes $edges}""")
    // Convert to graph and overwrite dot file
    s"dot -Tpng ${out + ".png"} -o ${out + ".png"}".!
  }

  /** Return color string for a gradient. */
  def gradient(position: Double): String = {
    val high = 0xff3300 // red
    val low = 0x33cc33 // green

    // Interpolate linear and create hex string
    f"#${(((high >> 16) & 0xff) * position + ((low >> 16) & 0xff) * (1 - position)).toInt}%02X" +
      f"${(((high >> 8) & 0xff) * position + ((low >> 8) & 0xff) * (1 - position)).toInt}%02X" +
      f"${(((high >> 0) & 0xff) * position + ((low >> 0) & 0xff) * (1 - position)).toInt}%02X"
  }

  /** Renders the instruction analysis into a HTML file. */
  def generateHotnessHTML(
    groupedInstructions: Map[String, List[(String, List[Int], String)]],
    out: String
  ): Unit = {
    // Generate the HTML using scalatags
    val output = doctype("HTML")(html(
      head(tag("style")(
        raw("td > p {margin-left: 20px;margin-right:20px;margin-top: 0;margin-bottom: 0} " +
          ".blockBegin p {margin-top: 10px;} " +
          ".blockEnd p {margin-bottom: 10px;} " +
          ".blockBegin td {vertical-align: bottom;}" +
          ".mnemonic {font-weight: bolder;} " +
          "body {font-family: Fira Code, monospace} " +
          "td {vertical-align: top;} "),
        tag("title")(out.split("/").last)
      )),
      body(
        for ((func, data) <- groupedInstructions.toList if !Seq("__cyg_profile_func_enter").contains(func))
          yield {

            // Get max to get relative counts
            val max = data.map(_._2.sum).max.toDouble
            // Get min to divide by times the function was called
            val min = data.map(_._2.sum).min

            val maxMedianCycleCount = data.map(_._2.median).max

            // Find begin of basic blocks by looking at branch instructions
            val blockBegins = data.map(_._3).map(raw"b\w+.*([0-9,a-f]{8}) <".r.findFirstMatchIn)
              .collect { case Some(mat) => mat.group(1).toUpperCase }.toSet

            // Center the content by setting a margin
            div(style := "margin-left: 30vw;")(
              h2(s"$func (estimated $min calls) ${data.flatMap(_._2).sum} cycles"),
              table(
                // Iterate over the instructions
                for ((addr, cycles, ins) <- data)
                  yield {
                    // Get detailed cycle data
                    val cMin = cycles.min
                    val cMax = cycles.max
                    val cycleMedian = cycles.median
                    val count = cycles.sum

                    val (mnemonic, args) = ins.split(raw"\s+", 2) match {
                      case Array(m)    => m -> ""
                      case Array(m, a) => m -> a
                    }

                    tr(
                      // Set a margin if this instruction is last or first in a basic block
                      if (blockBegins(addr)) cls := "blockBegin" else if (ins.startsWith("b")) cls := "blockEnd" else ()
                    )(
                      // Address
                      td(p(addr)),
                      // Summed cycle count
                      td(p(
                        span(style := s"color: ${gradient(count / max)}")(count / min)
                      )),
                      // Cycle count of one execution
                      td(p(
                        style := s"color: ${gradient(cycleMedian / maxMedianCycleCount)}",
                        // Display median as tooltip
                        title := f"Median: $cycleMedian%.0f"
                      )(if (cMax != count / min)
                        if (cMin != cMax) s" $cMin - $cMax" else s" $cMin"
                      else "")),
                      // Mnemonic (bold)
                      td(p(span(cls := "mnemonic")(mnemonic))),
                      // Operand and immediates
                      td(span(args))
                    )
                  }
              )
            )
          }
      )
    )).render

    // Write output
    File(s"$out.html").write(output)
  }

  /** Build a call tree of the traces. */
  def accumulateSophisticated(data: Iterator[FunctionMeasurement], symbols: Map[String, String]) = {
    // Build tree
    data.foldLeft((List[CallNode](), List[Long](), 0)) {
      // Entering a function
      case ((queue, stack, depth), FunctionMeasurement(true, addr, ctr)) =>
        // Do nothing besides pushing the counter value
        (queue, ctr :: stack, depth + 1)
      // Leaving a function
      case ((queue, stack, depth), FunctionMeasurement(false, addr, ctr)) =>
        // Pop last entry from stack
        val (entry :: rest) = stack
        // Get the nodes that are below the current function and the rest
        val (lower, higher) = queue.span(_.depth > depth)
        (
          // Build new node and push it onto the stack
          CallNode(symbols.getOrElse(addr, addr), depth, entry, ctr, lower) :: higher,
          rest,
          depth - 1
        )
    }._1.head
  }

  /** Creates the symbol table of a given elf and return two mappings from addresses to symbols, one with longs and one with
    * strings.
    */
  def parseSymbolTable(file: String): (Map[String, String], Map[Long, String]) = {
    // Read symbol and create mapping
    val stringToSymbols = (s"riscv32-unknown-elf-objdump -t $file").!!
      // Split lines, split columns
      .split("\n").map(_.split(" "))
      // take address and symbol name
      .filter(_.length > 2).map(a => raw"[0-9,a-f,A-F]{8}".r.findFirstIn(a.head) -> a.last)
      .collect { case (Some(head), last) => head.toUpperCase -> last.strip.replace(".", "dot") }.toMap

    stringToSymbols -> stringToSymbols.map((addr, sym) => addr.toLong(16) -> sym)
  }
}
