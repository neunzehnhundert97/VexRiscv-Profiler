import scala.sys.process._

import scala.collection.parallel.CollectionConverters._
import better.files._
import scala.compiletime.codeOf
import java.util.Date
import java.text.SimpleDateFormat

import scalatags.Text.all._
import scala.util._

object ManualProfiling {

  // Version of schemes
  val hqcVersions = List("128", "192", "256")
  val mcelieceVersions = List("348864", "460896")
  val bikeVersions = List("1", "3")

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  /** Displays the name of a boolean if it is true, else an empty string. */
  inline def literalBoolean(cond: Boolean): String =
    if (cond) codeOf(cond) else ""

  /** Reads arguments of the form "name=" and returns them as an option, optionally converted. */
  def extractArgumentOption[A](args: Seq[String], name: String, conversion: String => A = identity): Option[A] =
    args.find(_.startsWith(s"$name=")).map(_.substring(name.length + 1)).map(conversion)

  def apply(args: Seq[String]): Unit = {
    // Boolean arguments
    val hqc = args.contains("hqc")
    val mceliece = args.contains("mceliece")
    val bike = args.contains("bike")
    val doAnalysis = args.contains("analyse")
    val doProfile = args.contains("profile")
    val filter = args.contains("filter")
    val graph = args.contains("graph")

    // Reduce the number of version to profile, used for testing
    val take = extractArgumentOption(args, "take", _.toInt).getOrElse(10)

    // Arguments for the instruction analysis
    val debuggedFunction = extractArgumentOption(args, "func")
    val desiredCalls = extractArgumentOption(args, "calls", _.toInt).getOrElse(1)

    // Direct input of files to debug
    val manualInputs = extractArgumentOption(args, "input", _.split(','))

    // Flags for the profiler's makefile
    val profilerMakeFlags = extractArgumentOption(args, "profilerFlags", _.split(',')).getOrElse(Array[String]())

    // Address to start execution at
    val bootAt = extractArgumentOption(args, "bootAt").getOrElse("80000000")

    // A list of function names to exclude
    val exclude = extractArgumentOption(args, "exclude", _.split(",").map(_.toLowerCase).toList).getOrElse(Nil)

    // Build
    if (doProfile) {
      println("Build profiler")
      (s"make ${literalBoolean(hqc)} ${literalBoolean(mceliece)} ${literalBoolean(bike)} profile " +
        s"PROFILE=Y ${profilerMakeFlags.mkString(" ")}").!!
    }

    // HQC
    val hqcTasks =
      if (hqc)
        for (version <- hqcVersions.take(take))
          yield Task(s"../hqc/hqc-$version/bin/hqc-$version.hex")
      else Nil

    // McEliece
    val mcElieceTasks =
      if (mceliece)
        for (vers <- mcelieceVersions.take(take); variant <- Seq("", "f").take(take))
          yield Task(s"../McEliece/mceliece$vers$variant/bin/mceliece$vers$variant.hex")
      else Nil

    // BIKE
    val bikeTasks =
      if (bike)
        for (version <- bikeVersions.take(take))
          yield Task(s"../BIKE-Additional/bin/bike-$version.hex")
      else Nil

    // Manual tasks
    val manualTasks = manualInputs match {
      case None => Nil
      case Some(array) =>
        for (file <- array.toList)
          yield Task(file)
    }

    // Excute in parallel
    val tasks = hqcTasks ::: mcElieceTasks ::: bikeTasks ::: manualTasks
    println(s"Start execution of ${tasks.length} tasks")
    tasks.par.foreach(_.execute(doProfile, doAnalysis, debuggedFunction, desiredCalls, bootAt, filter, graph, exclude))
  }

  /** Performs the profiling measurements, expects the executable to be properly built. */
  def profile(hexFile: String, dataFile: String, debuggedFunction: Option[String], calls: Int, bootAt: String): Option[String] = {
    debuggedFunction match {
      case None =>
        // Call profiler
        try {
          (s"./obj_dir/VVexRiscv $hexFile $bootAt 1" #> File(dataFile).toJava).!
          None
        } catch case e: RuntimeException => Some("The profiler exited with a non zero exit value")
      case Some(func) =>
        // Get function address from symbol table
        try {
          val functionAddress =
            (s"riscv32-unknown-elf-objdump -t ${hexFile.replace(".hex", ".elf")}" #|
              // Match with grep the function name as a single word
              raw"grep -P '\b$func\b'").!!.split(" ").head.strip

          // Call profiler
          try {
            (s"./obj_dir/VVexRiscv $hexFile $bootAt 2 $functionAddress $calls" #> File(s"$dataFile-$func").toJava).!
            None
          } catch case e: RuntimeException => Some("The profiler exited with a non zero exit value")

        } catch case e: RuntimeException => Some(s"The given symbol '$func' could not be found in the symbol table.")

    }
  }

  def analyse(
    log: String,
    elf: String,
    out: String,
    filter: Boolean,
    functionName: Option[String],
    graph: Boolean,
    exclude: List[String]
  ): Unit =
    functionName match {
      case None =>
        functionAnalysis(log, elf, out, filter, graph, exclude)
      case Some(func) =>
        instructionAnalysis(s"$log-$func", elf, s"$out-$func", filter, graph)
    }

  def functionAnalysis(log: String, elf: String, out: String, filter: Boolean, graph: Boolean, exclude: List[String]): Unit = {
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
        (for (
          (func, total, totalRel, own, ownRel) <- relData.toList.sortBy(-_._4)
          if (!filter || totalRel > 5 || ownRel > 5)
        )
          yield f"| ${func}%-43s | ${total}%13d | ${totalRel * 100}%12.2f%% | ${own}%13d | ${ownRel * 100}%12.2f%% | ${own * 100.0 / total}%12.2f%% |")
          .mkString("\n") + s"\n\nOverall $totalTime cycles in ${accData.length} functions\n" +
        s"Of these cycles between $minOverhead to $maxOverhead are caused by the measurement\n" +
        f"The real cycle count is $estimatedCycles (${estimatedCycles * 100.0 / totalTime}%.02f%%) ± $derivation (${derivation * 100.0 / estimatedCycles}%.02f%%)%n" +
        s"Created at ${dateFormat.format(new Date())} under revision ${"git log -n 1 --pretty=format:%H -- Profiler/src/ManualProfiling.scala".!!}" +
        s"Profiling data was measured at ${dateFormat.format(new Date(profilingData.lastModifiedTime.toEpochMilli))}\n"
    // And write it to file
    File(out).write(output)

    // If requested, do the graph
    if (graph)
      generateGraph(rootNode, relData, callMap, out)
  }

  def instructionAnalysis(log: String, elf: String, out: String, filter: Boolean, graph: Boolean): Unit = {
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

    if (graph)
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
              h2(s"$func (estimated $min calls)"),
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

final case class Task(
  file: String
) {
  def execute(
    doProfile: Boolean,
    doAnalysis: Boolean,
    debuggedFunction: Option[String],
    desiredCalls: Int,
    bootAt: String,
    filter: Boolean,
    visualize: Boolean,
    exclude: List[String]
  ): Unit = {
    val name = file.split("/").last.split(".hex").head
    val dataFile = s"data/$name"
    val profError =
      if (doProfile) {
        ManualProfiling.profile(file, dataFile, debuggedFunction, desiredCalls, bootAt)
      } else None

    profError match {
      case Some(error) =>
        println(Console.RED + s"$name: $error" + Console.RESET)
      case None =>
        println(s"Done profiling $name")

        if (doAnalysis) {
          ManualProfiling.analyse(
            dataFile,
            s"${file.split(".hex").head}.elf",
            s"results/$name",
            filter,
            debuggedFunction,
            visualize,
            exclude
          )
          println(s"Done analysing $name")
        }
    }

  }
}

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

/** part of a call graph including depth and cycles. */
final case class CallNode(function: String, depth: Int, enterCount: Long, leaveCount: Long, successor: List[CallNode]) {
  def totalTime: Long =
    leaveCount - enterCount

  lazy val ownTime: Long =
    totalTime - succesorTime

  lazy val succesorTime: Long =
    successor.map(_.totalTime).sum

  /** Compresses the tree into a list of functions and cycles. */
  def collectSum(mapping: Map[String, (Long, Long)] = Map(), ancestor: String = ""): Map[String, (Long, Long)] = {

    // Update map with this nodes data
    val updatedMap = mapping.updatedWith(function) {
      case Some(c1 -> c2) if function != ancestor => Some(totalTime + c1, ownTime + c2)
      // Handle direct recursions
      case Some(c1 -> c2) if function == ancestor => Some(c1, c2 + ownTime)
      case _                                      => Some(totalTime, ownTime)
    }
    successor.foldLeft(updatedMap)((m, node) => node.collectSum(m, function))
  }

  /** Compresses the tree into a call map. The numbers in the tuple are times called by parent and times called over all. Return
    * value Map[Caller, Map[Calle, (Times called in all executions, Times called in one execution)]]
    */
  def callMap(map: Map[String, Map[String, (Long, Long)]] = Map(), called: Long = 1): Map[String, Map[String, (Long, Long)]] = {
    val calls = successor.map(_.function).groupBy(identity).map((func, funcs) =>
      func -> (funcs.length.toLong, funcs.length.toLong)
    )

    // Update map with this nodes data
    val updatedMap = map.updatedWith(function) {
      case None => Some(calls)
      case Some(map) =>
        Some((map.toSeq ++ calls.toSeq)
          .groupMap(_._1)(_._2)
          .map((func, data) => func -> (data.map(_._1).sum, data.map(_._2).head)))
    }
    successor.foldLeft(updatedMap)((m, node) => node.callMap(m, updatedMap(function)(node.function)._2))
  }

  /** Remove nodes which match the given strings. */
  def cutOut(filter: List[String]): CallNode = {
    // Partition nodes on given blacklist
    val (keptSuccessors, removedSuccesors) = successor.partition(c => !filter.exists(f => c.function.toLowerCase.contains(f)))
    // Cut all remaining successors
    val cutSuccessors = keptSuccessors.map(_.cutOut(filter))
    // Build new node
    CallNode(
      function,
      depth,
      enterCount,
      // Recalculate the leave count (this breaks its logic, but behaves as expected on the sums)
      enterCount + cutSuccessors.map(_.totalTime).sum + ownTime,
      cutSuccessors
    )
  }

  override def toString: String =
    s"[$function with $totalTime]"

  /** Show complete tree. */
  def showAll: String =
    s"${" " * depth}$this${if (successor.nonEmpty) successor.map(_.showAll).mkString("\n", "\n", "") else ""}"
}

extension (s: String) {
  def toLong(radix: Int): Long =
    java.lang.Long.parseLong(s, radix)
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
