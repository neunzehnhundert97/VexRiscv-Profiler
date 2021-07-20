package masterthesis
package profiler

import java.util.Date
import java.text.SimpleDateFormat

import scalatags.Text.all._

import better.files._

import zio.{IO, Task, ZIO, UIO, URIO, RIO}
import zio.blocking.Blocking

import upickle.default.read

object Analysis {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  def apply(log: String, elf: String, out: String, config: Config): ZIO[Blocking, String, Unit] = {
    // Verify log file
    val logFile = File(log)
    if (!logFile.exists)
      IO.fail(s"There is no data file '$log', maybe you forgot to run the profiler?")
    else if (logFile.isEmpty)
      IO.fail(s"The file '$log' is empty. Maybe the profiler was given wrong arguments?")
    else
      // Execute analysis and catch all errors
      config.debuggedFunction.match {
        case None =>
          highLevel(logFile, elf, out, config)
        case Some(func) =>
          lowLevel(logFile, elf, out, config)
      }.mapError(e => s"An exception ocurred during analysis: ${e}")
  }

  /** The high level analysis given cycles counts and call times on a function level. Optionally, a call graph can be produced. */
  def highLevel(log: File, elf: String, out: String, config: Config) = {
    import config.*

    for {
      // Read measurements and parse them into case classes
      data <- IO.effect(log.lineIterator.map(FunctionMeasurement.createFromTrace).collect { case Some(m) => m })
      // Read symbol and create mapping
      (symbols, _) <- readSymbolTable(s"$log-sym.json")
      revision <- currentRevision
      currentDate <- IO.effect(new Date())
      logDate <- IO.effect(new Date(log.lastModifiedTime.toEpochMilli))
      // Analyze collected data
      tree <- IO.fromOption(accumulateSophisticated(data, symbols)).mapError(_ => "Call graph could not be created")
      callTreeData = analyzeCallTree(tree, symbols, config)
      // Generate text output
      report = callGraphReport(callTreeData, currentDate, logDate, revision)
      _ <- writeToFile(out)(report)
      // Generate graph
      _ <- IO.when(visualize)(
        writeToFile(out + ".png")(generateDotGraph(callTreeData)) *> generateDotGraph(out + ".png", out + ".png")
      )
    } yield ()
  }

  type CallTreeData =
    (CallNode, List[(String, Long, Long)], List[(String, Long, Double, Long, Double)], Map[String, Map[String, (Long, Long)]])

  /** Builds the call tree from measurements and provides analytical data. */
  def analyzeCallTree(tree: CallNode, symbols: Map[String, String], config: Config): CallTreeData = {
    import config.*
    // Build call tree
    val rootNode = tree.cutOut(exclude)
    // Accumulate times
    val accData = rootNode.collectSum().map((a, b) => (a, b._1, b._2)).toList

    // Find maximal entry (the boot function which is about the whole program)
    val max = accData.maxBy(_._2)
    val maxTotal = max._2.toDouble
    // Compute percentages
    val relData = accData.map((f, tot, own) => (f, tot, tot / maxTotal, own, own / maxTotal))

    // Get call map
    val callMap = rootNode.callMap()

    (rootNode, accData, relData, callMap)
  }

  /** Creates a table as string describing the results off the call tree analysis. */
  def callGraphReport(data: CallTreeData, date: Date, logDate: Date, revision: String): String = {
    val (rootNode, accData, relData, callMap) = data
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
    f"| ${"Function name"}%-43s | ${"Total abs"}%13s | ${"Total rel"}%13s | ${"Own abs"}%13s | ${"Own abs"}%13s | ${"Total/Own rel"}%13s |%n" +
      (for ((func, total, totalRel, own, ownRel) <- relData.toList.sortBy(-_._4))
        yield f"| ${func}%-43s | ${total}%13d | ${totalRel * 100}%12.2f%% | ${own}%13d | ${ownRel * 100}%12.2f%% | ${own * 100.0 / total}%12.2f%% |")
        .mkString("\n") + s"\n\nOverall $totalTime cycles in ${accData.length} functions\n" +
      s"Of these cycles between $minOverhead to $maxOverhead are caused by the measurement\n" +
      f"The real cycle count is $estimatedCycles (${estimatedCycles * 100.0 / totalTime}%.02f%%) ± $derivation (${derivation * 100.0 / estimatedCycles}%.02f%%)%n" +
      s"Created at ${dateFormat.format(date)} under revision $revision " +
      s"Profiling data was measured at ${dateFormat.format(logDate)}\n"
  }

  /** A detailed overview for a single function and its decendents is created, giving cycles count for each individual
    * instruction. A refined HTML can be created additionally.
    */
  def lowLevel(log: File, elf: String, out: String, config: Config): RIO[Blocking, Unit] = {
    import config.*

    for {
      // Read measurements and parse them
      data <- IO.effect(log.lineIterator.map(raw"(\S{8}):(\d+)".r.findFirstMatchIn)
        .collect { case Some(mat) => mat.group(1) -> mat.group(2).toInt })
      // Read symbol and create mapping
      symbolMappings <- readSymbolTable(s"$log-sym.json")
      // Read disassembly and create mapping from address to mnemonic and operands
      symbols <- IO.effect(os.proc("riscv32-unknown-elf-objdump", "-d", elf).call().out.lines)
      // Read custom encodings from file
      additionalEncoding <- parseAdditionalInstructions("instructions_c.scv")
      // Do the analysis
      (output, groupedInstructions) = analyzeCycleCounts(data, symbolMappings, symbols, additionalEncoding)
      // Write text output
      _ <- writeToFile(out)(output)
      _ <- IO.when(visualize)(writeToFile(s"$out.html")(generateHotnessHTML(groupedInstructions, out)))
    } yield ()
  }

  /** */
  def analyzeCycleCounts(
    data: Iterator[(String, Int)],
    symbolMappings: (Map[String, String], Map[Long, String]),
    symbols: Vector[String],
    additionalEncoding: List[InstructionEncoding]
  ): (String, List[(String, List[(String, List[Int], String)])]) = {
    val (stringToSymbols, longToSymbols) = symbolMappings
    val disassemblyRegex = raw"([0-9,a-f]+):\s+([0-9,a-f]+)\s+(.+)".r

    // Construct a mapping of a function to its instructions and cycle counts
    // Put it into a block to free memory of local variables

    val instructionsAll = symbols.map(disassemblyRegex.findFirstMatchIn)
      .collect { case Some(mat) => mat.group(1).toUpperCase -> mat.group(3) }
      .map((addr, ins) => (("0" * (8 - addr.length)) + addr, ins)).toMap

    val instructions = instructionsAll.map((addr, ins) =>
      // If the disassembly failed to decode the instruction, try to do it with the additonal encodings
      if (ins.startsWith("0x"))
        addr -> additionalEncoding.map(e => e.decode(ins)).collectFirst { case Some(i) => i }.getOrElse(ins)
      else
        addr -> ins
    )

    // Compute cycle counts of groups
    val cycleCounts = data.foldLeft(Map[String, List[Int]]()) {
      case (map, (addr, cycles)) => map.updatedWith(addr) {
          case None       => Some(List(cycles))
          case Some(list) => Some(cycles :: list)
        }
    }

    // Group by parent function
    val groupedInstructions = cycleCounts.map { (addr, cycles) =>
      (addr, cycles, instructions.getOrElse(addr, "OUT OF CODE"))
    }.groupBy((addr, cycles, ins) =>
      longToSymbols.map((num, sym) => (addr.toLong(16) - num, sym)).filter((num, _) => num > 0).minBy(_._1)._2
    ).map((func, data) => (func, data.toList.sortBy(_._1))).toList.sortBy(_._2.head._1)

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

    output -> groupedInstructions
  }

  /** Create a callgraph using the dot language and write it to disk. */
  def generateDotGraph(data: CallTreeData): String = {
    val (root, _, timings, callMap) = data
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
        f""""$func" [shape=box,label="$func\n${data._2 * 100}%4.2f%%\n(${data._4 * 100}%4.2f%%)${
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
          .map((calle, count) => s""""$caller" -> "$calle" [label="${if (count._2 > 1) s"${count._2}x" else ""}"];""")
          .mkString("\n")
      )
      .mkString("\n")

    s"""digraph {bgcolor="transparent" \n $nodes $edges}"""

  }

  /** Converts the given dotfile into an PNG image. Needs the executable "dot" to be present in PATH. */
  def generateDotGraph(dotFile: String, imageFile: String): Task[Unit] =
    IO.effect(os.proc("dot", "-Tpng", imageFile, "-o", dotFile).call()).discard

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
    groupedInstructions: List[String -> List[(String, List[Int], String)]],
    out: String
  ): String =
    // Generate the HTML using scalatags
    doctype("HTML")(html(
      head(
        tag("style")(
          raw("td > p {margin-left: 20px;margin-right:20px;margin-top: 0;margin-bottom: 0} " +
            ".blockBegin p {margin-top: 10px;} " +
            ".blockEnd p {margin-bottom: 10px;} " +
            ".blockBegin td {vertical-align: bottom;}" +
            ".mnemonic {font-weight: bolder;} " +
            "body {font-family: Fira Code, monospace} " +
            "td {vertical-align: top;} ")
        ),
        tag("title")(out.split("/").last)
      ),
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

  /** Build a call tree of the traces. */
  def accumulateSophisticated(data: Iterator[FunctionMeasurement], symbols: Map[String, String]): Option[CallNode] = {
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
    }._1.headOption
  }

  /** Reads the dumped symbol table. */
  def readSymbolTable(file: String): RIO[Blocking, (Map[String, String], Map[Long, String])] = for {
    // Read symbol and create mapping
    stringToSymbols <- IO.effect(read[Map[String, String]](File(file).contentAsString))
    // .mapError(_ => "Could not read dumped symbol table")
  } yield stringToSymbols -> stringToSymbols.map((addr, sym) => addr.toLong(16) -> sym)

  /** Read the given csv of definitions. */
  def parseAdditionalInstructions(file: String): Task[List[InstructionEncoding]] = IO.effect {
    // Read csv
    val file = File("instructions_s.csv")
    if (file.exists) {
      // Drop heading line (column names)
      val rawData = file.lineIterator.drop(1)
      rawData
        .map(_.split(";"))
        .map(arr => InstructionEncoding(arr.head, arr(1), EncodingType.valueOf(arr.last)))
        .toList
    } else
      Nil
  }

  /** Uses git to fetch the revision of the profiler. */
  def currentRevision: Task[String] = IO.effect {
    os.proc("git", "log", "-n", 1, "--pretty=format:%H", "--", "Profiler/src/ManualProfiling.scala")
      .call().out.text
  }
}
