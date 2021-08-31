package masterthesis
package profiler

import zio.console
import zio.clock
import zio.*

import java.util.concurrent.TimeUnit

object Main extends zio.App {

  /** ZIO entry point */
  def run(args: List[String]) =
    main(args).catchAll(e => reportError(s"Unhandled error occurred:\n$e")).exitCode

  def main(args: List[String]) =
    if (args.isEmpty || args.contains("help") || args.contains("-h") || args.contains("--h"))
      printHelp
    else
      for {
        start <- clock.currentTime(TimeUnit.SECONDS)
        _ <- Controller(args)
        end <- clock.currentTime(TimeUnit.SECONDS)
        _ <- reportStatus(s"Done in ${end - start} seconds")
      } yield ()

  def printHelp =
    console.putStrLn(s"""Help for VexRiscv Profiler
      |Usage with source: mill      Profiler.run      [options]
      |Usage when build:  java -jar Profiler.jar      [options]
      |
      |Options can be supplied in arbitrary order and don't follow the usual standards, unkown ones are ignored
      |
      |Actions
      |
      |  profile                : Perform the profiling
      |  analysis               : Perform a analysis
      |  process                : Perform profiling, analysis with graphical output, and benchmark if variants are given
      |  benchmark              : Perform a benchmark, only usefull with one target and variants
      |
      |
      |General options
      |
      |  func=[name]            : Name of a function for instruction level analysis. Also changed the analysis mode
      |  variants=[v1[,v2..]]   : A list of variants to work on, for instance to benchmark different versions (only predefined tasks)
      |  input=[file,file...]   : Input files to profile. Expected to be .hex with a .elf of the same name in the same directory
      |  take=[n]               : Takes only n items from each predefined task's list, take happens after drop
      |  drop=[n]               : Drops n items from each predefined task's list, drop happens before take
      |  select=[n[,n..]]       : Selects the given indices from each predefined task's list, not compatible with take or drop
      |  postfix=[string]       : Appends the given string to every produced intermediate file, which does not include analysis results
      |
      |
      |Options for building/profiling
      |
      |  detailed               : Switches to the more detailed profiling mode with overhead
      |  profilerFlags=[flags]  : Flags that will be given to the profilers makefile
      |  bootAt=[address]       : Address to start execution at (default 80000000)
      |  profileThreads=[n]     : Ensures maximal n threads are used for profiling (default is the number of logical cores minus one)
      |
      |
      |Options for analysis
      |
      |  visualize              : Create a graphical representation for the given analysis
      |  imageFormat            : Generates the callgraph in the given file extension. See the dot documentation for a list of formats.
      |  prefix=[string]        : Puts all created analysis files in the given subfolder under their respective target
      |  exclude=[name,name...] : Functions to be excluded from the call graph
      |  zoom=[name]            : Zooms to the specified function in the call graph and uses it as the new root
      |  analysisThreads=[n]    : Ensures maximal n threads are used for heavy analysis (default is the number of logical cores minus one)
      |
      |
      |Misc Options
      |
      |  help, -h, --h          : Display this help
    """.stripMargin)
}
