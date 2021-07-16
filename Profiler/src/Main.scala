package masterthesis
package profiler

@main
def main(args: String*): Unit =
  if (args.isEmpty || args.contains("help") || args.contains("-h") || args.contains("--h"))
    printHelp()
  else
    ManualProfiling(args)

def printHelp(): Unit =
  println(s"""Help for VexRiscv Profiler
             |Usage with source: mill      Profiler.run      [options]
             |Usage when build:  java -jar Profiler.jar      [options]
             |
             |Options can be supplied in arbitrary order and don't follow the usual standards
             |  help, -h, --h         : Display this help
             |  profile               : Perform the profiling
             |  analysis              : Perform a analysis
             |  graph                 : Create a graphical representation for the given analysis
             |  func=[name]           : Name of a function for instruction level analysis. Also changed the analysis mode
             |  input=[file,file...]  : Input files to profile. Expected to be .hex with a .elf of the same name in the same directory
             |  profilerFlags=[flags] : Flags that will be given to the profilers makefile
             |  bootAt=[address]      : Address to start execution at (default 80000000)
             |  exclude=[name,name...]: Functions to be excluded from the call graph
             |  take=[n]              : Takes only n items from each predefined task's list, take happens after drop
             |  drop=[n]              : Drops n items from each predefined task's list, drop happens before take
             |  postfix=[string]      : Appends the given string to every produced output file
  """.stripMargin)
