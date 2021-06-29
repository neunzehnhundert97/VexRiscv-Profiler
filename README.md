
This profiler is part of my masterthesis "Development and Implementation of a Hardware/Software-Codesign for Code-based Cryptography".

## Dependencies

The tool itself is written in Scala3 and needs only a java runtime and the build tool [mill](https://github.com/com-lihaoyi/mill) 
which will take care of everything else in this direction. In order to profile your program, you additionally need a C++ compiler 
(preferably g++) and verilator.

## Usage

```
Help for VexRiscv Profiler
Usage with source: mill      Profiler.run      [options]
Usage when build:  java -jar Profiler.jar      [options]

Options can be supplied in arbitrary order and don't follow the usual standards
  help, -h, --h         : Display this help
  profile               : Perform the profiling
  analysis              : Perform a analysis
  graph                 : Create a graphical representation for the given analysis
  func=[name]           : Name of a function for instruction level analysis. Also changed the analysis mode
  input=[file,file...]  : Input files to profile. Expected to be .hex with a .elf of the same name in the same directory
  profilerFlags=[flags] : Flags that will be given to the profilers makefile
  bootAt=[address]      : Address to start execution at (default 80000000)
  exclude=[name,name...]: Functions to be excluded from the call graph
```
