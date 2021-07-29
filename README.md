
This profiler is part of my masterthesis "Development and Implementation of a Hardware/Software-Codesign for Code-based Cryptography".

## Dependencies

Mandatory dependecies, which must be avaiable on the path. The program won't build or run successful without them.

* [Java](https://adoptopenjdk.net/) JDK and JRE, at least Java 11
* [Mill](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html), a Scala build tool
* C++ compiler (preferably g++)
* Make
* [Verilator](https://verilator.org/guide/latest/install.html)
* [riscv32-unknown-elf-objdump](https://github.com/riscv/riscv-gnu-toolchain)

Optional dependecies, which could be avaiable on the path.

* [graphviz](https://graphviz.org/) (dot) to generate images

## Preparation of your executables

The profiler generates raw traces by watching changes in the **mscratch** register, which must be accessable in the simulation. 
The easiest way to achive this is using the CSRPlugin shipped with this repo, which is similar to the original plugin with the mscratch 
register made visible.

To insert meaningful values in this register, the -finstrument-functions flag for gcc can be used, which inserts function calls 
at the start and end of every function.

```c
// Function to call upon entering any function (except this one)
void __attribute__((always_inline)) __attribute__((no_instrument_function)) __cyg_profile_func_enter(void *this_fn,
                                                                                                     __attribute__((unused)) void *call_site)
{
    // Write PC of entered function into mscratch
    __asm__ volatile("csrw mscratch, %0"
                     :
                     : "r"(this_fn));
}

// Function to call upon leaving any function (except this one)
void __attribute__((always_inline)) __attribute__((no_instrument_function)) __cyg_profile_func_exit(__attribute__((unused)) void *this_fn,
                                                                                                    __attribute__((unused)) void *call_site)
{
    // Write magic number 1 into mscratch
    __asm__ volatile("csrwi mscratch, 0x1");
}
```

Manual access is also possible. The profiler automatically resolves addresses to their corresponding symbols, so one has to use 
values which are contained in the symbol table. For instance:

```
.global measurement2
measurement2:
    la x1, measurement2
    csrw mscratch, x1
    ...
    ...
    ...
    csrwi mscratch, 0x1
```

## Building the profiler

Navigate into the project's root directory and issue the following command:

```
mill Profiler.assembly
```

A .jar will be created at ./Profiling/out/Profiler/assembly/dest/out.jar. You can call it by

```
java -jar Profiler.jar [Commands]
```

Instead of building the .jar, you can also use the build tool to run the project. This is prefered, if you plan an modifying the code often.
Do this by typing and executing

```
mill Profiler.run [Commands]
```

## Usage

Currently, the profiler supports three modes of operation.

* a high level measurement which records execution time for every function and can generate a colorized callgraph
* a low level analysis that targets a single function and its decedents and record cycle counts for each individual instruction
* a benchmark which records execution times of multiple variants of the same program and creates a report

The command line arguments are parsed out of order, unknown ones are ignored.

## Command overview

```
Help for VexRiscv Profiler
Usage with source: mill      Profiler.run      [options]
Usage when build:  java -jar Profiler.jar      [options]

Options can be supplied in arbitrary order and don't follow the usual standards
  help, -h, --h         : Display this help
  profile               : Perform the profiling
  analysis              : Perform a analysis
  benchmark             : Perform a benchmark, only usefull with one target and variants
  graph                 : Create a graphical representation for the given analysis
  func=[name]           : Name of a function for instruction level analysis. Also changed the analysis mode
  variants=[v1[,v2..]]  : A list of variants to work on, for instance to benchmark different versions (only predefined tasks)
  input=[file,file...]  : Input files to profile. Expected to be .hex with a .elf of the same name in the same directory
  profilerFlags=[flags] : Flags that will be given to the profilers makefile
  bootAt=[address]      : Address to start execution at (default 80000000)
  exclude=[name,name...]: Functions to be excluded from the call graph
  take=[n]              : Takes only n items from each predefined task's list, take happens after drop
  drop=[n]              : Drops n items from each predefined task's list, drop happens before take
  select=[n[,n..]]      : Selects the given indices from each predefined task's list, not compatible with take or drop
  postfix=[string]      : Appends the given string to every produced output file
```
