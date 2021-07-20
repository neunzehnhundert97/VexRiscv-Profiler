
This profiler is part of my masterthesis "Development and Implementation of a Hardware/Software-Codesign for Code-based Cryptography".

## Dependencies

The tool itself is written in Scala3 and needs only a java runtime and the build tool [mill](https://github.com/com-lihaoyi/mill) 
which will take care of everything else in this direction. In order to profile your program, you additionally need a C++ compiler 
(preferably g++) and verilator.

In order to resolve symbols, the **riscv32-unknown-elf-objdump** must be avaiable on the path.

## Usage

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

Manual access is also possible. The profiler automatically resolves addresses to their corresponding symbols, so one should use 
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

Currently, the profiler supports three modes of operation.

* a high level measurement which records execution time for every function and can generate a colorized callgraph
* a low level analysis that targets a single function and its decedents and record cycle counts for each individual instruction
* a benchmark which records execution times of multiple variants of the same program and creates a report

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
