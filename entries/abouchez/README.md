# Arnaud Bouchez

**mORMot entry to The One Billion Row Challenge in Object Pascal.**

## mORMot 2 is Required

This entry requires the **mORMot 2** package to compile.

Download it from https://github.com/synopse/mORMot2

It is better to fork the current state of the *mORMot 2* repository, or get the latest release.

## Licence Terms

This code is licenced by its sole author (A. Bouchez) as MIT terms, to be used for pedagogical reasons.

I am very happy to share decades of server-side performance coding techniques using FPC on x86_64. ;)

## Presentation

Here are the main ideas behind this implementation proposal:

- **mORMot** makes cross-platform and cross-compiler support simple (e.g. `TMemMap`, `TDynArray.Sort`,`TTextWriter`, `SetThreadCpuAffinity`, `crc32c`, `ConsoleWrite` or command-line parsing);
- Will memmap the entire 16GB file at once into memory (so won't work on 32-bit OS, but reduce syscalls);
- Process file in parallel using several threads (configurable, with `-t=16` by default);
- Fed each thread from 64MB chunks of input (because thread scheduling is unfair, it is inefficient to pre-divide the size of the whole input file into the number of threads);
- Each thread manages its own data, so there is no lock until the thread is finished and data is consolidated;
- Each station information (name and values) is packed into a record of exactly 64 bytes, with no external pointer/string, to match the CPU L1 cache size for efficiency;
- Use a dedicated hash table for the name lookup, with direct crc32c SSE4.2 hash - when `TDynArrayHashed` is involved, it requires a transient name copy on the stack, which is noticeably slower (see last paragraph of this document);
- Store values as 16-bit or 32-bit integers (i.e. temperature multiplied by 10);
- Parse temperatures with a dedicated code (expects single decimal input values);
- No memory allocation (e.g. no transient `string` or `TBytes`) nor any syscall is done during the parsing process to reduce contention and ensure the process is only CPU-bound and RAM-bound (we checked this with `strace` on Linux);
- Pascal code was tuned to generate the best possible asm output on FPC x86_64 (which is our target);
- Some dedicated x86_64 asm has been written to replace *mORMot* `crc32c` and `MemCmp` general-purpose functions and gain a last few percents (nice to have);
- Can optionally output timing statistics and hash value on the console to debug and refine settings (with the `-v` command line switch);
- Can optionally set each thread affinity to a single core (with the `-a` command line switch).

The "64 bytes cache line" trick is quite unique among all implementations of the "1brc" I have seen in any language - and it does make a noticeable difference in performance. The L1 cache is well known to be the main bottleneck for any efficient in-memory process. We are very lucky the station names are just big enough to fill no more than 64 bytes, with min/max values reduced as 16-bit smallint - resulting in temperature range of -3276.7..+3276.8 which seems fair on our planet according to the IPCC. ;)

## Usage

If you execute the `abouchez` executable without any parameter, it will give you some hints about its usage (using *mORMot* `TCommandLine` abilities):

```
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ ./abouchez 
The mORMot One Billion Row Challenge

Usage: abouchez  <filename> [options] [params]

   <filename>         the data source filename

Options:
  -v, --verbose       generate verbose output with timing
  -a, --affinity      force thread affinity to a single CPU core
  -h, --help          display this help

Params:
  -t, --threads <number> (default 16)
                      number of threads to run
```
We will use these command-line switches for local (dev PC), and benchmark (challenge HW) analysis.

## Local Analysis

On my PC, it takes less than 5 seconds to process the 16GB file with 8/10 threads.

Let's compare `abouchez` with a solid multi-threaded entry using file buffer reads and no memory map (like `sbalazs`), using the `time` command on Linux:

```
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ time ./abouchez measurements.txt -t=10 >resmrel5.txt

real 0m4,216s
user 0m38,789s
sys  0m0,632s

ab@dev:~/dev/github/1brc-ObjectPascal/bin$ time ./sbalazs measurements.txt 20 >ressb6.txt

real 0m25,330s
user 6m44,853s
sys  0m31,167s
```
We used 20 threads for `sbalazs`, and 10 threads for `abouchez` because it was giving the best results for each program on our PC.

Apart from the obvious global "wall" time reduction (`real` numbers), the raw parsing and data gathering in the threads match the number of threads and the running time (`user` numbers), and no syscall is involved by `abouchez` thanks to the memory mapping of the whole file (`sys` numbers, which contain only memory page faults).

The `memmap()` feature makes the initial/cold `abouchez` call slower, because it needs to cache all measurements data from file into RAM (I have 32GB of RAM, so the whole data file will remain in memory, as on the benchmark hardware):
```
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ time ./abouchez measurements.txt -t=10 >resmrel4.txt

real 0m6,042s
user 0m53,699s
sys  0m2,941s
```
This is the expected behavior, and will be fine with the benchmark challenge, which ignores the min and max values during its 10 times run. So the first run will just warm up the file into memory.

On my Intel 13h gen processor with E-cores and P-cores, forcing thread to core affinity does not help:
```
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ ./abouchez measurements.txt -t=10 -v
Processing measurements.txt with 10 threads and affinity=false
result hash=8A6B746A,, result length=1139418, stations count=41343, valid utf8=1
done in 4.25s 3.6 GB/s
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ ./abouchez measurements.txt -t=10 -v -a
Processing measurements.txt with 10 threads and affinity=true
result hash=8A6B746A, result length=1139418, stations count=41343, valid utf8=1
done in 4.42s 3.5 GB/s
```
Affinity may help on Ryzen 9, because its Zen 3 architecture is made of identical 16 cores with 32 threads, not this Intel E/P cores mess. But we will validate that on real hardware - no premature guess!

The `-v` verbose mode makes such testing easy. The `hash` value can quickly check that the generated output is correct, and that it is valid `utf8` content (as expected).

## Benchmark Integration

Every system is quite unique, especially about its CPU multi-thread abilities. For instance, my Intel Core i5 has both P-cores and E-cores so its threading model is pretty unfair. The Zen architecture should be more balanced.

So we first need to find out which options leverage at best the hardware it runs on.

On the https://github.com/gcarreno/1brc-ObjectPascal challenge hardware, which is a Ryzen 9 5950x with 16 cores / 32 threads and 64MB of L3 cache, each thread using around 2.5MB of its own data, we should try several options with 16-24-32 threads, for instance:

```
time ./abouchez measurements.txt -v -t=8
time ./abouchez measurements.txt -v -t=16
time ./abouchez measurements.txt -v -t=24
time ./abouchez measurements.txt -v -t=32
time ./abouchez measurements.txt -v -t=16 -a
time ./abouchez measurements.txt -v -t=24 -a
time ./abouchez measurements.txt -v -t=32 -a
```
Please run those command lines, to guess which parameters are to be run for the benchmark, and would give the best results on the actual benchmark PC with its Ryzen 9 CPU. We will see if core affinity makes a difference here.

Then we could run:
```
time ./abouchez measurements.txt -v -t=1
```
This `-t=1` run is for fun: it will run the process in a single thread. It will help to guess how optimized (and lockfree) our parsing code is, and to validate the CPU multi-core abilities. In a perfect world, other `-t=##` runs should stand for a perfect division of `real` time per the number of working threads, and the `user` value reported by `time` should remain almost the same when we add threads up to the number of CPU cores.

## Feedback Needed

Here we will put some additional information, once our proposal has been run on the benchmark hardware.

Stay tuned!

## Ending Note

There is a "*pure mORMot*" name lookup version available if you undefine the `CUSTOMHASH` conditional, which is around 40% slower, because it needs to copy the name into the stack before using `TDynArrayHashed`, and has a little more overhead.

Arnaud :D