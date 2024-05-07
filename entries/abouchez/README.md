# Arnaud Bouchez

**mORMot entry to The One Billion Row Challenge in Object Pascal.**

## mORMot 2 is Required

This entry requires the **mORMot 2** package to compile.

Download it from https://github.com/synopse/mORMot2

It is better to fork the current state of the *mORMot 2* repository, or get the latest release.

## Licence Terms

This code is licenced by its sole author (A. Bouchez) as MIT terms, to be used for pedagogical reasons.

I am very happy to share some server-side performance coding techniques using FPC on x86_64. ;)

## Do Not Guess, Measure

As old/legacy/outdated/potbellied/coffee-addicted FPC or Delphi code writers, we are still very influenced by the [FastCode](https://en.wikipedia.org/wiki/FastCode) project style of coding, or even some Turbo Pascal tricks - I know some good Delphi programmers still believing that `if length(s)=0 then` is faster than `if s='' then`.

Performance is not about ideology. Or magical tips and tricks to blindly apply. The main is idea is to measure, not guess. There is no "well-educated guess" in computer performance. Just a clock on the wall, and how many data you can process per second, in a sustainable way. Because modern CPUs are very complex beasts. Far away from the Z80 I started programming on, or even the 80386 our Delphi/FPC compilers are still emitting code for.

There are plenty of material about high-performance computing, on the Internet. But we have to embrace the fact that modern hardware is complex, and some techniques from 1980s - e.g. [Hacker's Delight](https://github.com/hcs0/Hackers-Delight?tab=readme-ov-file) - do not apply any more with our modern out-of-order and pipelined CPUs with very powerful ALU. For instance, today an integer multiplication takes one cycle, or compilers are able to automatically apply (or un-apply!) some of those techniques, depending on the target CPU.

Back to my blog and pascal still in the race of performance:
- [Pascal in the race: the TFB challenge](https://blog.synopse.info/?post/2023/10/31/Pascal-in-the-race%3A-TFB-Challenge-Benchmarks)
- [Modern Pascal in the race: CSV parsing](https://blog.synopse.info/?post/2022/11/26/Modern-Pascal-is-Still-in-the-Race)
- [Three locks to rule them all](https://blog.synopse.info/?post/2022/01/22/Three-Locks-To-Rule-Them-All)
- [mORMot on Ampere-aarch64 CPU](https://blog.synopse.info/?post/2021/08/17/mORMot-2-on-Ampere-AARM64-CPU)
- [From Delphi to AVX2](https://blog.synopse.info/?post/2020/11/04/EKON-24-Presentation-Slides)
- ... and eventually an upcoming article about *1brc-ObjectPascal* itself. :)

Recently, I found some very educational material available at:
- https://en.algorithmica.org/hpc/

The reference tables of opcodes per Intel/AMD CPU generation can be downloaded from:
- https://agner.org/optimize/

Amazing videos about modern compilers:
- https://www.youtube.com/watch?v=bSkpMdDe4g4
- https://www.youtube.com/watch?v=kIoZDUd5DKw

Reference implementations of the 1brc challenge in other languages:
- [the fastest Java version in the original repository](https://github.com/gunnarmorling/1brc/blob/main/src/main/java/dev/morling/onebrc/CalculateAverage_thomaswue.java);
- [a very sophisticated Rust implementation](https://github.com/RagnarGrootKoerkamp/1brc);
- [another crazy DotNet attempt](https://github.com/noahfalk/1brc/tree/main);
- [a quite readable C entry](https://github.com/lehuyduc/1brc-simd);
- [a good blog article with comparison of most known solutions](https://hotforknowledge.com/2024/01/13/1brc-in-dotnet-among-fastest-on-linux-my-optimization-journey/#results).

Note that those versions did not use the same input as we do in pascal: we use a "41K dataset" with 41343 station names, whereas they were optimized for 400 stations - see the last blog article.

In the compiler landscape, FPC is not as advanced/magical as gcc/llvm are, but it generates good enough code (on paar or better than Delphi's), and works is still done to enhance its output - e.g. by [Kit](https://www.patreon.com/curiouskit). I was amazed how good "pure pascal" code runs even on aarch64, like Ampere (see my blog article above) or on Apple M1/M2.

## Presentation

Here are the main ideas behind our proposals:

- **mORMot** makes cross-platform and cross-compiler support simple - e.g. `TDynArray`,`TTextWriter`, `SetThreadCpuAffinity`, `crc32c`, `ConsoleWrite` or command-line parsing;
- File is processed in parallel using several threads - configurable via the `-t=` switch, default being the total number of CPUs reported by the OS;
- Input is fed into each thread as 16MB chunks (see also the `-c` command line switch): because thread scheduling is unbalanced, it is inefficient to pre-divide the size of the whole input file into the number of threads, but we use an atomic counter of processed chunks;
- Our first attempt did map the entire 16GB file at once into memory using **mORMot** `TMemMap`: but we found out that the `munmap()` syscall is very slow on Linux for huge content, and was blocking the main thread for hunderths of milliseconds at shutdown; some Java or C entries do `fork` the program at startup (which sounds like cheating); so instead each thread will sub-map and sub-unmap its own 16MB chunks of data - and we directly call `fpmmap()` with the `MAP_POPULATE` flag to gain a few dozen ms;
- Each thread manages its own `Station[]` data and `StationHash[]` lookup table, so there is no lock until the thread is finished and data is consolidated;
- Each `Station[]` information is packed into a as-small-as-possible record, with no external pointer/string, to leverage the CPU L1 cache size (64 bytes) for efficiency;
- Since `crc32c` is a perfect hash function for our dataset, no name comparison nor storage is required during the process - [see below](#perfect-hash-trick);
- On Intel/AMD/AARCH64 CPUs, *mORMot* offers hardware SSE4.2 opcodes for this `crc32c` computation, and we wrote a dedicated `ParseLine()` assembly function on Linux x86_64 to compute the station name hash and parse the temperature value - see [below](#branches-may-be-evil);
- The hash table does not directly store the `Station[]` data, but use a `StationHash[]` lookup array of 16-bit indexes (as our `TDynArray` does) to leverage the CPU caches;
- Temperatures are stored as 16-bit integers for discrete values or 32-bit integers for the sum of values, as temperature multiplied by 10;
- Temperatures are parsed with a dedicated branchless code, expecting single decimal input values in `x.x` or `xx.x` patterns;
- The station names are stored either as UTF-8 pointers to the `memmap`ed location where they appear first, or as their own cache-friendly storage, to be emitted eventually for the final output, and are not used during temperature parsing;
- No memory allocation (e.g. no transient `string` or `TBytes`) is done during the parsing process to reduce contention and ensure the process is only CPU-bound and RAM-bound, and the only syscall are `mmap/munmap` as expected - we checked this with `strace` on Linux;
- Pascal code was tuned to generate the best possible asm output on FPC x86_64 (which is our target) - perhaps making it less readable, because we used pointer arithmetics when it matters (I like to think as such low-level pascal code as [portable assembly](https://sqlite.org/whyc.html#performance) similar to "unsafe" code in managed languages);
- We tried to share the name hash table between the threads: it was faster on Intel CPUs, but not on the benchmark AMD Zen 3 hardware, so [we eventually disabled it for this challenge](#why-l3-cache-matters);
- It can optionally output timing statistics and resultset hash value on the console to debug and refine settings (with the `-v` command line switch);
- It can optionally set each thread affinity to a single core (with the `-a` command line switch).

## Several Entries to Rule Them All

You will find in the [`src` sub-folder](./src) several implementations of the challenge. They propose several attempts, with diverse patterns.

The following table gives you an overview of these versions, in time implementation order. The first column gives direct access to each project source code:

project                                         | shared | full | nobranch | submap | 41K  | 400
----------------------------------------------- |:------:|:----:|:--------:|:------:| ----:| ----:
[`old`](./src/brcmormotold.lpr)                    |     | `X`  |          |        | 1350 | 1102
[`sharedht`](./src/brcmormotsharedht.lpr)          | `X` |      |          |        | 1135 |  923
[`fullcheck`](./src/brcmormotfullcheck.lpr)        |     | `X`  | `X`      | `X`    | 1261 |  760
[`perthreadht -a`](./src/brcmormotperthreadht.lpr) |     |      | `X`      | `X`    |  779 |  745
[*final* `-a`](./src/brcmormot.lpr)                | `X` |      | `X`      | `X`    |  817 |  738
[*final* `-f -a`](./src/brcmormot.lpr)             | `X` | `X`  | `X`      | `X`    | 1424 |  759

Here are the columns meaning:

- "project" matches the `brcmormot*.lpr` source file and `abouchez*` output executable name, optionally with command line switch(es) to activate some features or scales better - *final* is used for the final "official" version;
- "shared" indicates that the station names (and hash table) are not stored by each thread, but shared for all threads (it seems to please the CPU caches);
- "full" indicates that the full station name is checked, byte-per-byte, to detect any hash collision (not required by our Pascal challenge, but required by the original Java challenge) - so no `X` here states that the ["perfect hash trick"](#perfect-hash-trick) is used by this solution;
- "nobranch" indicates that the temperature parsing is using a branchless algorithm;
- "submap" indicates that `mmap()` is not called for the whole 16GB input file, but for each chunk in its own worker thread;
- "41K" and "400" are the time (in milliseconds) reported on OVH public cloud by `paweld` in [the "Alternative results" discussion thread](https://github.com/gcarreno/1brc-ObjectPascal/discussions/103#discussioncomment-9307532) for 41343 or 400 stations - so it is on AMD CPU, but not the "official" timing.

So we have a good coverage on what should be the best solution to propose.

Note that those timings differs from what I got on my own Intel Core i5 computer, on which the 400 stations version is much faster than the 41K stations, and the *final* project is always faster (by a noticeable margin). Seems to be because of diverse CPU cache sizes - [see below](#analysis).

## Why L1 Cache Matters

Taking special care of the "64 bytes cache line" does make a noticeable difference in performance. Even the fastest Java implementations of the 1brc challenge try to regroup the data in memory.

The L1 cache is well known in the performance hacking litterature to be the main bottleneck for any efficient in-memory process. If you want things to go fast, you should flatter your CPU L1 cache, e.g. with spatial and temporal locality, or avoiding concurrent multi-core access with triggers slow inter-cache synchronization.

Count and Sum values can fit in 32-bit `integer` fields (with a range of about 2 billions signed values), whereas min/max values have been reduced as 16-bit `smallint` - resulting in temperature range of -3276.7..+3276.8 celsius grads. It seems fair on our galaxy according to the IPCC. ;)

As a result, each `Station[]` entry takes as few bytes as possible, so we can fit exactly several entries in a single CPU L1 cache line. To be accurate, if we put some more data into the record (e.g. use `Int64` instead of `smallint`/`integer`), the performance degrades only for a few percents. The main fact seems to be that the entry is likely to fit into a single cache line, even if filling two cache lines may be sometimes needed for misaligned data.

In our first attempt (see "Old Version" below), we stored the name text into the `Station[]` array, so that each entry is 64 bytes long exactly. But since `crc32c` is a perfect hash function for our dataset, it is enough to just store the 32-bit hash instead, and keep the actual name on disk or on a shared dynamic array. Less data would mean less cache size involved.

We tried to remove the `StationHash[]` array of `word` lookup table, by using the main `Station[]` as hash slots - with a lot of void slots. It made one data read less, but performed several times slower. Data locality and cache pollution do prevail on absolute number of memory reads. It is faster to access twice the memory, if this memory could remain in the CPU caches. Only profiling and timing would show this. The shortest code is not the fastest on modern CPUs.

Note that if we reduce the number of stations from 41343 to 400 (as the original 1BRC project does), the performance is higher, also with a 16GB file as input. My guess is that since 400x16 = 6400, each dataset could fit entirely in each core L1 cache. No slower L2/L3 cache is involved, therefore performance is better.

Once again, some reference material is available at https://en.algorithmica.org/hpc/cpu-cache/
including some mind-blowing experiment [about cache associativity](https://en.algorithmica.org/hpc/cpu-cache/associativity/). I told you CPUs were complex! :D
Thanksfully, in our use case, data access is almost random, because... it was pseudo-randomly generated, so we should not suffer from cache associativity.

Anyway, the cache memory seems to be the bottleneck of our code. Which is a good sign, even if it may be difficult to make it any faster. But who knows? Any feedback is welcome!

## Why L3 Cache Matters

We tried to share the hash table between the threads, to reduce the cache pollution.

On our Intel computer, it was noticeably faster. But on the reference AMD computer used by this benchmark... it was not so obvious.

Theoretically speaking, it may come from cache size differences: on Zen 3 the cache is bigger, so the hash table can stay in cache without being shared. Whereas on our Intel CPUs, smaller cache means sharing the hash table has a benefit. Let's verify.

In fact, if we compare the two machines:

Ryzen 9 5950X caches are https://www.techpowerup.com/cpu-specs/ryzen-9-5950x.c2364
- L1 64KB per core, L2 512KB per core, L3 64MB shared

Intel core i5 13500 caches are https://www.techpowerup.com/cpu-specs/core-i5-13500.c2851
- P-cores: L1 80KB per core, L2 1.25MB per core, L3 24MB shared
- E-cores: L1 96KB per core, L2 2MB per module

So, L1/L2 cache seems to be too small for our process, on both CPUs. So the L3 cache seems to be the bottleneck. And L3 cache is bigger on the AMD, and all our data is likely to fit in it .
- Without sharing, the per-thread temperature data size is 16 bytes * 43413 stations * 32 threads = 21 MB.
- With sharing, the per-thread temperature data size is 12 bytes * 43413 stations * 32 threads = 16 MB.
- If we include the 16-bit jumpers, and cache pollution due to its huge size (there are void slots), we can easily add 4-8 MB of polluted cache, per hash table.

The work dataset seems to fit within the L3 cache size of 64MB of the Ryzen CPU, if we share the hash table or not. Whereas on our Intel versions, we are likely to saturate the L3 cache if we don't share the hash table.

My initial guess what that maintaining a hash table for each thread could be a good idea. And it seems it was on the Ryzen CPU, but not on my Intel CPU. Final numbers seem to be in favor of sharing names, by a small margin.

## Branches May Be Evil

Branches in code are conditional jumps, which are taken or not. The CPU is able to remember which branches were taken, and guess the most likely execution path.

For instance, the following code is fine to run:
```
      if v < s^.Min then // branches are fine
        s^.Min := v;
      if v > s^.Max then
        s^.Max := v;
```
Since new min/max values are likely to seldom appear on our dataset, the assignment branches won't be taken most of the time, so any modern CPU is able to make a good guess and those code lines will run just fine.

But if the branches are taken with no clear pattern, there is nothing to guess, and it is not optimal.

For instance, our first attempt of temperature parsing was efficient (converting the value directly from the memory buffer bytes), but not easy to guess by the CPU:
```
      // parse the temperature (as -12.3 -3.4 5.6 78.9 patterns) into value * 10
      if p[0] = ord('-') then
      begin
        neg := -1;
        p := @p[1];
      end
      else
        neg := 1;
      if p[2] = ord('.') then // xx.x
      begin
        // note: the PCardinal(p)^ + "shr and $ff" trick is actually slower
        v := (p[0] * 100 + p[1] * 10 + p[3] - (ord('0') * 111)) * neg;
        p := @p[6]; // also jump ending $13/$10
      end
      else
      begin
        v := (p[0] * 10 + p[2] - (ord('0') * 11)) * neg; // x.x
        p := @p[5];
      end;
```
Remember that our temperature data comes from a random source, so there is no pattern in its generated values. The above code has a first branch to handle the sign, then another branch to handle temperatures either as 'xx.x' or 'x.x' patterns. And since values are almost random, those two branches are likely to be taken 50% each in average. No easy guess. A lot of wrong assumptions. Performance loss.

We refactored this parsing code using low-level branchless arithmetic in our `ParseLine()` function, as such:
```
  neg := ord(p[1] <> '-') * 2 - 1;         // neg = +1 or -1
  inc(p, ord(p[1] = '-'));                 // ignore '-' sign
  chunk.Start := @p[ord(p[2] <> '.') + 6]; // next line
  chunk.Value := (cardinal((((PCardinal(p + 1)^ shl
                   (byte(ord(p[2] = '.') shl 3))) and $0f000f0f) *
         (1 + 10 shl 16 + 100 shl 24)) shr 24) and cardinal(1023)) * neg;
```
To understand this weird/unreadable/alien code, you need to know that `ord(boolean)` returns the `0` value for `false` and `1` for `true`, so we can use `ord(p[1] = '-')` to process the sign, and `ord(p[2] = '.'` to adapt to 'xx.x' or 'x.x' input patterns.

On modern CPUs, always executing a line - e.g. `inc(p, ord(p[1] = '-'));` which is either `inc(p, 0);` or `inc(p, 1);` - is actually faster than a executing a branch with fewer instructions, if the CPU can't guess if the branch would be taken or not. Yes, the weird `inc(p, 0);` execution path (which does nothing for sure) is faster than a branch!

To be fair, `chunk.Value` is a bit complex to understand, but we detailed its steps in the SSE2 asm code.

Because yes, we were not able to resist, and we eventually wrote some dedicated SSE2 + SSE4.2 parsing code, which seemed to be fairly efficient.

We used a similar branchless approach in our [From Delphi to AVX2](https://blog.synopse.info/?post/2020/11/04/EKON-24-Presentation-Slides) blog article.

## Perfect Hash Trick

The "perfect hash" trick was not allowed in the original Java challenge, for good reasons. We have made some versions with full name comparison, but they are noticeably slower, and [the Pascal challenge does not make such requirement](https://github.com/gcarreno/1brc-ObjectPascal/issues/118).

Our final implementation is safe with the official dataset, and gives the expected result - which was the goal of this challenge: compute the right data reduction with as little time as possible, with all possible hacks and tricks. A "perfect hash" is a well known hacking pattern, when the dataset is validated in advance. We can imagine that if a new weather station appear, we can check for any collision. And since our CPUs offers `crc32c` which is perfect for our dataset... let's use it! https://en.wikipedia.org/wiki/Perfect_hash_function ;)

But for a fair comparison with other languages (e.g. Java), you should better consider using only our versions featuring "full name" comparison.

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
  -f, --full          force full name lookup (disable "perfect hash" trick)
  -h, --help          display this help

Params:
  -t, --threads <number> (default 20)
                      number of threads to run
  -c, --chunk <megabytes> (default 8)
                      size in megabytes used for per-thread chunking
```
We can use these command-line switches for some local analysis.

## Analysis

Here are the actual numbers for 41K stations, on my local machine, with the latest current version of our code.

The `-v` command line switch shows some information with proper timing and parsing speed:
```
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ ./abouchez measurements.txt -v -a
Processing measurements.txt with 20 threads, 16MB chunks and affinity=0
result hash=85614446, result length=1139413, stations count=41343, valid utf8=1
done in 1.10s 14.1 GB/s
```
Which are pretty good numbers, especially thanks to our `ParseLine()` dedicated asm function using available SSE2 and SSE4.2 instructions. Our code is 100% branchless for 8..15 bytes of stations name length, which is a fairly huge proportion of the input. Also not abusing of `mmap` seems like a good idea too.

If we force the full station name lookup (with the `-f` switch) instead of "perfect hash" quick check:
```
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ ./abouchez measurements.txt -v -a -f
Processing measurements.txt with 20 threads, 16MB chunks and affinity=0
result hash=85614446, result length=1139413, stations count=41343, valid utf8=1
done in 1.96s 7.9 GB/s
```

The "pure pascal" version of the code is not so far away:
```
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ ./abouchez measurements.txt -v
Processing measurements.txt with 20 threads, 16MB chunks and affinity=0
result hash=85614446, result length=1139413, stations count=41343, valid utf8=1
done in 1.61s 9.7 GB/s
```
So writing branchless code, even in pascal, could benefit to performance.

If we try with the input file reduced to 400 stations instead of 41K, here are the numbers:
```
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ ./abouchez measurements400.txt -v -a
Processing measurements400.txt with 20 threads, 16MB chunks and affinity=1 full=0
hash=E1C57229, length=11024, stations count=400, valid utf8=1
done in 725.90ms 20.4 GB/s
```
You can see that the hash is not the same, and processing is much faster, thanks to a better CPU cache usage, due to the reduced number of stations to track at the same time.

## Preliminary Implementation Notes

Here are some notes taken during preliminary analysis of our solution.

Final numbers were actually more optimized, but the following paragraphs are kept in this file as reference/historical material.

### Local Analysis

On my PC, it takes less than 2 seconds to process the 16GB file. Numbers below were with the initial version of our code. Current trunk is faster. But the analysis is still accurate.

Let's compare `abouchez` with a solid multi-threaded entry using file buffer reads and no memory map (like `sbalazs`), using the `time` command on Linux:

```
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ time ./abouchez measurements.txt -t=20 >resmormot.txt

real 0m2,350s
user 0m40,165s
sys  0m0,888s

ab@dev:~/dev/github/1brc-ObjectPascal/bin$ time ./sbalazs measurements.txt 20 >ressb.txt

real 0m25,330s
user 6m44,853s
sys  0m31,167s
```
We defined 20 threads for both executables, because our PC CPU has 20 threads in total, and using them all seems to achieve the best resutls.

Apart from the obvious global "wall" time reduction (`real` numbers), the raw parsing and data gathering in the threads match the number of threads and the running time (`user` numbers), and less syscalls are involved by `abouchez` thanks to the memory mapping of the file (`sys` numbers are much lower).

The `memmap()` feature makes the initial/cold `abouchez` call slower, because it needs to cache all measurements data from file into RAM (I have 64GB of RAM, so the whole data file will remain in memory, as on the benchmark hardware):
```
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ time ./abouchez measurements.txt -t=20 >resmormot.txt

real 0m6,042s
user 0m53,699s
sys  0m2,941s
```
This is the expected behavior, and will be fine with the benchmark challenge, which ignores the min and max values during its 10 times run. So the first run will just warm up the file into memory.

On my Intel 13h gen processor with E-cores and P-cores, forcing thread to core affinity does not make any huge difference (we are within the error margin):
```
ab@dev:~/dev/github/1brc-ObjectPascal/bin$ ./abouchez measurements.txt -v
Processing measurements.txt with 20 threads, 4MB chunks and affinity=0
result hash=85614446, result length=1139418, stations count=41343, valid utf8=1
done in 2.36s 6.6 GB/s

ab@dev:~/dev/github/1brc-ObjectPascal/bin$ ./abouchez measurements.txt -v -a
Processing measurements.txt with 20 threads, 4MB chunks and affinity=1
result hash=85614446, result length=1139418, stations count=41343, valid utf8=1
done in 2.44s 6.4 GB/s
```
Affinity may help on Ryzen 9, because its Zen 3 architecture is made of identical 16 cores with 32 threads, not this Intel E/P cores mess. But we will validate that on real hardware - no premature guess!

The `-v` verbose mode makes such testing easy. The `hash` value can quickly check that the generated output is correct, and that it is valid `utf8` content (as expected).

### Benchmark Integration

Every system is quite unique, especially about its CPU multi-thread abilities. For instance, my Intel Core i5 has both P-cores and E-cores so its threading model is pretty unbalanced. The Zen architecture should be more balanced.

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

### Back To Reality

Our proposal has been run on the benchmark hardware, using the full automation. 

Here are some numbers, with 16 threads:
```
-- SSD --
Benchmark 1: abouchez
  Time (mean ± σ):      2.095 s ±  0.044 s    [User: 21.486 s, System: 1.752 s]
  Range (min … max):    2.017 s …  2.135 s    10 runs
``` 

With 24 threads:
```
-- SSD --
Benchmark 1: abouchez
  Time (mean ± σ):      1.944 s ±  0.014 s    [User: 28.686 s, System: 1.909 s]
  Range (min … max):    1.924 s …  1.974 s    10 runs
``` 

With 32 threads:
```
-- SSD --
Benchmark 1: abouchez
  Time (mean ± σ):      1.768 s ±  0.012 s    [User: 33.286 s, System: 2.067 s]
  Range (min … max):    1.743 s …  1.782 s    10 runs
```

If we try with 32 threads and thread affinity (`-a` option):
```
  Time (mean ± σ):      1.771 s ±  0.010 s    [User: 33.415 s, System: 2.056 s]
  Range (min … max):    1.760 s …  1.786 s    10 runs
 ```

So it sounds like if we could just run the benchmark with the `-t=32` option, and achieve the best performance. Thread affinity is no silver bullet here, so we better stay away from it, and let the OS decide about thread scheduling.

The Ryzen CPU has 16 cores with 32 threads, and it makes sense that each thread only have to manage a small number of data per item (a 16 bytes `Station[]` item), so we could leverage all cores and threads.

### Notes about the "Old" Version

In the version same `src` sub-folder, you will find our first attempt of this challenge, as `brcmormotold.lpr`. In respect to the "final/new" version, it did store the name as a "shortstring" within its `Station[]` record, to fill exactly the 64-byte cache line size.

It was already very fast, but since `crc32c` is a perfect hash function, we finally decided to just store the 32-bit hash, and not the name itself. Lately, we even tried to share the name hash table process for all threads - with no clear benefit.

You could disable our tuned asm in the project source code, and loose some performance by using general purpose *mORMot* `crc32c()` function, which already runs SSE4.2 tuned assembly. 

There is a "*pure mORMot*" name lookup version available if you undefine the `CUSTOMHASH` conditional, which is around 40% slower, because it needs to copy the name into the stack before using `TDynArrayHashed`, and has a little more overhead.

As reference, here are the numbers of this "old" version, with 30 threads (on a busy Benchmark system): 
```
-- SSD --
Benchmark 1: abouchez
  Time (mean ± σ):      3.634 s ±  0.099 s    [User: 86.580 s, System: 2.012 s]
  Range (min … max):    3.530 s …  3.834 s    10 runs
 
-- HDD --
Benchmark 1: abouchez
  Time (mean ± σ):      3.629 s ±  0.102 s    [User: 86.086 s, System: 2.008 s]
  Range (min … max):    3.497 s …  3.789 s    10 runs
```

In fact, only the SSD values matters. We can see that once the data is loaded from disk into the RAM cache, there is no difference with a `memmap` file on SSD and HDD. Linux is a great Operating System for sure.

With 24 threads: 
```
-- SSD --
Benchmark 1: abouchez
  Time (mean ± σ):      2.977 s ±  0.053 s    [User: 53.790 s, System: 1.881 s]
  Range (min … max):    2.905 s …  3.060 s    10 runs
```

With 16 threads: 
```
-- SSD --
Benchmark 1: abouchez
  Time (mean ± σ):      2.472 s ±  0.061 s    [User: 27.787 s, System: 1.720 s]
  Range (min … max):    2.386 s …  2.588 s    10 runs
```

With 16 threads and thread affinity (`-a` switch on command line): 
```
-- SSD --
Benchmark 1: abouchez
  Time (mean ± σ):      3.227 s ±  0.017 s    [User: 39.731 s, System: 1.875 s]
  Range (min … max):    3.206 s …  3.253 s    10 runs
```
It is a known fact from experiment that forcing thread affinity is not always good idea, and it is safer to let any modern Operating System do the threads scheduling to the CPU cores, because it has a much better knowledge of the actual system load and status. Even on a "fair" CPU architecture like AMD Zen. For a "pure CPU" process, affinity may help a very little. But for our "old" process working outside of the L1 cache limits, we better let the OS decide.

So with this "old" version, it was decided to use `-t=16`. The "old" version is using a whole cache line (64 bytes) for its `Station[]` record, so it may be the responsible of using too much CPU cache, so more than 16 threads does not make a difference with it. Whereas our "new" version, with its `Station[]` of only 16 bytes, could use `-t=32` with benefits. The cache memory access is likely to be the bottleneck from now on.

Arnaud :D
