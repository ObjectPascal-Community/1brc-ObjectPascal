# OneBRC Challenge in Pascal
#### entry by Georges Hatem


## About The Challenge
Given a file with 1 billion rows (~16 GB) each containing a weather station and a temperature measurement, the goal is to provide the fastest implementation to parse the file, process it and extract some statistics [*(details here)*](https://github.com/ObjectPascal-Community/1brc-ObjectPascal)

## Results Overview

This entry was ranked <b> </b3>3<sup>rd</sup> @ 2.10 seconds</b>, after:
- 1<sup>st</sup> place : 1.26 seconds
- 2<sup>nd</sup> place: 1.95 seconds

An un-optimized, single-threaded implementation can take <b>5 - 8 minutes\*</b>. [*(view all results)*](https://github.com/ObjectPascal-Community/1brc-ObjectPascal?tab=readme-ov-file#results)

<b>\*</b> *numbers may vary depending on the hardware used to run the tests.*

## Hardware + Environment
The below summarizes my development environment.  Specs of the PC used to benchmark the implementations can be found in the project's homepage.
 - Dell XPS 15 (9560, 2017)
 - OS: ArchLinux
 - CPU: Intel i7 7700 HQ (4 Cores, 8 Threads @ 2.80-3.80 GHz)
 - Cache:
  - L1: 64 KB per core
  - L2: 256 KB per core
  - L3: 6 MB shared
 - 32 GB RAM DDR4 (2400 MHz)
 - 1 TB SSD NVME

# Opening Notes

## Start with a Slow and Functioning Solution, Follow with Iterative Optimizations
It is tempting to delve into early optimizations. However, these often result in complex code that is hard to understand or debug - particularly so if the implementation is multi-threaded.
For this reason, in this submission, the goal was to first reach a correct result, irrespective of performance.  Optimizations would follow, one after another, each taking the program from one working state to another working state that is a little faster.

## Don’t Guess, Measure
When writing high-performance code, it is not enough to rely on assumptions of what *could theoretically* execute faster.  These assumptions are important nonetheless, as they often are at the root of alternative paths we choose to explore in trying to improve the speed of execution of our program.
By measuring the (sub-)program's execution time, one can verify that a certain implementation is faster (or not) than another.

When possible, such measurements are best done on the hardware on which the program is intended to run, although this is not always possible, as was the case in this challenge.

## Clean Code vs. High-Performance Code
There is a natural trade-off here: code generally considered readable and maintainable often uses higher-level abstractions and constructs to convey the code's meaning.  This can lead to un-optimized, slower code.  When chasing high-performance, bottlenecks will arise and resolving these will likely come at the cost of readability / maintainability.

## Measuring Execution Time

### Manual Measurements
In the early stages of the implementation, the program's execution was quite slow, around 8 minutes on my laptop.  Performance improvements were obvious to the naked eye.

To time a specific function, simply measure the time before it starts and after it ends, and compute the delta.  `TStopWatch` in Delphi, or similarly `GetTickCount` in FPC.

As the implementation matured however, this manual approach was no longer viable. It was time for performance profilers.

### Performance Profiler

The first thing to note about performance profilers is that they slow down the execution tremendously, as they measure the time taken by every line of code in the project.
To reduce the wait between each run, a smaller input file was used (100M rows instead of 1B).  Even then, a single profiling run would take ~15 minutes.

In this project, we used `Valgrind` and `Callgrind`, which output a text log showing, for each line of code, the percentage of total execution among other metrics.  Optionally, the output log can be viewed graphically using `KCachegrind`.

Methods marked with the `inline` directive will not be analyzed, so the inlining must be temporarily removed to profile the method's internals.

### Mean Execution Time

When measuring two implementations to pick the fastest, often times the results are inconsistent, making the decision process difficult.
The utility `hyperfine` was used to run N rounds to obtain a mean execution time.

# Structuring a First Working Solution

We break down the problem into 3 major steps:
1. read the input file
2. process it
3. output to file / stdout

Since this first version is trivial to implement, I will not linger on its details much:

#### 1. read the input file
A file stream is used to read the lines one at a time

#### 2. process the data
The line is parsed and aggregated into a dictionary that maintains for each station, its min, max, count and sum of the temperature measurements.

#### 3. output the solution
The dictionary keys (station names) are sorted in alphabetical order, and the output is printed in the required format.

#### key point
The key point here is that the reading / writing steps (1 and 3) are single-threaded by nature, and the intermediary processing (step 2) will benefit from some parallel programming.




# Optimizations

Moving forward, this section describes in more detail the various optimizations attempted, their impact on performance, their implication on the code, and the issues faced during implementation.

## Memory-Mapped File

Instead of reading the file line-by-line, the file can be memory-mapped if it fits into the RAM.  In our case, the file size is ~16 GB, and both my laptop and the target PC have 32 GB of RAM.

With `memmap`, the number of syscalls is reduced significantly, and performance is no longer disk-bound.  The entire memmap process takes ~0.1 seconds.  Since this implementation dragged with it many changes, I do not have exact time measurements for the original stream reading, but the overall improvement was substantial.

On the flipside, the program must now be compiled as a 64-bit binary in order to allocate so much RAM to one process (in Windows, the theoretical limit is ~2 GB, but practically, an out-of-memory exception is raised at around 1.5 GB.  I did not try the same in Linux, but apparently, the limit is ~3 GB).
Since memmap returns a pointer to a buffer (`pAnsiChar`) and a buffer length (`Int64`), reading lines is no longer done using plain string-types.

#### issues encountered
A first memmap attempt was to use `GetFileSizeEx` and `CreateFileMapping` directly, but `CreateFileMapping` accepts `Cardinal` type as buffer-size parameter, thus trying to pass a value of 16 Billion (16 B characters) of data would yield a `range check` error.  A painful approach would have been to invoke the procedure several times in chunks that would fit in a `Cardinal` type, but it was slow (10 seconds to read the file) as compared to alternative memmap implementations.

Credits to `synopse/mORMot2` library for providing the `memmap` function.

## Caching Efficiency: Temporal and Spacial Locality Principles

A few stats for each station need to be maintained in order to generate the expected result:

```
TStationData = packed record
    Min: SmallInt; // 16-bits, range = [-32768..32767], enough to store a temperature measurement between -100.0 and +100.0 (times 10 for integer representation)
    Max: SmallInt; // 16-bits
    Count: UInt32; // 32-bits, to fit the number of occurrences for a given station (some versions could fit in 16-bits). unsigned since count is positive
    Sum: Integer; // 32-bit signed
  end;
  PStationData = ^TStationData;
```

Each field in the record is no larger than it needs to be to store the expected data.  The record is also packed so its fields are not aligned to byte-boundaries, thus reducing its size further.
Since these records will be regularly accessed, the smaller each one is, the more can fit into cache.

Additionally, the records are stored in an array to benefit from contiguous memory allocation:
when reading a byte of data from memory, a block of 64 bytes (the 64 byte cache line) containing nearby station-records is loaded into cache.  Had the storage not been contiguous, random unnecessary bytes of data would be polluting the cache.

Being able to maximize the *useful* data that can fit in cache, closest to the CPU layer, increases the likelihood of cache-hits, as otherwise the data needs to be fetched from memory, orders of magnitude slower than the L1 / L2 caches.

An alternative test benchmark by @paweld ran the program on a few variations of the dataset.  Among these variations, 400 weather stations were used instead of the full ~42,000. In such a case, the `count` field could be reduced to a `UInt16`.

Two types of implementations are provided: `smallrec` using a `UInt16`, and `largerec` using a `UInt32`.
Surprisingly, the implementation with the smaller record size performed worse than its larger record counterpart.

## A Custom Dictionary Data-Structure

When parsing a line that consists of a weather station and a measurement, the new reading must be added to the station's record.
Locating a station's record-data was initially achieved using a `TDictionary<AnsiString, PStationData>`.
The dictionary's value is a pointer to the record, and not the record itself: as records are passed by value, it would require copying their contents in and out of the dictionary and into local variables, adding unnecessary overhead.
By following the pointer instead, the read/write will be done directly where the records are stored: the aforementioned contiguous array.

However, it turns out the dictionary lookups were still a bottleneck.  Trying various dictionary implementations showed no significant difference in performance measurements:
We then aim to improve the lookup by implementing a custom dictionary.

Here are the constraints we try to satisfy:
 - minimize the storage size of records: the array should be dense to maximize spacial locality.
 - minimize the number of collisions when hashing keys into the dictionary: collisions must be resolved using one of many collision resolution algorithms, so the fewer collisions, the less work is to be done to "try finding an available slot".

At a glance, there is a contradiction in the above 2 constraints:
On one hand, reducing collisions can be achieved by increasing the number of available storage slots. On the other hand, increasing the storage size results in sparse data, which we have described is cache-inefficient.

The solution turns out to be quite simple:
 - the `data_array` that stores records need not be larger than the number of stations: so given 42,000 stations, an array of only 42,000 records is needed.
 - the lookup is done separately, where the key is the station name, and the value is the **index** where the record is stored in the `data_array`.

The lookup dictionary now consists of two arrays, one for keys and the other for values.
Since the lookup value is an `Int16` (the largest index is ~42,000, smaller than 65,536), we can afford to increase the lookup array-sizes to minimize collisions.
The exact choice of size is mainly the result of measuring a few attempts and settling on one that yields the best numbers. Again, since we did not have access to the target hardware, it was picked based on trials on the available hardware.

In the end, we chose a size that is 6 times larger than the station count. The chosen number was also a prime number, as it seems to have improved under some selected hash functions (see next section)

## Improve the Dictionary’s Hash Function

As was previously mentioned, the data is now being read from a pointer to an array of `AnsiChar`s.  The station name must be recovered from a `pAnsiChar` and a length.
The procedure `SetString (..., startPtr, length)` will extract this string-value.

### Altering the Lookup Key

Though not very expensive, `SetString` had still become a bottleneck: in an attempt to find an alternative `key` for the dictionary, I encountered the  `crc` hash functions while navigating the docs.  An implementation of `crc32` was natively available, and it also expected the same parameters as `SetString`, that is, a `pAnsiChar` to the first character, and a length.

So now, the dictionary's lookup keys store a `Cardinal` (the `crc32` of our station name) instead of the station name itself.

**note:**
For the `crc32` hashing to work properly, it has to offer a 1-to-1 mapping between the station name and the resulting `Cardinal`: should there have been collisions where the `crc32` of two stations yield the same value, the resulting output would be incorrect.
Thus, this technique can only be applied if the input dataset is known in advance, and if it shows no collisions.

### Value Lookup
In order to lookup a value, a simple modulus is applied to the key. If the slot is available, use it.  Otherwise resolve the collision (see next section).

@abouchez of `synopse\mORMot2` pointed out that computing a modulus (which requires a division) is slow and suggested to look at `mORMot2` docs.  He also noted that `mORMot2` provides a faster, optimized implementation of `crc32` using SSE (Streaming SIMD Extensions for x86 architecture - whose details I know nothing about).

The docs mention an alternative hash function, `Lemire hash`, which proved to be faster, even though it resulted in a higher collision rate than `modulus` hashing.

## Improve the Dictionary’s Collision Resolution

By widening the key-space of the dictionary, the number of collisions was reduced significantly.  Even then, there is still an average of 8 collisions when trying to bucket a key in the key-space.
Several collision resolution algorithms were attempted, among which:
 - Linear probing
 - Quadratic probing
 - Double-hashing

In theory, quadratic probing and double-hashing are supposed to improve performance, as they reduce clustering.  Measurements showed that linear probing was faster in our case, likely due to the relatively low collision rate.

## Decode Temperature Strings Into Numerical Value

The initial implementation used `StrToFloat`, which proved to be very slow.  A first alternative, `Val`, was still slow, but definitely a step-up. `Val` with `SmallInt` types proved to be faster than with `Double`, even though the `.` had to be extracted prior to conversion.

Another alternative was to manually parse each character in the string, accumulating them into a resulting `Integer`.
The temperature format is one of `x.x`, `-x.x`, `xx.x` or `-xx.x`.
Apply `Ord` on the needed characters, offsetting by the ASCII value of `0`, and multiply by the correct power of 10.  Naturally, the 2<sup>nd</sup> right-most character (the period) is skipped.

The temperature is thus extracted as an Integer value, at a fraction of the cost:

```delphi
  aTemp :=     (Ord(FData[aEnd])   - c0ascii)
         + 10 *(Ord(FData[aEnd-2]) - c0ascii);
  vDigit := Ord(FData[aEnd-3]);
  if (vDigit >= c0ascii) and (vDigit <= c9ascii) then begin
    aTemp := aTemp + 100*(Ord(FData[aEnd-3]) - c0ascii);
    vDigit := Ord(FData[aEnd-4]);
    if vDigit = cNegAscii then
      aTemp := -aTemp;
  end
  else if vDigit = cNegAscii then
    aTemp := -aTemp;
```

This block of code will be subject to yet another optimization, discussed further down.

## Inlining Functions

Inlining a function instructs the compiler to try substituting the call to a function `F` by the actual code of `F`.
Inlining a function generates larger binaries, but also generally would improve its overall performance, as there is no need to push a function onto the execution stack, and pop it when the function is done executing.  In this program, since some functions are called 1 billion times, this cost can accumulate to be significant.

Some numbers for perspective:
According to `Valgrind`, one of the functions cost 23% of the total program execution time. **9%** of total time was spent executing `fpc_stackcheck`, which is implicitly executed before the function call, and verifies correctness of the stack frame.  Inlining the function seems to remove this cost (since the code is substituted).
Alternatively, if a method is purely written in `asm` (assembly), the directive `nostackframe` will indicate not to generate a stack frame, but I did not venture to this level.

There is a catch, however: in FreePascal at least, if method A calls method B, both of them cannot be marked as `inline`.
For this reason, some code had to be duplicated into both methods A and B, so as to benefit from optimal inlining.

## Unrolling Loops

Loop unrolling is another space-time tradeoff, which in this case we did manually, when knowing ahead of time that a certain loop will execute `N` number of times.
Unrolling generally aims to improve performance by avoiding the control loop's internal `goto` statement, as well as evaluating its end-of-loop condition.

As usual, the performance numbers will dictate whether it is worth unrolling a loop or not.

## Branchless Code

Reading about branchless code as another optimization technique, I thought the temp-decode optimization above could be reduced to a single IF-statement.  In fact, it was possible to remove all branching from this snippet of code, as seen below:
```
  vIsNeg := Ord (FData[J+1] <> '-');

  aTemp := (
                 (Ord(FData[aEnd])  - c0ascii)
           + 10 *(Ord(FData[aEnd-2]) - c0ascii)
           + Ord ((J+4 - vIsNeg < aEnd)) * 100*(Ord(FData[aEnd-3]) - c0ascii)
         ) * (vIsNeg * 2 - 1);
```

One thing to notice is that in some cases, a no-op (no-operation) will execute faster than putting it in a conditional statement.
This can be observed at line 7, where `vIsNeg` could be `0`, so multiplying it by `2` will have no impact.  Yet, branching an if-else statement turns out to be slower.

Reading further into this subject, I came across CPU's branch predictors, which tries to predict which of an `if` or `else` branch would execute.  The branching path that is predicted to be most likely taken is executed speculatively. If it turns out the guess was wrong, then the correct branch path is evaluated and executed again, discarding previously obtained results.

#### Predictable Branch

In the below snippet, the probability of encountering a new min / max value is very low, so the majority of the time, the if-branch will not execute.  The branch predictor can benefit from this information:
```
if vTemp < vData^.Min then
  vData^.Min := vTemp;
if vTemp > vData^.Max then
  vData^.Max := vTemp;
```

#### Unpredictable Branch

However, if it is not so clear which branch will be most likely to enter, the branch predictor will often times be wrong.  In such a case, branchless code as shown above is consistently faster than having if-else conditions.

## Pass Parameters By Address, Not By Value

Since the functions invoked in this loop are executed one billion times, passing parameters by value to these functions means a copy of those variables are defined in the function's stack frame.  This could be avoided by instead passing a reference to a variable:
- `const` parameters are passed by reference and cannot be modified
- `var` parameters are passed by reference, and modifications are maintained when the function is popped off the call stack.

## Skip Characters When Possible

Given the structure of a line, it is possible to guess how many characters can be skipped, so they are not processed at all.
We do this when looking for the line-terminating characters, as well as the semicolons separating the station name from its temperature measurement.

## Performance Observations

Over the several iterations that were run on the target machine, the results went down from 8 minutes, 3 minutes, 58 seconds and eventually around 32 seconds.  In order to go faster, multi-core CPU must be leveraged.

# Multi-Threading

At a high-level, given an input of size `N` (bytes, not rows) and `K` threads, the input N will be distributed across the K threads, and each thread will operate on the data that has been assigned to it:
**Data parallelism** is at play.

Since each thread can operate in complete independence from the other threads, the workload is **perfectly parallelizable**.

Only when all the threads are done with their respective work can their result be merged.

## Even Data Distribution

The simplest way to distribute data is for each processor `k = [0 -> K-1]` to process a subset of the data, starting at byte `k * (N / K)`, and of length `N / K`.
Careful consideration must be taken when handling the boundary between threads, because the boundary may likely occur in the middle of a line: in such a case, both threads handling this boundary will look for the newline character that is immediately after the boundary.

An incorrect boundary handling will result in certain data lines to be either double-processed, or not processed at all.

## Merging Results

When all threads have finished working on the data that was assigned to them, the individual threads' results can be aggregated.
This is a **K-way merge** operation, but for simplicity, we choose to merge two arrays at a time:

```
procedure TOneBRC.MergeAll;
var
  i: TThreadCount;
begin
  // all thread-data is accumulated into index 0
  for i := 1 to FThreadCount - 1 do begin
    Merge (0, i);
  end;
end;
```

From here onwards, generating the output from the merged array is no different from the single-threaded implementation.

# Optimizations For Multi-Threading

## Alterations to the Custom Dictionary

When operating with a single-thread, only one array was needed to accumulate stations' statistics. We could choose to keep a single array, but then, it becomes shared memory between threads and would need write-protected locking to avoid race conditions.

Alternatively, each thread would need a separate array to store its own results: more memory allocation probably means more cache misses (and in turn, slow memory access).

Trying both approaches with 2 threads only, operating with shared-memory and locking was already slower than maintaining a separate array per thread: this lock contention would only increase with the number of threads, forcing an unnecessary serialization between threads.

Avoiding shared memory was clearly a more favorable approach.

Nonetheless, we try to avoid storing more data than needed:
- the `crc32` keys are common to all threads, and so can be shared between them
- the station names are also common to all threads and need not be recomputed by every thread

For both these data, only the first occurrence needs to actually do the work.  And in case two threads happen to encounter a given station for the first time, **and** at the same time, the race condition is not really harmful:
both threads would recompute the same piece of data, both threads would write this same piece of data into the shared memory: at worst, the work was done by more than 1 thread, but the resulting data is healthy.

This race condition could be prevented by locking, but most of the time, the lock would just cause unnecessary serialization (read slowness).  It is worth remembering that there are only 42,000 weather stations, so given 1 billion rows, the probability of this race condition is negligible, let alone harmless.

## More Balanced Workload Distribution

A simple N / K distribution of data over threads works generally fine.  However, some cores may be busy running some processes from other applications or the operating system.

Does it mean all threads will finish at more-or-less the same time?
Because if they don't, some threads would be done with their workload and are passively waiting for others to finish.

Such a distribution is also based on the assumption that all cores are equally fast. This is no longer true for Intel's recent generations of CPUs, as these consist of Economy and Performance cores.

Instead, threads will process the data in relatively small blocks, and every time a thread finishes the block it was assigned, it is given the **next available block**.

The block distribution must be handled carefully, as threads still need to be wary about mid-line boundaries.  Additionally, two threads might be done with their block at the same time, and cause a race condition while requesting the next block.  As a result, the same block may end up being double-processed.

The race condition is resolved by protecting the block distribution with a `critical section`, so that no two threads may request a block concurrently.  Naturally, the code protected by the critical section must be kept as short as possible, to minimize contention between threads.

Such an implementation imposes another decision: what should be the size of those data-blocks?
Several values were attempted prior to picking one.

## Parallel Merge

As the number of threads increases, so does the number of results to be merged.
One potential optimization would have been to merge the results in parallel, but the execution was already quite fast that such an implementation might not have been significant.


# Closing Notes

At first, my goal was to reach a 10-seconds implementation, but when Gus informed me that my parallel implementation had reached 6 seconds, it encouraged me to try pushing the boundary further.

More time was probably spent trying to optimize these last few seconds than the rest of the implementation.  
Definitely more time than I would like to admit :)

Nonetheless, much was learned, and I leave the challenge with still many questions as to why certain optimization attempts did not yield any improvements.

In addition, one must decide for each such optimization, whether it is appropriate for a given project / product or team.
In the OneBRC challenge, there are no long-term implications, and we can optimize without restraint or compromise.

However, the same cannot be said for mature, long-lived, production-ready applications where maintainability is key: at this point, one must evaluate each optimization and decide whether it is worth the readability compromise or not.
At a glance, branchless code is definitely a no-go. Deduplication of code for optimal inlining is probably a second.

Of course, there is no universal truth to such decisions, and the choice must be made keeping in mind the project at stake, and the team responsible for its development.

Finally, I would like to thank @gcarreno for organizing this event, as well as @paweld and @abouchez for providing additional testing results and hints.
