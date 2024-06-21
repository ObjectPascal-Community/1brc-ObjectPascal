# Georges Hatem

## Requirements
 - mORMot2 library (for the `MemMap` and `crc32c` functions)
 - 64-bit compilation

## Usage
 - -t flag to specify the thread-count (default reads the thread-count available on the CPU)

currently there are 2 versions that can be compiled / run:
 - `OneBRC.lpr                -> ghatem           `: compact record, optimal for the 1B row / 41k stations, will fail on the other tests due to overflow
 - `OneBRC-largerec.lpr       -> ghatem-largerec  `: same as OneBRC, but the StationData's "count" is UInt32 instead of 16. Passes all the tests
 
 - `OneBRC-parts.lpr          -> ghatem-parts     `: compact record, processes in parts as attempt stability, optimal for the 1B row / 41k stations, will fail on the other tests due to overflow
 - `OneBRC-parts-largerec.lpr -> ghatem-partslarge`: same as OneBRC-parts, but the StationData's "count" is UInt32 instead of 16. Passes all the tests

## Hardware + Environment
host: 
 - Dell XPS 15 (9560, 2017)
 - OS: ArchLinux
 - CPU: Intel i7 7700 HQ (4 Cores, 8 Threads @2.80-3.80 GHz, Kaby Lake)
 - 32 GB RAM DDR4 (2400 MHz)
 - 1 TB SSD NVME
 
VM (VirtualBox):
 - OS: Windows
 - CPU count: 4 out of 8 (threads, probably)
 - 20 GB RAM
 
the initial implementation (the Delphi baseline found in /baseline) aimed to get a correct output, regardless of performance: 
"Make it work, then make it work better".
It turns out even the baseline caused some trouble, namely the `Ceil` implementation was yielding different results between FPC and Delphi (and different results between Delphi Win32/Win64).
After the input of several peers including gcarreno, abouchez and paweld (thanks!), this last detail was ironed out, and the baseline yielded a matching hash.

## Single-Threaded Attempt (2024-04-03)
 
in this first attempt, the implementation is broken down into 3 major steps:
 1. read the input file
 2. process it
 3. output to file/stdout
 
a key point: 
 - the reading / writing (steps 1 an 3) will be done on the main thread.
 - the processing (step 2) is where a future submission will attempt to parallelize the work done.
 
### 1. Read The Input File

#### v1. File Stream
In the baseline implementation, a file stream is used to read line by line, which is far from optimal.

#### v2. Memory-Mapped File
An improvement was to read the whole file into memory in one shot, using memory mapping. 
In this implementation, I use `GetFileSize` and `CreateFileMapping`, procedure found online (need to find URL).  
First thing to note is, the usable memory by a Win32 process is limited to ~1.5-2 GB of RAM. Exceeding this limit yields an out-of-memory exception, so we must compile for Win64.
Some issues with this implementation (see unit FileReader.pas): 
 - `GetFileSize` was returning a size of ~3.9B, while we know the real input is ~16B.
 - `CreateFileMapping` was taking 2.5 seconds to read the file into a `TArray<Utf8Char>` the first time, and subsequent reads were down to 1.7 seconds.  But this is for only a quarter of the input size.
 - using `GetFileSizeEx` instead, we now get the real file size, ~16B
 - however, `CreateFileMapping` accepts as parameters `Cardinal` types, so a value of ~16B (Int64) would yield a `range check` error.
 - if we wanted to move forward with this implementation, we would need to call `CreateFileMapping` in 4 or 5 batches, which would take 1.7 x 5 ~= 8.5 seconds just to read the data.
 - attempt aborted, see v3.
 
#### v3. Memory-Mapped File, Provided by `mORMot2`
A v3 attempt at reading the file was using a ready-made implementation of file memory-mapping, provided by synopse/mORMot, big thanks @abouchez!
The function returns a pAnsiChar and the size as Int64 of the mapped data.  Performance-wise, it all happens in under 0.1 seconds, but now we must delve into pointers.


### 2. Process the File

Well, at a glance this is straightforward:
 - look for new-line chars to delimit each line, split it to extract StationName / Temperature.
 - Decode the temperature into a numerical value (double, or integer x 10)
 - accumulate the information into a dictionary of StationName -> Record of data
 
A few optimizations done here, to the best of my knowledge:
  
#### For Each line, Iterate Backwards
`Length(StationName) > Length(Temperature)`, so for each line, better look for the `;` starting from the end.
Given the below input:
```
Rock Hill;-54.3
            ^^^
```
the 3 last characters will be mandatorily present, so we can skip them while iterating.
I tried unrolling the loop over the last 2-3 characters that must be checked, but measuring it, it showed to be slower, don't know why.

#### Extract the Strings Using `SetString`
manual string concatenation and splitting proved to be very slow.
Using `SetString` yielded a noticeable improvement. Remaining in the realm of pointers would probably be much faster, but I haven't ventured there (yet, maybe if time is available).

#### Decode Temperature Into Numerical Value
First attempt was to use `StrToFloat`, which was pretty catastrophic. Using `Val` was still slow, but definitely a step-up. `Val` with SmallInt proved to be faster than with Double, even though there's extra operations to be done.
So now we need to get rid of the `.` character.

Again, string functions being very slow, replicating the last character at length-1, and then reduce the length of the AnsiString using `SetLength` yielded faster results. I tried doing so with pAnsiChar but got some range-check errors.  Might get back to it later on.

Finally, assuming temperatures in the range `-100` and `+100`, with 1 decimal figure, there should be 2000 different temperatures possible.
Instead of decoding the same temperature values using `Val`, do it once, store it in a TDictionary (TemperatureString -> TemperatureSmallInt). There were I believe 1998 different temperature values, so we only call `Val` 1998 times instead of 1 billion times.  Over an input size of 100M, the gain was 4-5 seconds (total 28s -> 23s)

#### Accumulate Data Into a Dictionary of Records
 - the records are packed, with minimal size
 - the dictionary maps StationName -> Pointer to record, to avoid passing around full records
 - records are pre-allocated in an array of 45,000, instead of allocating them on-the-fly.
 - when a station is not found in the dictionary, we point to the next element in the records-array.
 - with an input of size 100M, this accumulation step takes a considerable amount of time (9 seconds out of 23 total). I haven't identified yet if it is the `dict.Add` that takes time, the `dict.TryGetValue`, or just generally the dictionary hash collisions.  Even though the dictionary is pre-allocated with a capacity of 45,000, but that did not seem to improve much. I also tried the dictionary implementation of Spring4D, but also no improvements.
 
### 3. Output the Results
Since I started using pointers (pAnsiChar), getting a matching hash was a bit of a pickle:
Some Unicode characters were messed up in their display, or messed up in their ordering.
Eventually, the ordering issue was resolved by using `AnsiStrings.CompareStr` instead of `SysUtils.CompareStr`. This step will clearly remain single-threaded, but takes 0.15 seconds for all 45,000 stations, so it is not a big deal.

### Conclusion

If there are any improvements to be done in this single-threaded version, they would be in the following order, from most impactful to least impactful (performance numbers over an input of 100M, not 1B):
 - try improve storing / retrieval from the dictionary of station data, cost: 9 sec / 23 sec
 - try improve the extraction of string data, maybe using pointers (6.5 sec / 23 sec)
 - try improve the type conversion, though not sure how at this point (4.5 sec / 23 sec)
 - somehow, incrementing an integer 1B times is taking 1.2 seconds, while incrementing the main input index (16B times) is only taking 0.5 second.  It's just 1.2 seconds, but I'm not understanding why it should behave that way. 


## Single-Threaded Attempt (2024-04-09)

Turns out mORMot2 does not compile under Delphi for Linux x64 due to poor Delphi support for Linux.
This implementation was migrated as-is with Lazarus (FPC), which is much slower than Delphi on Windows x64.

**Timing as per gcarreno: 176 seconds, single-threaded**

Since then, a few improvements:

### better temperature conversion to SmallInt

In the previous implementation, we identified `Val` and `StrToFloat` as a source of slowness.  @abouchez suggested that the numerical value could be obtained without those.
Indeed, by using Ord of the needed characters, offsetting by the ASCII value of '0', and multiplying by the right power of 10, the temperature can be extracted as integer at a fraction of the cost.

### better usage of dictionary data-structure:

Instead of extracting the station name as a string 1B times, and use it as a dictionary key, instead use the FPC hashing function to reduce the string extraction to ~45k times (once per station), and otherwise use the pointer + length (station_name) to compute the hash.
This requires us to migrate from TFPHashList to the generic TDictionary. Even though TDictionary is slower than TFPHashList, the overall improvements yielded a significant performance gain.
Using TDictionary, the code is more similar to my Delphi version, in case we get to run those tests on a Windows PC.

*expected timing: ~60 seconds, single-threaded*

**ACTUAL TIMING: 58 seconds as per gcarreno**


## Multi-Threaded Attempt (2024-04-10)

In a first attempt, using 2 threads, I evaluated how harmful were my "shared-memory" variables, namely:
 - the pre-allocated records array
 - its counter
 - the dictionary of aggregated data
 - the stringlist of station names

Using a Critical-Section, even on just the stringlist, the performance drops quite a bit.

### minimize shared-memory

I got rid of the pre-allocated records array and its counter, back to on-the-fly allocation of records
Got rid of the station names stringlist, because merging those lists at the end was very slow: instead, store the station name directly in the record.
Replaced the singular dictionary with an array of dictionaries, 1-per-thread.

### redundant work

All of this causes some redundant work to be made by various threads:
 - one record is allocated per-station-per-thread
 - since we store the station name in the record, we convert it to string (using `SetString`) once per-station-per-thread

### parallelism

Given N bytes and K threads, a basic attempt is to distribute a range `N / K` of data per thread.
A thread may get its start/end boundaries in the middle of a line: we ensure that each line is processed exactly once.

Again, just to evaluate how things will go, we wait for all threads to complete before proceeding.

### merging

Given 2 dictionaries L and R, we merge them into L. This merge is applied K-1 times, and dictionaries[0] will contain the aggregated result.
We could later consider merging every 2 threads that have completed their work.

### expectations

parallelization on my environment (see above) performed quite poorly, considering there is no shared-mem to protect against concurrent access:
4 threads were barely getting 60% improvement over 1 thread.

is it due to:
 - VM virtualization?
 - too many dictionaries were causing too many cache misses?
 - work-load distributed unevenly?

Better wait and see the results on the real environment, before judging.

### results

**ACTUAL TIMING: 6.042 seconds as per gcarreno**

Due to the unexpectedly slow performance on Craig Chapman's powerful computer, and since the results above intrigued me, I have ported my FPC code onto Delphi to be able to compare the output of both compilers on Windows x64.
Hopefully, it will help identify is the issue stems from Windows x64 or FPC, in multi-threaded implementations.

## Multi-Threaded attempt v.2 (2024-04-16)

On my Linux setup (FPC, no VM), I realize that as the number of cores increases, the performance improvement is far from linear:

- 1 core: 77 seconds
- 2 cores: 50 seconds
- 4 cores: 35 seconds

In order to identify the source of the problem, I ﬁrst forced all threads to write to un-protected shared-memory (the results are wrong, of course). The idea is to try to understand if the source of the problem stems from ~45k records being created for each thread, and if the retrieval of those records are causing too many cache misses.

with 4 cores, we're now at ~21 seconds, which is much closer to linear performance. In order to make sure the threads are approximately getting a balanced load, each thread is initially assigned a range of data, and as soon as they are done with their given range, they request the next available range. I've tried with varying range sizes, but the result was always slower than just a plain `N / K` distribution.

So the problem (on my computer at least) does not seem to be related to the load imbalance between threads, but rather having to swap all those records from the cache.

As a last attempt, I tried again accumulating data in a shared memory, protecting all data accumulation with `InterlockedInc`, `InterlockedExchangeAdd`, and `TCriticalSection`. In order to avoid too many contentions on the critical section, I also tried to maintain a large array of critical sections, acquiring only the index for which we are accumulating data. All of these attempts under-performed on 4 threads, and likely will perform even worse as thread-count increases. The only way this would work is by having ﬁner-grained control over the locking, such that a thread would only be blocked if it tried to write into a record that is already locked.

Lastly, the `TDictionary.TryGetValue` has shown to be quite costly, around `1/4th` of the total cost. And although it is currently so much better than when using the station name as key, evaluating the `mod` of all those hashes, there is a lot of collisions. So if the dictionary key-storage is implemented as an array, and `mod` is used to transform those `CRC32` into indexes ranging in `[0, 45k]`, those collisions will be the cause of slowness. If there is a way to reduce the number of collisions, then maybe a custom dictionary implementation might help.


## Multi-Threaded attempt v.3 (2024-04-21)

Using performance profiler ValGrind, it identified that:
 - 30% of the time was spent on `TryGetValue` of the generic `TDictionary`.
 - 14% of the time is on computing the crc32 hash
 - 15% of the time on extracting the line data
 - surprisingly, 9% of the time is spent on looking for the #13 (new-line) character

I implemented my own Dictionary class consisting of two arrays. We compute the modulus of the incoming key (Cardinal) to fit it in the correct bucket. A first attempt at collision resolution was to store as values a TList, but performance was worse than the generic TDictionary.  Next attempt was a linear probing, with circular indexing in case the index goes out of bounds. Performance improved from 35s to 30s.  Will later try quadratic probing, as it apparently reduces clustering.

edit:
quadratic probing improved performance even further. we could probably do better with 2-level hashing, but finding such a hash function is going to take a lot of trials, this is probably acceptable results

**ACTUAL TIMING (busy machine): ~4 seconds as per gcarreno**

## v.4 (2024-04-24)

a few performance improvements, and measurements as per gcarreno on a busy machine:
 - using mORMot's `crc32c` function instead of the native `crc32`, time dropped to 3.8 seconds
 - I had removed my pre-allocated records implementation. restored it in the custom dictionary class, time dropped to 3.2 seconds
 - skipping a few chars that we don't need to bother with, no timing yet

## v.5 (2024-04-27)

Various attempts at dictionary sizes, ranging from 45k to 95k. Even though larger dictionaries reduce collision tremendously, a dictionary of size 45k was still optimal.

Another trial with various hash functions, a simple modulus vs. a slightly more complex one: modulus is slower on my PC, remains to try on the test env.
Can be tested with the HASHMULT build option

Finally, it seems choosing a dictionary size that is a prime number is also recommended: shaves 1 second out of 20 on my PC.

## v.6 (2024-05-04)

As of the latest results executed by Paweld, there are two main bottlenecks throttling the entire implementation, according to CallGrind and KCacheGrind:
 - function ExtractLineData, 23% of total cost, of which 9% is due to `fpc_stackcheck`
 - the hash lookup function, at 40% of total cost

Currently, the hash lookup is done on an array of records. Increasing the array size causes slowness, and reducing it causes further collisions.
Will try to see how to reduce collisions (increase array size), all while minimizing the cost of cache misses.

Edit:
The goal is to both:
 - minimize collisions on the hashes (keys) by having a good hash function, but also increase the size of the keys storage
 - minimize the size of the array of packed records

The idea:
 - the dictionary will no longer point to a PStationData pointer, but rather to an index between 0 and StationCount, where the record is stored in the array.
 - -> data about the same station will be stored at the same index for all threads' data-arrays
 - -> names will also be stored at that same index upon first encounter, and is common to all threads
 - no locking needs to occur when the key is already found, since there is no multiple-write occurring
 - the data-arrays are pre-allocated, and an atomic-counter will be incremented to know where the next element will be stored.

Thinking again, this is likely similar to the approach mentioned by @synopse in one of his comments.

For the ExtractLineData, three ideas to try implementing:
 - avoid using a function, to get rid of the cost of stack checking
 - reduce branching, I think it should be possible to go from 3 if-statements, to only 1
 - unroll the loop (although I had tried this in the past, did not show any improvements)

Edit 2: 
 - was unable to get rid of the stack_check: removing the function somehow became more expensive, I don't understand why that is.
 - I was able to reduce branching to zero in the parsing of temperature
 - unroll the loop was also beneficial, and even more so when the inner if-statement was removed in favor of branchless
 - dictionary improvements were successful and showed a 30% speedup
