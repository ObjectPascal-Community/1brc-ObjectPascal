# Brian Fire

An Entry to the One Billion Row Challenge in Object Pascal using Delphi 12 by [EagleAglow](https://github.com/EagleAglow), Discord: briar.on.fire

## Compiler

**Delphi 12** Professional Edition

### Dependencies

Project uses Delphi System units: `Classes`, `SysUtils`, `StrUtils`, `Diagnostics`,
`Threading` and `SyncObjs`.

### Execution
```
    Usage
    bfire -h                       |  Write this help message and exit
    bfire -v                       |  Write the version and exit
    bfire -i <file_1>              |  <file_1> contains Weather Data
    bfire -i <file_1> -o <file_2>  |  <file_1> contains Weather Data
                                   |  <file_2> contains result
    If <file_2> is not defined, result goes to CONSOLE (STDOUT)
```

#### Contest Mode

To run the challenge, read from the 'challenge.csv' file:

```console
C:> bfire -i challenge.csv
```

## Remarks

I haven't used Delphi very much recently, really needed to work on this for a refresher.
I like TStringList self-sorting, but it is not as fast as other techniques.
Now that this entry is set up, I can play with improvements. Maybe even get a time under 15 minutes! :)

Second version uses hash of station name to accumulate data and fill a TStringList for Unicode station name.
The list is initially unsorted and has linked objects for records holding accumulated data for each station.
Finally, the TStringList is sorted and used to output sorted data.

Third version has a thread for the console (which waits for tabulation, then sorts and writes results),
two threads to read file, five threads to tabulate stations.  Stations are grouped into five separate stacks,
so each tabulation thread has roughly the same work load. File is read byte-wise into "classic" byte array
for each file line ending in ascii 10.  Each of these arrays is queued as a record in a last-in-first-out stack.
Tabulation threads split the data into station name and temperature, then hash station name and use hash
as index into one of five data arrays.  After all data is read and tabulated, the five data arrays are added to an
initially unsorted TStringList that holds unsorted Unicode station name and has linked pointers to
tabulated data for each station.  Finally, the TStringList is sorted, and the data is output.

## History

- Version 1.0: first working version, based on TStringList.
- Version 1.1: modified rounding to new baseline.
- Version 2.0: use hashing, sort later.
- Version 2.1: minor speed tweaks.
- Version 2.2: try hash functions modification.
- Version 3.0: Six threads: one to read, four to tabulate, one (console) to rule them all...
- Version 3.1: Safer locking strategy - didn't work.
- Version 3.2: Eigth threads: two to read, five to tabulate, one (console) to rule them all...

