# Brian Fire

An Entry to the One Billion Row Challenge in Object Pascal using Delphi 12 by [EagleAglow](https://github.com/EagleAglow), Discord: briar.on.fire

## Compiler

**Delphi 12** Professional Edition

### Dependencies

Project uses Delphi units: `Classes`, `System.SysUtils`, `System.StrUtils` and `Math`.

### UTF8 vs. Windows Terminal

The text in the Windows Terminal console uses the system code page, which does not play well with `UTF8`.
The only way to match the approved result is to write the output to a file, with resulting `SHA256` hash:\
`4256d19d3e134d79cc6f160d428a1d859ce961167bd01ca528daca8705163910`

If the Windows console output is redirected to a file, some characters are mangled, and the resulting `SHA256` hash is:\
`5c1942377034a69c7457f7cf671b5f8605df597ef18037c1baf4b9ead3c84678`

For the challenge, compiled for LINUX, the console result will (hopefully) be correct.

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
one thread to read file, four threads to tabulate stations (split by section of alphabet). File is read
byte-wise into "classic" byte arrays for station name and temperature. The arrays are passed to one of
four stacks, split by section of alphabet, for tabulation. Tabulation threads hash station name, use hash
as index into a data array.  After all data is read and tabulated, the four data arrays are added to an
initially unsorted TStringList that holds unsorted Unicode station name and has linked pointers to
tabulated data for each station.  Finally, the TStringList is sorted, and the data is output.

## History

- Version 1.0: First working version, based on TStringList.
- Version 1.1: Modified rounding to new baseline.
- Version 2.0: Use hashing, sort later.
- Version 2.1: Minor speed tweaks.
- Version 2.2: Try hash functions modification.
- Version 3.0: Six threads: one to read, four to tabulate, one (console) to rule them all...
- Version 3.1: Safer locking strategy
