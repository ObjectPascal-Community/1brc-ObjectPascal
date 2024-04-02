# Iwan Kelaiah

An Entry to the One Billion Row Challenge in Object Pascal.

The approach I implemented here is simplistic.

- Sequentially read the measurement file.
- Populate a `TDictionary` with station names, min, max, count and sum; without storing all the temperature measurements.
- Format output and sort in `TStringList`.
- Use a custom comparer to sort the station and temperature statistics in a `TStringList`.
- Use the rounding method as provided in the `baseline.lpr` (or the `README.md` from 1brc-ObjectPascal).
- Display the sorted measurements using a simple `for` loop.

## Getting Started

### Dependencies

* None. Only latest Free Pascal Compiler and Lazarus. You can get these easily by using [`https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases`](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases).

### Compiling

* Open `OneBRC.lpi` using Lazarus.
* Hit `Ctrl + F9` to compile.

### Running the executable

```bash
$ ./OneBRC.exe -i your_measurements.txt
```

To time the execution, do the following.

```bash
$ time ./OneBRC.exe -i your_measurements.txt
```

### Save an output

```bash
$ ./ikelaiah.exe -i measurements.txt > ikelaiah-output.txt
```

### Verifying SHA256 output on Windows

Launch `git bash` (make sure you have Git for Windows installed).

Run dos2unix on your output. The `\r\n` on Windows changes calculation of `sha256`.

```bash
$ dos2unix.exe ikelaiah-output.txt
dos2unix: converting file ikelaiah-output.txt to Unix format...
```

Run `sha256sum` on your output.

```bash
$ sha256sum.exe ikelaiah-output.txt
4256d19d3e134d79cc6f160d428a1d859ce961167bd01ca528daca8705163910 *ikelaiah-output.txt
```

## Help

To see flags, use `-h`.

```
$ ./OneBRC.exe -h
OneBRC -- An entry to the One Billion Row Challenge for Object Pascal

Usage: OneBRC [-h] [-v] [-i input_file]

  -h | --help                      Show this help screen
  -v | --version                   Show the version number
  -i | --input-file <filename>     Input text file to process.
                                   Each row is one temperature measurement in the format <string: station name>;<double: measurement>
```

Use `-v` to check version.

```bash
$ ./OneBRC.exe -v
OneBRC version 1.5
```

## Authors

Iwan Kelaiah
[ikelaiah](https://github.com/ikelaiah)

## Version History

* 1.0
    * Initial Release - Sequential approach. Approx. 18-20 mins on my Inspiron 15 7510 laptop.
    * `AssignFile` -> `Reset` -> Parse weather station and the recorded temperature with `TStringHelper.Split` ->  `TDictionary` -> `TStringList` -> A `for` loop -> output.

* 1.2
    * Revision release - Sequential approach. Approx. 15-18 mins on my Inspiron 15 7510 laptop.
    * Replaced `TStringHelper.Split` with `Pos()` and `Copy()`. 2-3 mins faster for 1 billion rows.
    * Float now stored as Int64.
    * Applied baseline's rounding.
    * 2-3 mins faster for 1 billion rows.

* 1.3
    * Revision release - Sequential approach. 12-15 mins on my Inspiron 15 7510 laptop.
    * Replaced `AssignFile()` and `Reset()` with `TFileStream` and `TStreamReader`. 
    * About 2mins 22sec to read 1 billion row.
    * 2-3 mins faster for 1 billion rows.

* 1.4
  * Revision release - Sequential approach. 6-8 mins on my Inspiron 15 7510 laptop.
  * Replaced `TFileStream` with `TBufferedFileStream`. Now, ~1 min faster. Total time for 1BRC (Object Pascal) is now approx. 11 mins on my laptop.
  * Replaced `TDictionary` with `TGHashMapQP`. Now ~4 mins faster. Total time for 1BRC (Object Pascal) is now approx. 7 mins on my laptop.
  * Replaced `StringReplace` with a simpler function avoiding creating a new string for each replacement. This saves 15-30 seconds on my laptop. The run time now is approx. 6-8 minutes.
  * Updated the rounding method as per the latest `README.md` in the 1BRC GitHub page.

* 1.5 
* Revision release - Sequential approach. 6-8 mins on my Inspiron 15 7510 laptop (No improvements on speed).
* Encapsulate process in a class.
* Updated the rounding method as per the latest `README.md` in the 1BRC GitHub page.

## License

This project is licensed under the MIT License - see the LICENSE.md file for details

The license of `LGenerics` is provided in the source code, in the `LGenerics` package.

## Acknowledgments

Inspiration, code snippets, libraries, etc.

 1. The FPC team, Lazarus team, fpcupdeluxe team, and other contributors.
      - For providing a usable programming language and a usable ecosystem.
 2. Gustavo 'Gus' Carreno.
      - For making this happen - 1BRC for Object Pascal.
      - Borrowed Gus' approach to use `TCustomApplication` and using `unit`s properly
        to make main code more readable.
      - Borrowed and modified Gus' `WriteHelp` from the `baseline.lpr`.
 3. A.Koverdyaev (avk)
      - For the amazing (LGenerics)[https://github.com/avk959/LGenerics] library.
 4. Székely Balázs.
      - Now I know what `Single` data type is!
      - I borrowed the custom `TStringList` comparer from the `baseline` program.
 5. Shraddha Agrawal - https://www.bytesizego.com/blog/one-billion-row-challenge-go.
      - The advice for not storing measurements for each station in a data structure.
 6. Arman Hajisafi - https://arman-hs.github.io
      - Encouragements and inspirations.
