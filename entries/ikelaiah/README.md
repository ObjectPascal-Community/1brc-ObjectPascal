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
db3d79d31b50daa8c03a1e4f2025029cb137f9971aa04129d8bca004795ae524 *ikelaiah-output.txt
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
OneBRC version 1.3
```

## Authors

Iwan Kelaiah
[ikelaiah](https://github.com/ikelaiah)

## Version History

* 1.0
    * Initial Release - Sequential approach. 
    * `AssignFile` -> `Reset` -> Parse weather station and the recorded temperature with `TStringHelper.Split` ->  `TDictionary` -> `TStringList` -> A `for` loop -> output.

* 1.2
    * Revision release - Sequential approach. 
    * Replaced `TStringHelper.Split` with `Pos()` and `Copy()`. 2-3 mins faster for 1 billion rows.
    * Float now stored as Int64. 2-3 mins faster for 1 billion rows.
    * Applied baseline's rounding.

* 1.3
    * Revision release - Sequential approach.
    * Replaced `AssignFile()` and `Reset()` with `TfileStream` and `TStreamReader`. 3-4 mins faster for 1 billion rows.

## License

This project is licensed under the MIT License - see the LICENSE.md file for details

## Acknowledgments

Inspiration, code snippets, etc.

 1. The FPC team, Lazarus team, fpcupdeluxe team, and other contributors.
      - For providing a usable programming language and a usable ecosystem.
 2. Gustavo 'Gus' Carreno.
      - For making this happen.
      - Borrowed Gus' approach to use `TCustomApplication` and using `unit`s properly
        to make main code more readable.
      - Borrowed and modified Gus' `WriteHelp` from the `baseline.lpr`.
 3. Székely Balázs.
      - Now I know what `Single` data type is!
      - I borrowed the custom `TStringList` comparer from the `baseline` program.
 4. Shraddha Agrawal - https://www.bytesizego.com/blog/one-billion-row-challenge-go.
      - The advice for not storing measurements for each station in a data structure.
 5. Arman Hajisafi - https://arman-hs.github.io
      - Encouragements and inspirations.