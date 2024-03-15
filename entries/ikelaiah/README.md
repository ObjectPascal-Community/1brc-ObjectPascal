# OneBRC for Object Pascal

An Entry to the One Billion Row Challenge in Object Pascal.

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
OneBRC version 1.0
```

## Authors

Iwan Kelaiah
[ikelaiah](https://github.com/ikelaiah)

## Version History

* 1.0
    * Initial Release - Sequential approach.

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