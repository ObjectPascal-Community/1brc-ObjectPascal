# Richard Lawson

An Entry to the One Billion Row Challenge in Object Pascal.

Approach.

- Sequentially read the measurement file, splitting into chunks of 64k.
- Populate a `TFPHashList` with station names, min, max, count and sum; without storing all the temperature measurements.
- Use a custom comparer to sort the station and temperature statistics in a `TStringList`.
- Display the sorted measurements using a simple for loop.

## Getting Started

### Dependencies

* None. Only latest Free Pascal Compiler and Lazarus. You can get these easily by using [`https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases`](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases).

### Compiling

* Open `weather.lpi` using Lazarus. 
* Hit `Ctrl + F9` to compile.

### Running the executable

```bash
$ ./weather -i your_measurements.txt
```

To time the execution, do the following.

```bash
$ time ./weather -i your_measurements.txt
```

## Authors

Richard Lawson
[lawson89](https://github.com/lawson89)

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
      - I borrowed the custom `TStringList` comparer from the `baseline` program.
 4. Iwan Kelaiah.
      - I borrowed the README.md and output generation code.
