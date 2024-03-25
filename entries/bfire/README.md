# Brian Fire

An Entry to the One Billion Row Challenge in Object Pascal using Delphi 12 by [EagleAglow](https://github.com/EagleAglow), Discord: briar.on.fire

## Compiler

**Delphi 12 ** Professional Edition

### Dependencies

Project uses Delphi units: 'Classes', 'System.SysUtils', 'System.StrUtils' and 'Math'.

### UTF8 vs. Windows Terminal

The text in the Windows Terminal console uses the system code page, which does not play well with UTF8.
The only way to match the approved result is to write the output to a file, with resulting
SHA256 hash: db3d79d31b50daa8c03a1e4f2025029cb137f9971aa04129d8bca004795ae524

If the Windows console output is redirected to a file, some characters are mangled, and the resulting SHA256 hash is:
82411ba76c59ae765e85b497f135a8f4e68d7a14cb7c0909ba96dea0d0635a28

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

```
C:> bfire -i challenge.csv
```

## Remarks

I haven't used Delphi very much recently, really needed to work on this for a refresher.
I like TStringList self-sorting, but it is not as fast as other techniques.
Now that this entry is set up, I can play with improvements. Maybe even get a time under 15 minutes! :)

## History

- Version 1.0: first working version.