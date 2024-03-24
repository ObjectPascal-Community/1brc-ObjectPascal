# EagleAglow

An Entry to the One Billion Row Challenge in Object Pascal using Delphi 12 by [EagleAglow](https://github.com/EagleAglow), Discord: briar.on.fire

## Compiler

**Delphi 12 ** Professional Edition

### Dependencies

Project uses Delphi units: 'Classes', 'System.SysUtils', 'System.StrUtils' and 'Math'.

### UTF8 vs. Windows Terminal

The text in the Windows Terminal console uses the system code page, which does not play well with UTF8.
Although I tried various ways to make this work, the only reliable approach was to write the results to a file.
For the challenge, compiling in DEBUG mode suppresses console output.

### Execution
```
    Usage
    OneBRC -h                      |  Write this help message and exit
    OneBRC -v                      |  Write the version and exit
    OneBRC -i <file_1> -o <file_2> |  <file_1> contains Weather Data
                                   |  <file_2> contains result
```

#### Contest Mode

To run the challenge, compile a DEBUG version, read from the 'challenge.csv' file and write to 'results.txt' file:

```
C:> OneBRC -i challenge.csv -o results.txt
```

## Remarks

I haven't used Delphi very much recently, really needed to work on this for a refresher.
I like TStringList self-sorting, but it is not as fast as other techniques.
Now that this entry is set up, I can play with improvements. Maybe even get a time under 15 minutes! :)

## History

- Version 1.0: first working version.