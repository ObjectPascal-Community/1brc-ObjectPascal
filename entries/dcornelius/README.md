# David Cornelius

An Entry to the One Billion Row Challenge in Object Pascal using Delphi 12 Athens by [David Cornelius](https://github.com/corneliusdavid).

I wanted to see how different methods compared for ease of writing the code and speed of execution, so I solved this in three different ways:

- **TDictionary** - as each line is read, create an object and add it to a `TDictionary` collection; if an entry already exists for a city, update it instead of adding. This was simple to implement but since `TDictionary` doesn't have a sort method, after this list is built, another list must be created to sort them.
- **TStringList** - this is a really simple implementation but requires a lot of memory because the `LoadFromFile` method is used to read in all rows before processing them. Then a second TStringList is used to collate and sort the data. *NOTE: This method results in an immediate Range Check Error! The documentation on TStringList says it'll handle over a billion records--but the LoadFromFile function does not!!*
- **In-Memory Table** - another approach I thought I'd try was to load all the data into an in-memory table and use local SQL to query the data. This also takes a lot of memory and I didn't expect this to be the most efficient method (it actually turned out to be the slowest by far) but I did learn some cool things about FireDAC.

## Compiler

**Delphi 12 Athens** Enterprise Edition - which includes the ability to generate Linux console apps.

### Dependencies

There are no dependencies if run under one of the most recent versions of Delphi. The code should be backwards-compatible to Delphi 10.3 Rio (it uses inline variables and type inference introduced in that version) and further back with a few simple modifications. It uses `System.StrUtils`, `Generics.Collections`, and a few other run-time libraries in Delphi.

### Conditional Compilation

There are compiler directives to add some convenience when debugging. If built with the default *Debug* configuration, then the DEBUG compiler symbol is defined which turns on a few lines of code that give a little feedback and wait for Enter to be pressed so you can run this from the IDE without missing the quickly disappearing DOS box where the output is displayed.

Thus, build in *Release* mode for the official challenge. 

### Execution

The program runs identical between Win32, Win64, and Linux64. If you run it without exactly two parameters, it will display the syntax and exit. The parameters are:

- **Filename**: this is the input filename containing the measurements data as defined by the challenge.
- *Method*: this specifies which method to use when reading and collating the data and correspond to the methods as discussed above:
	- **DIC**: TDictionary
	- **TSL**: TStringList
	- **TBL**: In-Memory Table

#### Example

To run the challenge, read from the `measurements.txt` file, and use the TDictionary method, run it like this:

```DOS
$ obrc_docrnelius measurements.txt dic
```

## History

- Version 1.0: working version with `TDictionary`, `TStringList`, and `TFDMemTable` methods implemented.