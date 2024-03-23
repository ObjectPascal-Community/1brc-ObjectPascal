# David Cornelius

An Entry to the One Billion Row Challenge in Object Pascal using Delphi 12 Athens by [David Cornelius](https://github.com/corneliusdavid).

I wanted to see how different methods compared for ease of writing the code and speed of execution, so I solved this in three different ways:

- **TDictionary** - as each line is read, create an object and add it to a `TDictionary` collection; if an entry already exists for a city, update it instead of adding. This was simple to implement but since `TDictionary` doesn't have a sort method, after this list is built, another list must be created to sort them.
- **TStringList** - this is a really simple implementation but requires a lot of memory because the `LoadFromFile` method is used to read in all rows before processing them. Then a second TStringList is used to collate and sort the data. *NOTE: Using LoadFromFile resulted in an immediate Range Check Error when trying to read in the 1-billion line file! The default Stream created in `LoadFromFile` was the problem. When I switched to LoadFromStream and created my own Stream, it worked. However, it it's not near as fast as the `TDictionary` version.*
- **In-Memory Table** - another approach I thought I'd try was to load all the data into an in-memory table and use local SQL to query the data. While I did learn some cool things about FireDAC, I also learned that this is by far, the most *inefficient* approach for this: after running for 26 *HOURS*, I killed the process! 

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
	- **TSL**: TStringList; NOT an official entry--very slow.
	- **TBL**: In-Memory Table; NOT an official entry, only test with *small* data sets

#### Example

To run the challenge, read from the `measurements.txt` file, and use the TDictionary method, run it like this:

```
C:> docrnelius measurements.txt dic
```

## Remarks

I entered this challenge as a learning experience. I did not expect to be the fastest as I don't have time to implement multiple threads (which is clearly the road to victory here) but I had fun and learned a lot! 

I now know more about how buffer size can affect stream reads significantly. I have not used streams much in Delphi before but after using `AssignFile`/`Reset`/`Readln`/`CloseFile` in my first attempts, noticing how fast `TStringList.LoadFromFile` was on small files and studying its implementation, I switched to using a `TStreamReader` and realized how much simpler and faster it is for reading text files.

I also learned some things about a `TDictionary` and why it works so well for this particular situation. And, *after* I implemented this method, I looked at other entries and noticed the one by IWAN KELAIAH was very similar to mine. While the "ikelaiah" entry was submitted before mine, I did not look at or copy anything from that implementation. Chalk it up to great minds thinking alike, I guess!

Finally, I learned that FireDAC has "LocalSQL" and uses the SQLite engine internally to query in-memory tables. It's not efficient for this long data set of two fields but could come in handy later, like when handling results from a REST service or something.  

## History

- Version 1.0: working version with `TDictionary`, `TStringList`, and `TFDMemTable` methods implemented.