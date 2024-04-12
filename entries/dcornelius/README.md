# David Cornelius

An Entry to the One Billion Row Challenge in Object Pascal using Delphi 12 Athens by [David Cornelius](https://github.com/corneliusdavid).

I wanted to see how different methods compared for ease of writing the code and speed of execution, so I approached this in three different ways:

- **TDictionary** - as each line is read, create an object and add it to a `TDictionary` collection; if an entry already exists for a city, update it instead of adding. This was simple to implement but since `TDictionary` doesn't have a sort method, the city name keys were dumped to an array and then sorted. 
- **TStringList** - This was a really simple implementation at fist because I was using `LoadFromFile`; however, when loading one billion rows, it choked with an immediate Range Check Error, so I had to revert to loading it the same way as other approaches. Loading all the strings individually turned out to be so slow and take so much memory, I killed the process before ever checking the results.
- **In-Memory Table** - Another approach I thought I'd try was to load all the data into an in-memory table and use local SQL to query the data. This had the same speed and memory problems the TStringList technique had. Curious to see how long it would take, I left it running overnight but finally killed it after it ran for *26 HOURS*!

My conclusion is that it doesn't make sense to try and load all the rows then apply summation but to collate the data as you load it.  

## Compiler

**Delphi 12 Athens** Enterprise Edition - which includes the ability to generate Linux console apps.

### Dependencies

There are no dependencies if run under one of the most recent versions of Delphi. The code should be backwards-compatible to Delphi 10.3 Rio (it uses inline variables and type inference introduced in that version) and further back with a few simple modifications. It uses `System.StrUtils`, `Generics.Collections`, and a few other run-time libraries in Delphi.

### Debugging

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

To run the challenge, read from the `measurements.txt` file, and use the `TDictionary` method, run it like this:

```
C:> docrnelius measurements.txt dic
```

## Remarks

I entered this challenge as a learning experience. I did not expect to be the fastest as I don't have time to implement multiple threads (which is clearly the road to victory here) but I had fun and learned a lot! 

I now know more about how buffer size can affect stream reads significantly. I have not used streams much in Delphi before but after using `AssignFile`/`Reset`/`Readln`/`CloseFile` in my first attempts, noticing how fast `TStringList.LoadFromFile` was on small files and studying its implementation, I switched to using a `TStreamReader` and realized how much simpler and faster it is for reading text files, something most everyone else has certainly known for many years! (But hey, I work with databases and APIs far more often than text files, so reverted back to some very old habits.)

I also learned some things about a `TDictionary` and why it works so well for this particular situation. And, *after* I implemented this method, I looked at other entries and noticed the one by IWAN KELAIAH was very similar to mine. While the "ikelaiah" entry was submitted before mine, I did not look at or copy anything from that implementation. Chalk it up to great minds thinking alike, I guess!

Finally, I learned that FireDAC has "LocalSQL" and uses the SQLite engine internally to query in-memory tables. It's not efficient for this long data set of two fields but could come in handy later, like when handling results from a REST service or something. 

## Acknowledgements

I'd like to thank [Gustavo 'Gus' Carreno](https://github.com/gcarreno) for bringing this challenge to the Pascal programming community. 

I'd also like to give a shout-out to my friends at the [Oregon Delphi User Group](https://odug.org), where I [presented the challenge](https://odug.org/events/2024-03/) and implemented several of their suggestions for optimization.

## History

- Version 1.0 (April, 2024): successful entry using `TDictionary`. Also, implemented the solution with `TStringList` and `TFDMemTable` but they never produced a result in any reasonable timeframe.