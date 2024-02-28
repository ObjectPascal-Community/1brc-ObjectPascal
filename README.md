# 1️⃣🐝🏎️ The One Billion Row Challenge in Object Pascal

This is the repository that will coordinate the 1 Billion Row Challenge for Object Pascal.

The One Billion Row Challenge (1BRC) is a fun exploration of how far modern Object Pascal can be pushed for aggregating one billion rows from a text file.
Grab all your threads, reach out to SIMD,  or pull any other trick, and create the fastest implementation for solving this task!

<img src="img/1brc.png" alt="1BRC" style="display: block; margin-left: auto; margin-right: auto; margin-bottom:1em; width: 50%;">

The text file contains temperature values for a range of weather stations. Each row is one measurement in the format `<string: station name>;<double: measurement>``, with the measurement value having exactly one fractional digit.
The following shows ten rows as an example:

```
Hamburg;12.0
Bulawayo;8.9
Palembang;38.8
St. John's;15.2
Cracow;12.6
Bridgetown;26.9
Istanbul;6.2
Roseau;34.4
Conakry;31.2
Istanbul;23.0
```

The task is to write an Object Pascal program which reads the file, calculates the min, mean, and max temperature value per weather station, and emits the results on `STDOUT` like this (i.e. sorted alphabetically by station name, and the result values per station in the format `<min>/<mean>/<max>``, rounded to one fractional digit):

```
{Abha=-23.0/18.0/59.2, Abidjan=-16.2/26.0/67.3, Abéché=-10.0/29.4/69.0, Accra=-10.1/26.4/66.4, Addis Ababa=-23.7/16.0/67.0, Adelaide=-27.8/17.3/58.5, ...}
```

Submissions will be, preferably, as a `GIT` `submodule`.
They must contain the source and a Linux `ELF` binary in 64 bits in a folder named `bin` from the `root` folder.

In order to produce the One Billion Rows of text, we are providing the source code for the official generator, so we all have the same entry data.
> **NOTE**
> Generator will come shortly.

Submit your implementation and become part of the leader board!

## Results

These are the results from running all entries into the challenge on my personal computer:
- Ryzen 9 5950x 16 core
- 32GB RAM
- 250GB SSD
- 1TB HDD

| # | Result (m:s.ms): SSH | Result (m:s.ms): HDD | Submitter     | Notes     | Certificates |
|---|----------------------|----------------------|---------------|-----------|--------------|
|?|?|?|?|?|?|
