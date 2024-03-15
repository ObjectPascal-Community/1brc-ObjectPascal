# 1️⃣🐝🏎️ The One Billion Row Challenge in Object Pascal
<p>
    <a href="https://discord.gg/c382VBk"><img src="https://img.shields.io/discord/623794270255579146?label=Delphi Community Discord" alt="Delphi Community" /></a>
    <a href="https://discord.gg/3VdxbSFyJP"><img src="https://img.shields.io/discord/570025060312547359?label=Unofficial Free Pacal Discord" alt="Unofficial Free Pacal" /></a>
</p>

This is the repository that will coordinate the 1 Billion Row Challenge for Object Pascal.

The One Billion Row Challenge (1BRC) is a fun exploration of how far modern Object Pascal can be pushed for aggregating one billion rows from a text file.
Grab all your threads, reach out to SIMD,  or pull any other trick, and create the fastest implementation for solving this task!

<p align="center">
<img src="img/1brc.png" alt="1BRC" style="display: block; margin-left: auto; margin-right: auto; margin-bottom:1em; width: 50%;">
</p>

The text file contains temperature values for a range of weather stations. Each row is one measurement in the format `<string: station name>;<double: measurement>`, with the measurement value having exactly one fractional digit.
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

The task is to write an Object Pascal program which reads the file, calculates the min, mean, and max temperature value per weather station, and emits the results on `STDOUT` like this (i.e., sorted alphabetically by station name, and the result values per station in the format `<min>/<mean>/<max>`, rounded to one fractional digit, with the decimal separator being a period `.`):

```
{Abha=-23.0/18.0/59.2, Abidjan=-16.2/26.0/67.3, Abéché=-10.0/29.4/69.0, Accra=-10.1/26.4/66.4, Addis Ababa=-23.7/16.0/67.0, Adelaide=-27.8/17.3/58.5, ...}
```

## Entering The Challenge
Submissions will be via a `PR`( Pull Request ) to this repository. \
The challenge will run from the 10th of March until the 10th of May, 2024.

When creating your entry, please do as follows:
1. Create a folder under `entries` with your first initial and last name, e.g., for Gustavo Carreno: `entries/gcarreno`.
2. If you're worried about anonymity, because the Internet stinks, feel free to use a fictional one: Bruce Wayne, Clark Kent, James Logan, Peter Parker, Diana of Themyscira. Your pick!
3. Create a `README.md` with some content about your approach, e.g., `entries/gcarreno/README.md`.
4. Put all your code under `entries/<your name>/src`, e.g., `entries/gcarreno/src`.
5. If you need to provide a custom `.gitignore` for something not present in the main one, please do.

This challenge is mainly to allow us to learn something new. This means that copying code from others will be allowed, under these conditions:
1. You can only use pure Object Pascal with no calls to any operating system's `API` or external `C/C++` libraries.
2. The code must have some sort of mention/attribution to the original author, in case you've used someone else's code.
3. It's not a blatant copy just for the sake of submission.
4. It adds something of value, not just a different code formatting.
5. All code should be formatted with the `IDE`'s default formatting tool.

**IMPORTANT** \
This challenge can be entered even if you only have access to the Community Edition of RAD Studio. \
I have a Windows VM, with RAD Studio installed, that will do the necessary cross compilation into my Linux host.

Submit your implementation and become part of the leader board!

## Generating the measurements.txt
> **NOTE** \
> We now have both a Lazarus version and a Delphi version of the generator for both 32b and 64b.

In order to produce the One Billion Rows of text, we are providing the [source code](./generator) for the official generator, so we all have the same entry data.

| Parameter | Description |
|:----------|:------------|
| -h or --help | Writes this help message and exits |
| -v or --version | Writes the version and exits |
| -i or --input-file <filename> | The file containing the Weather Stations |
| -o or --output-file <filename> | The file that will contain the generated lines |
| -n or --line-count <number> | The amount of lines to be generated ( Can use 1_000_000_000 ) |


### Verify
You can verify the generated `measurements.txt` with a `SHA256` utility:

**Linux**
```sh
$ sha256sum ./data/measurements.txt
```
**Windows (PowerShell)**
```ps
Get-FileHash .\data\measurements.txt -Algorithm SHA256
```
Expected `SHA256` hash:
`ebad17b266ee9f5cb3d118531f197e6f68c9ab988abc5cb9506e6257e1a52ce6`

> **NOTE**
>
> I'm still being lazy and I need to do the baseline in order for us to have the same `SHA256` value for an official output.

## Results
These are the results from running all entries into the challenge on my personal computer:
- Ubuntu 23.10 64b
- Ryzen 9 5950x 16 cores
- 32GB RAM
- 250GB SSD
- 1TB HDD

| # | Result (m:s.ms): SSD | Result (m:s.ms): HDD | Compiler | Submitter     | Notes     | Certificates |
|--:|---------------------:|---------------------:|:---------|:--------------|:----------|:-------------|
| 1 | 0:29.212 | 2:2.504 | lazarus-3.0, fpc-3.2.2 | Székely Balázs | Using 16 threads | |

## Evaluating Results
Each contender is run 10 times in a row for both `SSD` and `HDD` using `hyperfine` for the time taking. \
The mean value of the 10 runs is the result for that contender and will be added to the results table above. \
The exact same `measurements.txt` file is used for evaluating all contenders.

## Prize
This is being run for bragging rights only and the fun of such a challenge.

## FAQ
_Q: Can I copy code from other submissions?_\
A: Yes, you can. The primary focus of the challenge is about learning something new, rather than "winning". When you do so, please give credit to the relevant source submissions. Please don't re-submit other entries with no or only trivial improvements.

_Q: What is the encoding of the measurements.txt file?_\
A: The file is encoded with UTF-8.

_Q: Which operating system is used for evaluation?_\
A: Ubuntu 23.10.

## Honour Mentions
I'd like to thank [@paweld](https://github.com/paweld) for taking us from my miserable 20m attempt, to a whopping ~25s, beating the [Python script](https://github.com/gunnarmorling/1brc/blob/main/src/main/python/create_measurements.py) by about 4 and a half minutes.\
I'd like to thank [@mobius](https://github.com/mobius1qwe) for taking the time to provide the Delphi version of the generator.
I'd like to thank [@dtpfl](https://github.com/dtpfl) for his invaluable work on maintaining the `README.md` file up to date with everything.

## Links
The original repository: https://github.com/gunnarmorling/1brc \
I found out about it by watching this video about an attempt in Go: https://www.youtube.com/watch?v=cYng524S-MA \
The blog post in question: https://www.bytesizego.com/blog/one-billion-row-challenge-go

## License
This code base is available under the MIT License.

## Code of Conduct
Be excellent to each other!\
More than winning, the purpose of this challenge is to have fun and learn something new.
