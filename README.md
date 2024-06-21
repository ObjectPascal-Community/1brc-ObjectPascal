# 1️⃣🐝🏎️ The One Billion Row Challenge in Object Pascal
<p>
    <a href="https://discord.gg/c382VBk"><img src="https://img.shields.io/discord/623794270255579146?label=Delphi Community&logo=discord" alt="Delphi Community" /></a>
    <a href="https://discord.gg/3VdxbSFyJP"><img src="https://img.shields.io/discord/570025060312547359?label=Unofficial Free Pascal&logo=discord" alt="Unofficial Free Pacal" /></a>
    <a href="https://t.me/delphidevelopers"><img src="https://img.shields.io/badge/Telegram-Delphi_Developers-blue?logo=telegram" /></a>
    <a href="https://t.me/freepascal_en"><img src="https://img.shields.io/badge/Telegram-Free_Pascal_&_Lazarus-blue?logo=telegram" /></a>
    <a href="https://forum.lazarus.freepascal.org/index.php/topic,66571.0.html"><img src="https://img.shields.io/badge/Lazarus_Forum-1BRC_Thread-blue" /></a>
    <a href="https://en.delphipraxis.net/topic/11209-offical-launch-of-the-1-billion-row-challenge-in-object-pascal/"><img src="https://img.shields.io/badge/Delphi_Praxis_Forum-1BRC_Thread-blue" /></a>
</p>

This is the repository that will coordinate the 1 Billion Row Challenge for Object Pascal.

The One Billion Row Challenge (1BRC) is a fun exploration of how far modern Object Pascal can be pushed for aggregating one billion rows from a text file.
Grab all your threads, reach out to SIMD,  or pull any other trick, and create the fastest implementation for solving this task!

<p align="center">
<img src="img/1brc.png" alt="1BRC" style="display: block; margin-left: auto; margin-right: auto; margin-bottom:1em; width: 50%;">
</p>

The text file contains temperature values for a range of weather stations. Each row is one measurement in the format `<string: station name>;<double: measurement>`, with the measurement value having exactly one fractional digit. Rows are separated by a single line feed equal of LF (ascii 10) for consistency with the original challenge - and not CR+LF (ascii 13+10) any more.
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

The task is to write an Object Pascal program which reads the file, calculates the min, mean, and max temperature value per weather station, and emits the results on `STDOUT` like this (i.e., sorted alphabetically by station name, and the result values per station in the format `<min>/<mean>/<max>`, rounded to one fractional digit, with the decimal separator being a period `.`, and for that you can chose one of the options presented in the [Rounding Section](#rounding) or implement your own that is consistent with the options provided.):

```
{Abha=-23.0/18.0/59.2, Abidjan=-16.2/26.0/67.3, Abéché=-10.0/29.4/69.0, Accra=-10.1/26.4/66.4, Addis Ababa=-23.7/16.0/67.0, Adelaide=-27.8/17.3/58.5, ...}
```

## Entering The Challenge
Submissions will be via a `PR` (Pull Request) to this repository.  
The challenge will run from the 10th of March until the 10th of May, 2024.

When creating your entry, please do as follows:
1. Create a folder under `entries` with your first initial and last name, e.g., for Gustavo Carreno: `entries/gcarreno`.
2. If you're worried about anonymity, because the Internet stinks, feel free to use a fictional one: Bruce Wayne, Clark Kent, James Logan, Peter Parker, Diana of Themyscira. Your pick!
3. Create a `README.md` with some content about your approach, e.g., `entries/gcarreno/README.md`.
4. Put all your code under `entries/<your name>/src`, e.g., `entries/gcarreno/src`.
5. Send your binary to the `bin` folder off the root of this repository. 
6. If you need to provide a custom `.gitignore` for something not present in the main one, please do.
7. Read the [CONTRIBUTING.md](./CONTRIBUTING.md) file for more details.

This challenge is mainly to allow us to learn something new. This means that copying code from others will be allowed, under these conditions:
1. You can only use pure Object Pascal with no calls to any operating system's `API` or external `C/C++` libraries. \
  **There's been a bit of confusion about this restriction.**  
  - To clear that out: You can use any package/custom code you want.
  - As long as it compiles cross-platform and itself is only pure Object Pascal.
  - Anything from the `Jedi Project` or even `mORMmot` ( or anything else ), if it compiles, runs cross-platform it's allowed.
2. The code must have some sort of mention/attribution to the original author, in case you've used someone else's code.
3. It's not a blatant copy just for the sake of submission.
4. It adds something of value, not just a different code formatting.
5. All code should be formatted with the `IDE`'s default formatting tool.

**IMPORTANT**  
This challenge can be entered even if you only have access to the Community Edition of RAD Studio. I have a Windows VM, with RAD Studio installed, that will do the necessary cross compilation into my Linux host.

Submit your implementation and become part of the leader board!

## Rounding

With the help of this magnificent community, we were able to finally get to a rounding solution that works.

This means that I'm encouraging everyone to use the code that is now in the [Baseline.Common](baseline/Common) unit.\
I do have to make crystal clear that using that code is an **option**, one that you can always opt out of.\
But if you do opt in, just include that unit in your entry and job's a done.

## Generating the measurements.txt
> **NOTE**  
> We now have both a Lazarus version and a Delphi version of the generator for both 32b and 64b.

In order to produce the One Billion Rows of text, we are providing the [source code](./generator) for the official generator, so we all have the same entry data.

| Parameter | Description |
|:----------|:------------|
| **-h** or **--help** | Writes this help message and exits |
| **-v** or **--version** | Writes the version and exits |
| **-i** or **--input-file \<filename\>** | The file containing the Weather Stations |
| **-o** or **--output-file \<filename\>** | The file that will contain the generated lines |
| **-n** or **--line-count \<number\>** | The amount of lines to be generated ( Can use 1_000_000_000 ) |
| **-4** or **--400stations** | Only 400 weather stations in output file |

## Baseline
> **NOTE**  
> This is still a bit in flux, still needing to get the Delphi version done.

In order to verify the official output, we are providing the [source code](./baseline) for the official baseline.

| Parameter | Description |
|:----------|:------------|
| **-h** or **--help** | Writes this help message and exits |
| **-v** or **--version** | Writes the version and exits |
| **-i** or **--input-file \<filename\>** | The file containing the 1 billion rows |

## Verify Input File
You can verify the generated `measurements.txt` with a `SHA256` utility:

**Linux**
```console
$ sha256sum ./data/measurements.txt
```
**Windows (Command Line)**
```console
C:> CertUtil -hashfile .\data\measurements.txt SHA256
```
**Windows (PowerShell)**
```powershell
Get-FileHash .\data\measurements.txt -Algorithm SHA256
```
Expected `SHA256` hash:
`2b48bc2fa0b82d748925a820f43f75df01cc06df7447c7571e52d3962e675960`

## Verify Output File

There is now a Delphi version of the baseline. This means that we now have an official way of generating a valid output on both sides of the fence.

With this, we now have the official hash: `4256d19d3e134d79cc6f160d428a1d859ce961167bd01ca528daca8705163910`

There's also an archived version of the [baseline output](./data/baseline.output.gz)

## Small File Hashes

For easier comparison with the baseline, here are the hashes for different generated row counts:

| Lines | Input File Hash | Output File Hash |
|--:|----------------:|---------:|
| 1_000       | `0be4844a2417c08a85a44b26509bbe6868a6f65d0e0d087d3f9ceedc02f5ceaa` | `d42c37ca405f230e91dd0a75e6741dbbbcddd2338963ea0f0e727cf90ecbd7e7` |
| 10_000      | `447380628ca25b1c9901c2e62e01591f2e2f794d2888934a5e9d4a67d72346a5` | `b4dd36d80a63fefdccbff50ffaaef7e2092034935c729b3330569c7c7f7351fc` |
| 100_000     | `dd3a4821e91de82e44f17f65b1951af8a21652b92c20a7e53a1fa02ea6e5fbd2` | `c9e50d46bba327727bf4b412ec0401e0c2e59c9035b94b288e15631ca621cb52` |
| 1_000_000   | `c2955973c3db29bf544655c11d2d3c7ac902c9f65014026b210bd25eb1876c0c` | `5fedbd9811660ee3423f979a0c854ec8b70da3e804170bc39bcc400c94f93bc0` |
| 10_000_000  | `90193d314e991f7789258c8b6b06c493a4d624991e203b12343c2a8ce1d0c7fd` | `2f3a6383b3bc83a9ad53fc0773de2da57bd4add8a51662cdb86bfca502d276a3` |
| 100_000_000 | `f55384da4646a0c77a1d5dd94a58f8430c5956fe180cedcb17b4425fe5389a39` | `7e8339b5d268fa400a93887b7a1140ac1adf683a8e837e6274fd71e383c26c6b` |

## Differences From Original
I've decided that I would want this challenge to be turned way up to 11!

This means that there are some differences from the original.

The original results are calculated on a smaller set of weather stations: 400.\
While I haven't tabulated how many reside on the input file, we do not limit it to any number as we use the full ~40K stations present on `data/weather_stations.csv` to generate the input file.

Another difference is the machines these are run on.\
I'm using my own machine, with the specs mentioned on the [Results](#results) section bellow.\
I'm also allowing the use of the full 32 threads that my machine provides, where the original challenge limits it to 8.\
The original challenge also has a second results table with 10K stations and the use of all 64 threads.

With all this said, comparison with the original challenge should be made with this in mind.

## Results
These are the results from running all entries into the challenge on my personal computer:
- Ubuntu 23.10 64b
- Ryzen 9 5950x 16 cores
- 32GB RAM
- 250GB SSD
- 1TB HDD

| # | Result (m:s.ms) | Compiler | Submitter | Notes | Certificates |
|--:|----------------:|---------:|:----------|:------|:-------------|
|  1 |   0:1.261 | lazarus-3.99, fpc-3.3.1 | Arnaud Bouchez         | Using `mORMot2`, 32 threads        | |
|  2 |   0:1.950 | lazarus-3.99, fpc-3.3.1 | O Coddo                | Using `SCL`, 32 threads            | |
|  3 |   0:2.101 | lazarus-3.99, fpc-3.3.1 | Georges Hatem          | Using `mORMot2`, 32 threads        | |
|  4 |   0:5.248 | lazarus-3.99, fpc-3.3.1 | Hartmut Grosser        | Using 32 thread                    | |
|  5 |   0:7.363 | lazarus-3.99, fpc-3.3.1 | Benito van der Zander  | Using 32 threads                   | |
|  6 |   0:9.627 | lazarus-3.99, fpc-3.3.1 | G Klark                | Using 32 threads                   | |
|  7 |  0:13.321 | lazarus-3.99, fpc-3.3.1 | Székely Balázs         | Using 32 threads                   | |
|  8 |  0:18.062 | lazarus-3.99, fpc-3.3.1 | Lurendrejer Aksen      | Using 32 threads                   | |
|  9 |   1:9.354 | lazarus-3.99, fpc-3.3.1 | Richard Lawson         | Using 1 thread                     | |
| 10 |  2:24.787 | lazarus-3.99, fpc-3.3.1 | Iwan Kelaiah           | Using 1 thread                     | |
| 11 |   6:2.343 | delphi 12.1             | Brian Fire             | Using 8 threads                    | |
| 12 |  6:53.788 | delphi 12.1             | David Cornelius        | Using 1 thread                     | |
| 13 |  8:37.975 | delphi 12.1             | Daniel Töpfl           | Using 1 thread                     | |

> **NOTE**
>
> After some tests performed by @paweld, it makes no sense to have an `HDD` run.
> I've removed that from the results

## Evaluating Results
Each contender is run 10 times in a row for both `SSD` and `HDD` using `hyperfine` for the time taking. \
The mean value of the 10 runs is the result for that contender and will be added to the results table above. \
The min and max values are discarded and the remaining 8 values are then used to calculate the average. \
The exact same `measurements.txt` file is used for evaluating all contenders.

## Prize
This is being run for bragging rights only and the fun of such a challenge.

## FAQ
_Q: Can I copy code from other submissions?_\
A: Yes, you can. The primary focus of the challenge is about learning something new, rather than "winning". When you do so, please give credit to the relevant source submissions. Please don't re-submit other entries with no or only trivial improvements.

_Q: What is the encoding of the measurements.txt file?_\
A: The file is encoded with UTF-8.

_Q: Which operating system is used for evaluation?_\
A: Ubuntu 23.10 64b.

## Honour Mentions
I'd like to thank [@paweld](https://github.com/paweld) for taking us from my miserable 20m attempt, to a whopping ~25s, beating the [Python script](https://github.com/gunnarmorling/1brc/blob/main/src/main/python/create_measurements.py) by about 4 and a half minutes.\
I'd like to thank [@mobius](https://github.com/mobius1qwe) for taking the time to provide the Delphi version of the generator.\
I'd like to thank [@dtpfl](https://github.com/dtpfl) for his invaluable work on maintaining the `README.md` file up to date with everything.\
I'd like to thank Székely Balázs for providing many patches to make everything compliant with the original challenge.\
I'd like to thank [@corneliusdavid](https://github.com/corneliusdavid) for giving some of the information files a once over and making things more legible and clear.\
I'd like to thank Mr. **Pack**man, aka O, for clearing the fog around the rounding issues.\
I'd like to thank [Georges](https://github.com/georges-hatem) for providing us with the Delphi version of baseline.

## Links
The original repository: https://github.com/gunnarmorling/1brc \
I found out about it by watching this video about an attempt in Go: https://www.youtube.com/watch?v=cYng524S-MA \
The blog post in question: https://www.bytesizego.com/blog/one-billion-row-challenge-go

## License
This code base is available under the MIT License.

## Code of Conduct
Be excellent to each other!\
More than winning, the purpose of this challenge is to have fun and learn something new.
