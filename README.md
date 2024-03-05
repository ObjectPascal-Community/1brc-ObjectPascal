# 1️⃣🐝🏎️ The One Billion Row Challenge in Object Pascal

This is the repository that will coordinate the 1 Billion Row Challenge for Object Pascal.

The One Billion Row Challenge (1BRC) is a fun exploration of how far modern Object Pascal can be pushed for aggregating one billion rows from a text file.
Grab all your threads, reach out to SIMD,  or pull any other trick, and create the fastest implementation for solving this task!

<img src="img/1brc.png" alt="1BRC" style="display: block; margin-left: auto; margin-right: auto; margin-bottom:1em; width: 50%;">

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

The task is to write an Object Pascal program which reads the file, calculates the min, mean, and max temperature value per weather station, and emits the results on `STDOUT` like this (i.e. sorted alphabetically by station name, and the result values per station in the format `<min>/<mean>/<max>`, rounded to one fractional digit):

```
{Abha=-23.0/18.0/59.2, Abidjan=-16.2/26.0/67.3, Abéché=-10.0/29.4/69.0, Accra=-10.1/26.4/66.4, Addis Ababa=-23.7/16.0/67.0, Adelaide=-27.8/17.3/58.5, ...}
```

## Honour Mentions

I'd like to thank [@paweld](https://github.com/paweld) for taking us from my miserable 20m attempt, to a whopping ~25s, beating the [Python script](https://github.com/gunnarmorling/1brc/blob/main/src/main/python/create_measurements.py) by about 4 amd a half minutes.

I'd like to thank [@mobius](https://github.com/mobius1qwe) for taking the time to provide the Delphi version of the generator.

## Links

The original repository: https://github.com/gunnarmorling/1brc

I found out about it by watching this video about an attempt in Go: https://www.youtube.com/watch?v=cYng524S-MA

The blog post in question: https://www.bytesizego.com/blog/one-billion-row-challenge-go

## Entering The Challenge

Submissions will be via a `PR`( Pull Request ) to this repository.

When creating your entry, please do as follows:
1. Create a folder under `entries` with your first initial and last name, e.g., for Gustavo Carreno: `entries/gcarreno`.
2. Create a `README.md` with some content about your approach, e.g., `entries/gcarreno/README.md`.
3. Put all your code under `entries/<your name>/src`, e.g., `entries/gcarreno/src`.
4. If you need to provide a custom `.gitignore` for something not present in the main one, please do.

This challenge is mainly to allow us to learn something new. This means that copying code from others will be allowed, under these conditions:
1. The code must have some sort of mention/attribution to the original author.
2. It's not a blatant copy just for the sake of submission.
3. It adds something of value, not just a different code formatting.

In order to produce the One Billion Rows of text, we are providing the source code for the official generator, so we all have the same entry data.

> **NOTE**
>
> We now have both a Lazarus version and a Delphi version of the generator for both 32b and 64b.

Submit your implementation and become part of the leader board!

## Results

These are the results from running all entries into the challenge on my personal computer:
- Ryzen 9 5950x 16 cores
- 32GB RAM
- 250GB SSD
- 1TB HDD

| # | Result (m:s.ms): SSD | Result (m:s.ms): HDD | Submitter     | Notes     | Certificates |
|---|----------------------|----------------------|---------------|-----------|--------------|
|?|?|?|?|?|?|

## Evaluating Results

Results are determined by running the program on:
- Ryzen 9 5950x 16 core
- 32GB RAM
- 250GB SSD
- 1TB HDD

Each contender is run five times in a row for both `SSD` and `HDD` using `hyperfine` for the time taking.
The slowest and the fastest runs are discarded.
The mean value of the remaining three runs is the result for that contender and will be added to the results table above.
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

## License

This code base is available under the MIT License.

## Code of Conduct

Be excellent to each other!
More than winning, the purpose of this challenge is to have fun and learn something new.
