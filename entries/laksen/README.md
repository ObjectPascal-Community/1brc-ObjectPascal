# Lurendrejer Aksen

Applies a very lowlevel solution to the problem. No tricks, only threads.

1. Split file into chunks at line-ending boundaries.
2. Process all chunks in a number of threads. Add parsed information in thread local hash tables.
3. Merge hash tables in a sorted list when the threads exit.
4. Dump sorted information.

## Example for a 16 core machine

```sh
./bin/laksen datafile.csv 16 16
```
