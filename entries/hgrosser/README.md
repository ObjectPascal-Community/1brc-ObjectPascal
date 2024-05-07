# Hartmut Grosser

**1 billion row Challenge entry**

## Version
Version 2.10 (version with threads, does NOT yet work for 400 stations or 5 billion rows)

## How to compile
The program was developed with FPC 3.2.2 and Lazarus 2.2.4

It uses package **mORMot 2** to compile, which you can download from https://github.com/synopse/mORMot2

Files to compile: **1brc_th.lpi** and **1brc_th.pas** = this version with threads

## How to start
```
Usage: <path to input file> <thread count> [bit-width for hash-list]
 - thread count: allowed range = [1..64]
 - bit-width for hash-list: sets the size of the hash list, e.g. '16' => 65536 entries,
   allowed range = [16..28], Default=16
Example: hgrosser measurements.txt 32 16
```
There are no switches like `-i` etc, only 2 or 3 values.

There is no more need for further optimizing. Please use '32' as 2nd command line parameter and '16' as 3rd command line parameter.

## How the program works
The Program works with threads.

To speed things up:

- the input file is read via **mORMot**'s type 'TMemoryMap'
- to manage the city names, a self made hash-list is used
- temperatures are stored as integers (multiplied by 10)

## History

- Version 1.00: initial version
- Version 1.50: hash-list optimized, small improvements in parsing the file
- Version 1.51: small improvements in asm function
- Version 1.60: hash-list optimized, some minor improvements, Conditional "noCR" added
- Version 1.61: Conditional "noCR" constantely enabled => input files must NOT have CR's
- Version 2.00: 1st version with threads
- Version 2.10: the input file is now read via **mORMot**'s type 'TMemoryMap', the hash code is now computed via **mORMot**'s crc32c-function, several small improvements
