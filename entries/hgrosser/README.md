# Hartmut Grosser

**1 billion row Challenge entry**

## Version
Version 1.0

## How to compile
The program was developed with FPC 3.2.2 and Lazarus 2.2.4

## How to start
```
Usage:   hgrosser <path to input file> [buffer size in kb (Default=128 kb)]
Example: hgrosser ./measurements.txt 128
```
There are no switches like `-i` etc, only values.

Please start the program 3 times with buffer sizes of '128', '192' and '256'.

## How the program works
The Program works with 1 thread.

To speed things up:

- the input file is read via procedure 'blockread' ...
- into an AnsiString, so that function 'PosEx' can be used to parse it
- to manage the city names, a 'TFPHashList' is used
- temperatures are stored as integers (multiplied by 10)
