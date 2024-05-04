# Hartmut Grosser

**1 billion row Challenge entry**

## Version
Version 2.00 (first version with threads)

## How to compile
The program was developed with FPC 3.2.2 and Lazarus 2.2.4

1brc.lpi + 1brc.pas = version without threads

1brc_th.lpi + 1brc_th.pas = version with threads

## How to start
```
Usage: <path to input file> <thread count> [<bit-width for hash-list> [buffer size in KB]]
 - thread count: allowed range = [1..32]
 - bit-width for hash-list: sets the size of the hash list, e.g. '16' => 65536 entries,
   allowed range = [16..28], Default=18
 - buffer size in KB: allowed range = [1..2,000,000 KB], Default=128 KB
Example: hgrosser measurements.txt 32 18 128
```
There are no switches like `-i` etc, only 2..4 values.

### Optimizing the 2nd command line parameter
This parameter sets the thread count. With my own old CPU I could only test 1..4 threads. Although I expect 32 threads to be the fastest, I would be very interested to see the results for 1, 4, 8, 16 and 32 threads. Please use for this test the defaults for the 3rd and 4th command line parameters.

### Optimizing the 3rd command line parameter
In theory the program should run faster with greater bit-widths for the hash-list (because of less collisions). On the computer of Gus - without threads - 18 bits was the fastest. Please try the values from 16 to 20 again and use '32' for the 2nd command line parameter and the default for the 4th command line parameter.

### Optimizing the 4th command line parameter
After the 2nd and 3rd command line parameters had been optimized, please try with them 64, 96, 128, 192 and 256 KB as the 4th command line parameter. Thanks a lot!

## How the program works
The Program works with multi threads.

To speed things up:

- the input file is read via procedure 'blockread' ...
- into an AnsiString, so that function 'PosEx' can be used to parse it
- to manage the city names, a self made hash-list is used
- temperatures are stored as integers (multiplied by 10)

## History

- Version 1.00: initial version
- Version 1.50: hash-list optimized, small improvements in parsing the file
- Version 1.51: small improvements in asm function
- Version 1.60: hash-list optimized, some minor improvements, Conditional "noCR" added
- Version 1.61: Conditional "noCR" constantely enabled => input files must NOT have CR's
- Version 2.00: 1st version with threads

