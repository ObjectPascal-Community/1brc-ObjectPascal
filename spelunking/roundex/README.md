# Investigating Rounding

In this folder we are attemnpting to determine if the `Match.Ceil` function is consistent across all platfroms and `IDE`s.

## Lazarus

### Linux

```console
$ sha256sum *.txt
e432eeb4be87e52abc4993f80289a644ca8a71bbbcb007076438147a7c239da3  RoundExDouble.txt
6898cfe05a03f936dd6b977a7b383d80bcd6c509e8669c49d7ec8a8eb4eb5af1  RoundExInteger.txt
```

### Windows

```console
$ sha256sum *.txt
e432eeb4be87e52abc4993f80289a644ca8a71bbbcb007076438147a7c239da3 *RoundExDouble.txt
6898cfe05a03f936dd6b977a7b383d80bcd6c509e8669c49d7ec8a8eb4eb5af1 *RoundExInteger.txt
```

## Delphi

### Linux

### Windows