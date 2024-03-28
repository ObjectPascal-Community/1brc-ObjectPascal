# Investigating Rounding

In this folder we are attemnpting to determine if the `Match.Ceil` function is consistent across all platfroms and `IDE`s.

## Lazarus

### Linux 64

```console
$ sha256sum *.txt
e432eeb4be87e52abc4993f80289a644ca8a71bbbcb007076438147a7c239da3  RoundExDouble.txt
6898cfe05a03f936dd6b977a7b383d80bcd6c509e8669c49d7ec8a8eb4eb5af1  RoundExInteger.txt
```

### Windows 64

```console
$ sha256sum *.txt
e432eeb4be87e52abc4993f80289a644ca8a71bbbcb007076438147a7c239da3 *RoundExDouble.txt
6898cfe05a03f936dd6b977a7b383d80bcd6c509e8669c49d7ec8a8eb4eb5af1 *RoundExInteger.txt
```

## Delphi

### Linux 64

```console
$ sha256sum *.txt
c46b85b4004ab4d01b9bea87f1aca68ef0826b86117e023153fce34c28254890  RoundExDouble.txt
b64b43934182fdf63fb85ea7118230e9d249f27a76c63c59ea42afbb18b16dc9  RoundExInteger.txt
```

### Windows 64

```console
$ sha256sum *.txt
5c825e537589688d20d82b1871a00be64587834c82922b98159fc362c1739e9b *RoundExDouble.txt
be44b2fd0f87ec2aa564817608aa48294fdbd5d15f884610b06e313abeb1fff7 *RoundExInteger.txt
```
