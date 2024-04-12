# O Coddo

This is a fun test and was shared as requested by a friendly face. Do not consider it a final or stable work. These projects need to be optimized for the destination hardware, and I do not have easy access to such hardware.

Hope you find this interesting.

## How to use

- Get the latest FPC and Lazarus from https://gitlab.com/freepascal.org. [fpcupdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/)  is suggested.
- Get the SCL, the standard library used for development. Clone https://github.com/SCLOrganization/SCL and https://github.com/SCLOrganization/Libraries
- Using Lazarus, open SCL packages: `SCL.lpk` and `SCLLibraries.lpk`
- Open the project from `src` folder
- Make sure you set build mode to `Release`
- Build
- Run as `./ocoddo ./input.txt`
- You can try different parameters for your hardware: `./ocoddo ./input.txt --jumper-count=192 --part-size=192 --processor-count=32`. Try values from `128`, `192`, `256`, and up



Made by O

