#!/usr/bin/env sh
set -e

echo Building Driver
docker run --rm -it -v $PWD/driver:/src fbelavenuto/8bitcompilers N80 DRIVER.ASM DRIVER.BIN --listing-file DRIVER.LST

echo Building ROM
docker run --rm -it -v $PWD:/src fbelavenuto/8bitcompilers mknexrom Nextor/Nextor-2.1.0-beta2.base.dat driver/SDMAPPER.ROM /d:driver/driver.bin /m:Nextor/Mapper.ASCII16.bin

echo Building Updater
docker run --rm -it -v $PWD/updater:/src fbelavenuto/8bitcompilers N80 SDMUPD.ASM SDMUPD.BIN --listing-file SDMUPD.LST
