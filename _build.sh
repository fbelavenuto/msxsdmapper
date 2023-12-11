#!/usr/bin/env sh
set -e

echo Updating docker images
docker pull fbelavenuto/8bitcompilers
docker pull fbelavenuto/xilinxise

NXT_VERSION=`<Nextor/VERSION`

echo Building Driver
docker run --rm -it -v $PWD/driver:/src fbelavenuto/8bitcompilers N80 DRIVER.ASM DRIVER.BIN --listing-file DRIVER.LST

echo Building ROM
docker run --rm -it -v $PWD:/src fbelavenuto/8bitcompilers mknexrom Nextor/Nextor-${NXT_VERSION}.base.dat driver/SDMAPPER.ROM /d:driver/DRIVER.BIN /m:Nextor/Mapper.ASCII16.bin

echo Building Updater
docker run --rm -it -v $PWD/updater:/src fbelavenuto/8bitcompilers N80 SDMUPD.ASM SDMUPD.COM --listing-file SDMUPD.LST

echo Building CPLD bitstream
docker run --rm -it -v $PWD/CPLD:/workdir fbelavenuto/xilinxise make
