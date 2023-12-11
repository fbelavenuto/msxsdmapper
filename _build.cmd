@echo off

echo Updating docker image
docker pull fbelavenuto/8bitcompilers
docker pull fbelavenuto/xilinxise

set /p NXT_VERSION=<Nextor\VERSION

echo Building Driver
docker run --rm -it -v %cd%/driver:/src fbelavenuto/8bitcompilers N80 DRIVER.ASM DRIVER.BIN --listing-file DRIVER.LST
IF ERRORLEVEL 1 GOTO error

echo Building ROM
docker run --rm -it -v %cd%:/src fbelavenuto/8bitcompilers mknexrom Nextor/Nextor-%NXT_VERSION%.base.dat driver/SDMAPPER.ROM /d:driver/DRIVER.BIN /m:Nextor/Mapper.ASCII16.bin
IF ERRORLEVEL 1 GOTO error

echo Building Updater
docker run --rm -it -v %cd%/updater:/src fbelavenuto/8bitcompilers N80 SDMUPD.ASM SDMUPD.COM --listing-file SDMUPD.LST
IF ERRORLEVEL 1 GOTO error

echo Building CPLD bitstream
docker run --rm -it -v %cd%/CPLD:/workdir fbelavenuto/xilinxise make

goto ok

:error
echo Ocorreu algum erro!
:ok
echo.
pause
