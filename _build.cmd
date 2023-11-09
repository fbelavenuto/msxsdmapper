@echo off

echo Building Driver
docker run --rm -it -v %cd%/driver:/src fbelavenuto/8bitcompilers sjasmplus --lst=driver.lst driver.asm
IF ERRORLEVEL 1 GOTO error

echo Building Updater
docker run --rm -it -v %cd%/updater:/src fbelavenuto/8bitcompilers sjasmplus --lst=sdmupd.lst sdmupd.asm
IF ERRORLEVEL 1 GOTO error

echo Building ROM
docker run --rm -it -v %cd%:/src fbelavenuto/8bitcompilers mknexrom Nextor/Nextor-2.1.0-beta2.base.dat driver/SDMAPPER.ROM /d:driver/driver.bin /m:Nextor/Mapper.ASCII16.bin
IF ERRORLEVEL 1 GOTO error

goto ok

:error
echo Ocorreu algum erro!
:ok
echo.
pause
