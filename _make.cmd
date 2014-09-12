@echo off
cd Driver
sjasmplus --lst=Driver.lst Driver.asm
cd ..
cd updater
sjasmplus --lst=sdmupd.lst sdmupd.asm
cd ..
Nextor\mknexrom Nextor\Nextor-2.0.3.base.dat Driver\SDMAPPER.ROM /d:Driver\driver.bin /m:Nextor\Mapper.ASCII16.bin
rem Nextor\mknexrom Nextor\Nextor-2.1-alpha1.base.dat Driver\SDMAPPER.ROM /d:Driver\driver.bin /m:Nextor\Mapper.ASCII16.bin
echo.
pause