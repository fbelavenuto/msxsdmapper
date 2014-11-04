@echo off
cd Driver
sjasmplus --lst=Driver.lst Driver.asm
cd ..
cd updater
sjasmplus --lst=sdmupd.lst sdmupd.asm
cd ..
Nextor\mknexrom Nextor\Nextor-2.0.4.base.dat Driver\SDMAPPER.ROM /d:Driver\driver.bin /m:Nextor\Mapper.ASCII16.bin
echo.
pause