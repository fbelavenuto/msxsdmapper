@echo off
cd Driver
sjasmplus --lst=Driver.lst Driver.asm
cd ..
cd updater
sjasmplus sdmupd.asm
cd ..
rem Nextor\mknexrom Nextor\Nextor-2.0.2.base.dat Driver\NEXTOR.ROM /d:Driver\driver.bin /m:Nextor\Mapper.ASCII16.bin
Nextor\mknexrom Nextor\Nextor-2.1-alpha1.base.dat Driver\NEXTOR.ROM /d:Driver\driver.bin /m:Nextor\Mapper.ASCII16.bin
echo.
pause