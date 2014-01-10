@echo off
cd Driver
sjasm Driver.asm
sjasm DriverINIOUTI.asm
cd ..
Nextor\mknexrom Nextor\Nextor-2.0-beta2.base.dat Driver\NEXTOR.ROM /d:Driver\driver.bin /m:Nextor\Mapper.ASCII16.bin
Nextor\mknexrom Nextor\Nextor-2.0-beta2.base.dat Driver\NEXTOR2.ROM /d:Driver\driverINIOUTI.bin /m:Nextor\Mapper.ASCII16.bin
echo.
pause