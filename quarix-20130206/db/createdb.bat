@echo off
set DLC=d:\Progress\v10.2B\dlc

call %DLC%\bin\prostrct.bat create quarix quarix.st -blocksize 4096

call %DLC%\bin\procopy.bat %DLC%/empty4 quarix

pause
