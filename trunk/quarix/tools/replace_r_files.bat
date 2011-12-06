@ECHO OFF

set /p choice=Copy r files? [y/n]:

IF '%choice%'=='N' GOTO :END
IF '%choice%'=='n' GOTO :END

SET BUILDDIR=D:\Projects\OpenedgeArchitect\TVH_BUILD\quarix_TVH

set /p choice=Delete old r files? [y/n]:

IF '%choice%'=='N' GOTO :COPY
IF '%choice%'=='n' GOTO :COPY

cd T:\QUARIX_TVH\Build

T:

del  /F /S /Q *.r

:COPY

XCOPY %BUILDDIR%\*.r T:\QUARIX_TVH\Build /S /E /Y /D

:END

PAUSE
EXIT
