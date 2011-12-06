@ECHO OFF

call disconnect.bat

call map.bat

call set_dlc.bat

start %DLC%\oeide\eclipse\eclipse.exe -showlocation -vm %DLC%\jre\bin\javaw.exe -data D:\Projects\OpenedgeArchitect\TVH

exit
