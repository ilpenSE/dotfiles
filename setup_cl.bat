@echo off

set "WINSDK=10.0.26100.0"
set "MSVC=C:\Program Files (x86)\Microsoft Visual Studio\18\BuildTools\VC\Tools\MSVC\14.50.35717"
set "KITS=C:\Program Files (x86)\Windows Kits\10"

set "PATH=%MSVC%\bin\Hostx64\x64;%PATH%"

set "INCLUDE=%MSVC%\include;%KITS%\Include\%WINSDK%\ucrt;%KITS%\Include\%WINSDK%\um;%KITS%\Include\%WINSDK%\shared"

set "LIB=%MSVC%\lib\x64;%KITS%\Lib\%WINSDK%\ucrt\x64;%KITS%\Lib\%WINSDK%\um\x64"

echo MSVC ortami hazir!