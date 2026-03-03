#!/bin/bash

MSVC="/c/Program Files (x86)/Microsoft Visual Studio/18/BuildTools/VC/Tools/MSVC/14.50.35717"
KITS="/c/Program Files (x86)/Windows Kits/10"
WINSDK="10.0.26100.0"

export PATH="$MSVC/bin/Hostx64/x64:$PATH"

# cl.exe Windows-style path'lere ihtiyaç duyuyor
export INCLUDE="$(cygpath -w "$MSVC/include");$(cygpath -w "$KITS/Include/$WINSDK/ucrt");$(cygpath -w "$KITS/Include/$WINSDK/um");$(cygpath -w "$KITS/Include/$WINSDK/shared")"

export LIB="$(cygpath -w "$MSVC/lib/x64");$(cygpath -w "$KITS/Lib/$WINSDK/ucrt/x64");$(cygpath -w "$KITS/Lib/$WINSDK/um/x64")"

