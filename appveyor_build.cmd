@rem ***********************************************************************
@rem *                                                                     *
@rem *                                 OCaml                               *
@rem *                                                                     *
@rem *                 David Allsopp, OCaml Labs, Cambridge.               *
@rem *                                                                     *
@rem *   Copyright 2017 MetaStack Solutions Ltd.                           *
@rem *                                                                     *
@rem *   All rights reserved.  This file is distributed under the terms of *
@rem *   the GNU Lesser General Public License version 2.1, with the       *
@rem *   special exception on linking described in the file LICENSE.       *
@rem *                                                                     *
@rem ***********************************************************************

@rem BE CAREFUL ALTERING THIS FILE TO ENSURE THAT ERRORS PROPAGATE
@rem IF A COMMAND SHOULD FAIL IT PROBABLY NEEDS TO END WITH
@rem   || exit /b 1
@rem BASICALLY, DO THE TESTING IN BASH...

@rem Do not call setlocal!
@echo off

goto %1

goto :EOF

:SaveVars
set OCAML_PREV_PATH=%PATH%
set OCAML_PREV_LIB=%LIB%
set OCAML_PREV_INCLUDE=%INCLUDE%
goto :EOF

:RestoreVars
set PATH=%OCAML_PREV_PATH%
set LIB=%OCAML_PREV_LIB%
set INCLUDE=%OCAML_PREV_INCLUDE%
goto :EOF

:install
rem appveyor DownloadFile "http://alain.frisch.fr/flexdll/flexdll-0.35.tar.gz" -FileName "flexdll.tar.gz" || exit /b 1
appveyor DownloadFile "http://alain.frisch.fr/flexdll/flexdll-bin-0.35.zip" -FileName "flexdll.zip" || exit /b 1
rem flexdll.zip is processed here, rather than in appveyor_build.sh because the
rem unzip command comes from MSYS2 (via Git for Windows) and it has to be
rem invoked via cmd /c in a bash script which is weird(er).
mkdir "%APPVEYOR_BUILD_FOLDER%\..\flexdll"
move flexdll.zip "%APPVEYOR_BUILD_FOLDER%\..\flexdll"
cd "%APPVEYOR_BUILD_FOLDER%\..\flexdll" && unzip -q flexdll.zip

rem Make sure the Cygwin path comes before the Git one (otherwise cygpath
rem gets confused as to which root it's in).
set Path=C:\cygwin64\bin;%OCAMLROOT%\bin\flexdll;%Path%
"%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc cygwin"
"%CYG_ROOT%\setup-x86_64.exe" -qgnNdO -R "%CYG_ROOT%" -s "%CYG_MIRROR%" -l "%CYG_CACHE%" -P diffutils -P make -P mingw64-i686-gcc-core >NUL || exit /b 1
"%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc cygwin"
"%CYG_ROOT%\bin\bash.exe" -lec "$APPVEYOR_BUILD_FOLDER/appveyor_build.sh install" || exit /b 1

call :SaveVars
goto :EOF

:build
rem Run the msvc64 and mingw32 builds
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\amd64\vcvars64.bat"
"%CYG_ROOT%\bin\bash.exe" -lec "$APPVEYOR_BUILD_FOLDER/appveyor_build.sh" || exit /b 1

rem Reconfigure the environment and run the msvc32 partial build
call :RestoreVars
call "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.cmd" /x86
"%CYG_ROOT%\bin\bash.exe" -lec "$APPVEYOR_BUILD_FOLDER/appveyor_build.sh msvc32-only" || exit /b 1
goto :EOF

:test
rem Reconfigure the environment for the msvc64 build
call :RestoreVars
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\amd64\vcvars64.bat"
"%APPVEYOR_BUILD_FOLDER%\ocamlc.opt" -version || exit /b 1
set CAML_LD_LIBRARY_PATH=%OCAMLROOT%/lib/stublibs
"%CYG_ROOT%\bin\bash.exe" -lec "$APPVEYOR_BUILD_FOLDER/appveyor_build.sh test" || exit /b 1
goto :EOF
