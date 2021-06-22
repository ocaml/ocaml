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

chcp 65001 > nul
set BUILD_PREFIX=🐫реализация
set OCAMLROOT=%PROGRAMFILES%\Бактріан🐫

if "%1" neq "install" goto %1
setlocal enabledelayedexpansion
echo AppVeyor Environment
for %%K in (ACCOUNT_NAME ACS_DEPLOYMENT_UPGRADE_MODE API_URL
            ARTIFACT_UPLOAD_TIMEOUT BUILD_FOLDER BUILD_ID BUILD_NUMBER
            BUILD_VERSION BUILD_WORKER_IMAGE BUILD_WORKER_IMAGE
            CACHE_ENTRY_UPLOAD_DOWNLOAD_TIMEOUT CACHE_SKIP_RESTORE
            CACHE_SKIP_SAVE FILE_DOWNLOAD_TIMEOUT FORCED_BUILD
            IGNORE_COMMIT_FILTERING_ON_TAG JOB_ID JOB_NAME JOB_NUMBER PROJECT_ID
            PROJECT_NAME PROJECT_SLUG PULL_REQUEST_HEAD_COMMIT
            PULL_REQUEST_HEAD_REPO_BRANCH PULL_REQUEST_HEAD_REPO_NAME
            PULL_REQUEST_NUMBER PULL_REQUEST_TITLE RE_BUILD REPO_BRANCH
            REPO_COMMIT_AUTHOR REPO_COMMIT_AUTHOR_EMAIL REPO_COMMIT
            REPO_COMMIT_MESSAGE REPO_COMMIT_MESSAGE_EXTENDED
            REPO_COMMIT_TIMESTAMP REPO_NAME REPO_PROVIDER REPO_SCM
            REPOSITORY_SHALLOW_CLONE_TIMEOUT REPO_TAG_NAME REPO_TAG
            RE_RUN_INCOMPLETE SAVE_CACHE_ON_ERROR SCHEDULED_BUILD
            SKIP_FINALIZE_ON_EXIT APPVEYOR URL WAP_ARTIFACT_NAME
            WAP_SKIP_ACLS) do echo APPVEYOR_%%K=!APPVEYOR_%%K!
echo CI=%CI%
echo CONFIGURATION=%CONFIGURATION%
echo PLATFORM=%PLATFORM%
endlocal

goto install

goto :EOF

:CheckPackage
"%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc %1" | findstr %1 > nul
if %ERRORLEVEL% equ 1 (
  echo Cygwin package %1 will be installed
  set CYGWIN_INSTALL_PACKAGES=%CYGWIN_INSTALL_PACKAGES%,%1
)
goto :EOF

:UpgradeCygwin
if "%CYGWIN_INSTALL_PACKAGES%" neq "" "%CYG_ROOT%\setup-x86_64.exe" --quiet-mode --no-shortcuts --no-startmenu --no-desktop --only-site --root "%CYG_ROOT%" --site "%CYG_MIRROR%" --local-package-dir "%CYG_CACHE%" --packages %CYGWIN_INSTALL_PACKAGES:~1% > nul
for %%P in (%CYGWIN_COMMANDS%) do "%CYG_ROOT%\bin\%%P.exe" --version 2> nul > nul || set CYGWIN_UPGRADE_REQUIRED=1
"%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc %CYGWIN_PACKAGES%"
if %CYGWIN_UPGRADE_REQUIRED% equ 1 (
  echo Cygwin package upgrade required - please go and drink coffee
  "%CYG_ROOT%\setup-x86_64.exe" --quiet-mode --no-shortcuts --no-startmenu --no-desktop --only-site --root "%CYG_ROOT%" --site "%CYG_MIRROR%" --local-package-dir "%CYG_CACHE%" --upgrade-also > nul
  "%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc %CYGWIN_PACKAGES%"
)
goto :EOF

:install

if defined SDK set SDK=call %SDK%
if not defined SDK (
  if "%PORT%" equ "msvc64" set SDK=call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\amd64\vcvars64.bat"
  if "%PORT%" equ "msvc32" set SDK=call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\vcvars32.bat"
)
%SDK%

git worktree add "..\%BUILD_PREFIX%-%PORT%" -b appveyor-build-%PORT%

cd "..\%BUILD_PREFIX%-%PORT%"
if "%BOOTSTRAP_FLEXDLL%" equ "true" (
  git submodule update --init flexdll
)

cd "%APPVEYOR_BUILD_FOLDER%"
appveyor DownloadFile "https://github.com/alainfrisch/flexdll/archive/%FLEXDLL_VERSION%.tar.gz" -FileName "flexdll.tar.gz" || exit /b 1
appveyor DownloadFile "https://github.com/alainfrisch/flexdll/releases/download/%FLEXDLL_VERSION%/flexdll-bin-%FLEXDLL_VERSION%.zip" -FileName "flexdll.zip" || exit /b 1
rem flexdll.zip is processed here, rather than in appveyor_build.sh because the
rem unzip command comes from MSYS2 (via Git for Windows) and it has to be
rem invoked via cmd /c in a bash script which is weird(er).
mkdir "%APPVEYOR_BUILD_FOLDER%\..\flexdll"
move flexdll.zip "%APPVEYOR_BUILD_FOLDER%\..\flexdll"
cd "%APPVEYOR_BUILD_FOLDER%\..\flexdll" && unzip -q flexdll.zip

rem CYGWIN_PACKAGES is the list of required Cygwin packages (cygwin is included
rem in the list just so that the Cygwin version is always displayed on the log).
rem CYGWIN_COMMANDS is a corresponding command to run with --version to test
rem whether the package works. This is used to verify whether the installation
rem needs upgrading.
set CYGWIN_PACKAGES=cygwin make diffutils
set CYGWIN_COMMANDS=cygcheck make diff
if "%PORT%" equ "mingw32" (
  rem mingw64-i686-runtime does not need explicitly installing, but it's useful
  rem to have the version reported.
  set CYGWIN_PACKAGES=%CYGWIN_PACKAGES% mingw64-i686-gcc-core mingw64-i686-runtime
  set CYGWIN_COMMANDS=%CYGWIN_COMMANDS% i686-w64-mingw32-gcc cygcheck
)
if "%PORT%" equ "mingw64" (
  set CYGWIN_PACKAGES=%CYGWIN_PACKAGES% mingw64-x86_64-gcc-core
  set CYGWIN_COMMANDS=%CYGWIN_COMMANDS% x86_64-w64-mingw32-gcc
)
if "%PORT%" equ "cygwin32" (
  set CYGWIN_PACKAGES=%CYGWIN_PACKAGES% cygwin32-gcc-core flexdll
  set CYGWIN_COMMANDS=%CYGWIN_COMMANDS% i686-pc-cygwin-gcc flexlink
)
if "%PORT%" equ "cygwin64" (
  set CYGWIN_PACKAGES=%CYGWIN_PACKAGES% gcc-core flexdll
  set CYGWIN_COMMANDS=%CYGWIN_COMMANDS% x86_64-pc-cygwin-gcc flexlink
)
if "%PORT:~0,6%%BOOTSTRAP_FLEXDLL%" equ "cygwinfalse" set CYGWIN_PACKAGES=%CYGWIN_PACKAGES% flexdll

set CYGWIN_INSTALL_PACKAGES=
set CYGWIN_UPGRADE_REQUIRED=%FORCE_CYGWIN_UPGRADE%

for %%P in (%CYGWIN_PACKAGES%) do call :CheckPackage %%P
call :UpgradeCygwin

"%CYG_ROOT%\bin\bash.exe" -lc "$APPVEYOR_BUILD_FOLDER/tools/ci/appveyor/appveyor_build.sh install" || exit /b 1

goto :EOF

:build
"%CYG_ROOT%\bin\bash.exe" -lc "$APPVEYOR_BUILD_FOLDER/tools/ci/appveyor/appveyor_build.sh" || exit /b 1
goto :EOF

:test
rem No tests run in the "C" build mode
if "%BUILD_MODE%" equ "C" goto :EOF
rem Add a C# compiler in PATH for the testsuite for mingw
if "%PORT%" equ "mingw64" call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\amd64\vcvars64.bat"
if "%PORT%" equ "mingw32" call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\vcvars32.bat"
"%CYG_ROOT%\bin\bash.exe" -lc "$APPVEYOR_BUILD_FOLDER/tools/ci/appveyor/appveyor_build.sh test" || exit /b 1
goto :EOF
