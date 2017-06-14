#!/bin/bash
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                         Christophe Troestler                           *
#*                                                                        *
#*   Copyright 2015 Christophe Troestler                                  *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

function run {
    NAME=$1
    shift
    echo "-=-=- $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    $@
    CODE=$?
    if [ $CODE -ne 0 ]; then
        echo "-=-=- $NAME failed! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
        exit $CODE
    else
        echo "-=-=- End of $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    fi
}

PREFIX="C:/Program Files/OCaml"

wmic cpu get name

if [[ $1 = "msvc32-only" ]] ; then
  cd $APPVEYOR_BUILD_FOLDER/flexdll-0.35
  make MSVC_DETECT=0 CHAINS=msvc MSVC_FLAGS="-nologo -MD -D_CRT_NO_DEPRECATE -GS- -WX" support
  cp flexdll*_msvc.obj "$PREFIX/bin/flexdll"

  cd $APPVEYOR_BUILD_FOLDER/../build-msvc32
  cp config/m-nt.h config/m.h
  cp config/s-nt.h config/s.h

  eval $(tools/msvs-promote-path)

  PREFIX="C:/Program Files/OCaml-msmvc32"
  echo "Edit config/Makefile to set PREFIX=$PREFIX"
  sed -e "s|PREFIX=.*|PREFIX=$PREFIX|" -e "/\(BYTE\|NATIVE\)CCCOMPOPTS=./s/\r\?$/ -WX\0/" config/Makefile.msvc > config/Makefile

  run "make world" make world
  run "make runtimeopt" make runtimeopt
  run "make -C otherlibs/systhreads libthreadsnat.lib" make -C otherlibs/systhreads libthreadsnat.lib

  exit 0
fi

cd $APPVEYOR_BUILD_FOLDER

git worktree add ../build-mingw32 -b appveyor-build-mingw32
git worktree add ../build-msvc32 -b appveyor-build-msvc32

cd ../build-mingw32
git submodule update --init flexdll

cd $APPVEYOR_BUILD_FOLDER

tar -xzf flexdll.tar.gz
cd flexdll-0.35
make MSVC_DETECT=0 CHAINS=msvc64 support
cp flexdll*_msvc64.obj "$PREFIX/bin/flexdll"
cd ..

cp config/m-nt.h config/m.h
cp config/s-nt.h config/s.h

echo "Edit config/Makefile to set PREFIX=$PREFIX"
sed -e "s|PREFIX=.*|PREFIX=$PREFIX|" -e "/\(BYTE\|NATIVE\)CCCOMPOPTS=./s/\r\?$/ -WX\0/" config/Makefile.msvc64 > config/Makefile
#run "Content of config/Makefile" cat config/Makefile

run "make world" make world
run "make bootstrap" make bootstrap
run "make opt" make opt
run "make opt.opt" make opt.opt

cd ../build-mingw32

cp config/m-nt.h config/m.h
cp config/s-nt.h config/s.h

PREFIX="C:/Program Files/OCaml-mingw32"
echo "Edit config/Makefile to set PREFIX=$PREFIX"
sed -e "s|PREFIX=.*|PREFIX=$PREFIX|" -e "/\(BYTE\|NATIVE\)CCCOMPOPTS=./s/\r\?$/ -Werror\0/" config/Makefile.mingw > config/Makefile
#run "Content of config/Makefile" cat config/Makefile

run "make flexdll" make flexdll
run "make world.opt" make world.opt
