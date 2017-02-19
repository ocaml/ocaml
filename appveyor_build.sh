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

cd $APPVEYOR_BUILD_FOLDER

git worktree add ../build-mingw32 -b appveyor-build-mingw32

cd ../build-mingw32

git submodule update --init flexdll

cd $APPVEYOR_BUILD_FOLDER

cp config/m-nt.h config/m.h
cp config/s-nt.h config/s.h
cp config/Makefile.msvc64 config/Makefile

PREFIX="C:/Program Files/OCaml"
echo "Edit config/Makefile to set PREFIX=$PREFIX"
sed -i -e "s|PREFIX=.*|PREFIX=$PREFIX|" config/Makefile
#run "Content of config/Makefile" cat config/Makefile

run "make world" make world
run "make bootstrap" make bootstrap
run "make opt" make opt
run "make opt.opt" make opt.opt

cd ../build-mingw32

cp config/m-nt.h config/m.h
cp config/s-nt.h config/s.h
cp config/Makefile.mingw config/Makefile

PREFIX="C:/Program Files/OCaml-mingw32"
echo "Edit config/Makefile to set PREFIX=$PREFIX"
sed -i -e "s|PREFIX=.*|PREFIX=$PREFIX|" config/Makefile
#run "Content of config/Makefile" cat config/Makefile

run "make flexdll" make flexdll
run "make world.opt" make world.opt
