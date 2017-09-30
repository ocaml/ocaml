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

BUILD_PID=0

function run {
    NAME=$1
    shift
    echo "-=-=- $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    $@
    CODE=$?
    if [ $CODE -ne 0 ]; then
        echo "-=-=- $NAME failed! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
        if [ $BUILD_PID -ne 0 ] ; then
          kill -KILL $BUILD_PID 2>/dev/null
          wait $BUILD_PID 2>/dev/null
        fi
        exit $CODE
    else
        echo "-=-=- End of $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    fi
}

PREFIX=$(echo $OCAMLROOT| cygpath -f - -m)
APPVEYOR_BUILD_FOLDER=$(echo $APPVEYOR_BUILD_FOLDER| cygpath -f -)

case "$1" in
  install)
    mkdir -p "$PREFIX/bin/flexdll"
    cd $APPVEYOR_BUILD_FOLDER/../flexdll
    for f in flexdll.h flexlink.exe default_amd64.manifest ; do
      cp $f "$PREFIX/bin/flexdll/$f"
    done
    echo 'eval $($APPVEYOR_BUILD_FOLDER/tools/msvs-promote-path)' >> ~/.bash_profile
    ;;
  msvc32-only)
#    cd $APPVEYOR_BUILD_FOLDER/flexdll-0.35
#    make MSVC_DETECT=0 CHAINS=msvc MSVC_FLAGS="-nologo -MD -D_CRT_NO_DEPRECATE -GS- -WX" support
#    cp flexdll*_msvc.obj "$PREFIX/bin/flexdll"

    cd $APPVEYOR_BUILD_FOLDER/../build-msvc32
    cp config/m-nt.h byterun/caml/m.h
    cp config/s-nt.h byterun/caml/s.h

    PREFIX="C:/Program Files/OCaml-msvc32"
    echo "Edit config/Makefile to set PREFIX=$PREFIX"
    sed -e "s|PREFIX=.*|PREFIX=$PREFIX|" -e "/^ *CFLAGS *=/s/\r\?$/ -WX\0/" config/Makefile.msvc > config/Makefile

    # Temporarily bootstrap flexdll
    run "make flexdll" make flexdll
    run "make world" make world
    run "make runtimeopt" make runtimeopt
    run "make -C otherlibs/systhreads libthreadsnat.lib" make -C otherlibs/systhreads libthreadsnat.lib

    exit 0
    ;;
  test)
    run "test msvc64" make -C $APPVEYOR_BUILD_FOLDER tests
    run "test mingw32" make -C $APPVEYOR_BUILD_FOLDER/../build-mingw32 tests
    run "install msvc64" make -C $APPVEYOR_BUILD_FOLDER install
    run "install mingw32" make -C $APPVEYOR_BUILD_FOLDER/../build-mingw32 install
    ;;
  *)
    cd $APPVEYOR_BUILD_FOLDER

    # tar -xzf flexdll.tar.gz
    # cd flexdll-0.35
    # make MSVC_DETECT=0 CHAINS=msvc64 support
    # cp flexdll*_msvc64.obj "$PREFIX/bin/flexdll"
    # cd ..

    cp config/m-nt.h byterun/caml/m.h
    cp config/s-nt.h byterun/caml/s.h

    echo "Edit config/Makefile to set PREFIX=$PREFIX"
    sed -e "s|PREFIX=.*|PREFIX=$PREFIX|" -e "/^ *CFLAGS *=/s/\r\?$/ -WX\0/" config/Makefile.msvc64 > config/Makefile
    #run "Content of config/Makefile" cat config/Makefile

    cd ../build-mingw32

    cp config/m-nt.h byterun/caml/m.h
    cp config/s-nt.h byterun/caml/s.h

    PREFIX=$(echo $OCAMLROOT2| cygpath -f - -m)
    echo "Edit config/Makefile to set PREFIX=$PREFIX"
    sed -e "s|PREFIX=.*|PREFIX=$PREFIX|" -e "/^ *CFLAGS *=/s/\r\?$/ -Werror\0/" config/Makefile.mingw > config/Makefile
    #run "Content of config/Makefile" cat config/Makefile

    cd $APPVEYOR_BUILD_FOLDER

    export TERM=ansi
    script --quiet --return --command "make -C ../build-mingw32 flexdll world.opt" ../build-mingw32/build.log >/dev/null 2>/dev/null &
    BUILD_PID=$!

    # Temporarily bootstrap flexdll
    run "make flexdll" make flexdll
    run "make world" make world
    run "make bootstrap" make bootstrap
    run "make opt" make opt
    run "make opt.opt" make opt.opt

    # For an explanation of the sed command, see https://github.com/appveyor/ci/issues/1824
    (tail --pid=$BUILD_PID -n +1 -f ../build-mingw32/build.log & echo $! >&3) 3>pid | sed -e 's/\d027\[K//g' -e 's/\d027\[m/\d027[0m/g' -e 's/\d027\[01\([m;]\)/\d027[1\1/g' &
    TAIL_PID=$(<pid)
    wait $BUILD_PID
    STATUS=$?
    wait $TAIL_PID 2>/dev/null
    exit $STATUS
    ;;
esac
