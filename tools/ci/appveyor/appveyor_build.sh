#!/usr/bin/env bash
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

set -e

BUILD_PID=0

# This must correspond with the entry in appveyor.yml
CACHE_DIRECTORY=/cygdrive/c/projects/cache

if [[ -z $APPVEYOR_PULL_REQUEST_HEAD_COMMIT ]] ; then
  MAKE="make -j"
else
  MAKE=make
fi

function run {
    if [[ $1 = "--show" ]] ; then SHOW_CMD='true'; shift; else SHOW_CMD=''; fi
    NAME=$1
    shift
    echo "-=-=- $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    if [[ -n $SHOW_CMD ]]; then (set -x; "$@"); else "$@"; fi
    CODE=$?
    if [[ $CODE -ne 0 ]] ; then
        echo "-=-=- $NAME failed! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
        if [[ $BUILD_PID -ne 0 ]] ; then
          kill -KILL $BUILD_PID 2>/dev/null
          wait $BUILD_PID 2>/dev/null
        fi
        exit $CODE
    else
        echo "-=-=- End of $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    fi
}

# Function: set_configuration
# Takes 3 arguments
# $1:the Windows port. Recognized values: mingw, msvc and msvc64
# $2: the prefix to use to install
function set_configuration {
    case "$1" in
        mingw)
            build='--build=i686-pc-cygwin'
            host='--host=i686-w64-mingw32'
            dep='--disable-dependency-generation'
        ;;
        msvc)
            build='--build=i686-pc-cygwin'
            host='--host=i686-pc-windows'
            dep='--disable-dependency-generation'
        ;;
        msvc64)
            build='--build=x86_64-unknown-cygwin'
            host='--host=x86_64-pc-windows'
            # Explicitly test dependency generation on msvc64
            dep='--enable-dependency-generation'
        ;;
    esac

    mkdir -p "$CACHE_DIRECTORY"
    ./configure --cache-file="$CACHE_DIRECTORY/config.cache-$1" \
                $dep $build $host --prefix="$2" --enable-ocamltest || ( \
      rm -f "$CACHE_DIRECTORY/config.cache-$1" ; \
      ./configure --cache-file="$CACHE_DIRECTORY/config.cache-$1" \
                  $dep $build $host --prefix="$2" --enable-ocamltest )

#    FILE=$(pwd | cygpath -f - -m)/Makefile.config
#    run "Content of $FILE" cat Makefile.config
}

APPVEYOR_BUILD_FOLDER=$(echo "$APPVEYOR_BUILD_FOLDER" | cygpath -f -)
# These directory names are specified here, because getting UTF-8 correctly
# through appveyor.yml -> Command Script -> Bash is quite painful...
OCAMLROOT=$(echo "$PROGRAMFILES/Бактріан🐫" | cygpath -f - -m)

# This must be kept in sync with appveyor_build.cmd
BUILD_PREFIX=🐫реализация

PATH=$(echo "$OCAMLROOT" | cygpath -f -)/bin/flexdll:$PATH

case "$1" in
  install)
    mkdir -p "$OCAMLROOT/bin/flexdll"
    cd "$APPVEYOR_BUILD_FOLDER/../flexdll"
    # msvc64 objects need to be compiled with VS2015, so are copied later from
    # a source build.
    for f in flexdll.h flexlink.exe flexdll*_msvc.obj default*.manifest ; do
      cp "$f" "$OCAMLROOT/bin/flexdll/"
    done
    if [[ $PORT = 'msvc64' ]] ; then
      echo 'eval $($APPVEYOR_BUILD_FOLDER/tools/msvs-promote-path)' \
        >> ~/.bash_profile
    fi
    ;;
  msvc32-only)
    cd "$APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX-msvc32"

    set_configuration msvc "$OCAMLROOT-msvc32"

    run "$MAKE world" $MAKE world
    run "$MAKE runtimeopt" $MAKE runtimeopt
    run "$MAKE -C otherlibs/systhreads libthreadsnat.lib" \
         $MAKE -C otherlibs/systhreads libthreadsnat.lib

    exit 0
    ;;
  test)
    FULL_BUILD_PREFIX="$APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX"
    run 'ocamlc.opt -version' "$FULL_BUILD_PREFIX-$PORT/ocamlc.opt" -version
    if [[ $PORT = 'mingw32' ]] ; then
      run "Check runtime symbols" \
          "$FULL_BUILD_PREFIX-$PORT/tools/check-symbol-names" \
          $FULL_BUILD_PREFIX-$PORT/runtime/*.a
    fi
    run "test $PORT" $MAKE -C "$FULL_BUILD_PREFIX-$PORT" tests
    run "install $PORT" $MAKE -C "$FULL_BUILD_PREFIX-$PORT" install
    if [[ $PORT = 'msvc64' ]] ; then
      run "$MAKE check_all_arches" \
           $MAKE -C "$FULL_BUILD_PREFIX-$PORT" check_all_arches
      cd "$FULL_BUILD_PREFIX-$PORT"
      # Ensure that .gitignore is up-to-date - this will fail if any untracked
      # or altered files exist. We revert the change from the bootstrap (that
      # would have failed the build earlier if necessary)
      git checkout -- boot/ocamlc boot/ocamllex
      # Remove the FlexDLL sources placed earlier in the process
      rm -rf "flexdll-$FLEXDLL_VERSION"
      run --show "Check tree is tracked" test -z "$(git status --porcelain)"
      # check that the `distclean` target definitely cleans the tree
      run "$MAKE distclean" $MAKE distclean
      # Check the working tree is clean
      run --show "Check tree is tracked" test -z "$(git status --porcelain)"
      # Check that there are no ignored files
      run --show "Check tree is clean" \
        test -z "$(git ls-files --others -i --exclude-standard)"
    fi
    ;;
  *)
    cd "$APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX-$PORT"

    if [[ $PORT = 'msvc64' ]] ; then
      # Ensure that make distclean can be run from an empty tree
      run "$MAKE distclean" $MAKE distclean
      tar -xzf "$APPVEYOR_BUILD_FOLDER/flexdll.tar.gz"
      cd "flexdll-$FLEXDLL_VERSION"
      $MAKE MSVC_DETECT=0 CHAINS=msvc64 support
      cp flexdll*_msvc64.obj "$OCAMLROOT/bin/flexdll/"
      cd ..
    fi

    if [[ $PORT = 'msvc64' ]] ; then
      set_configuration msvc64 "$OCAMLROOT"
    else
      set_configuration mingw "$OCAMLROOT-mingw32"
    fi

    cd "$APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX-$PORT"

    export TERM=ansi

    if [[ $PORT = 'mingw32' ]] ; then
      set -o pipefail
      # For an explanation of the sed command, see
      # https://github.com/appveyor/ci/issues/1824
      script --quiet --return --command \
        "$MAKE -C ../$BUILD_PREFIX-mingw32 flexdll && "\
"$MAKE -C ../$BUILD_PREFIX-mingw32 world.opt" \
        "../$BUILD_PREFIX-mingw32/build.log" |
          sed -e 's/\d027\[K//g' \
              -e 's/\d027\[m/\d027[0m/g' \
              -e 's/\d027\[01\([m;]\)/\d027[1\1/g'
    else
      run "$MAKE world" $MAKE world
      run "$MAKE bootstrap" $MAKE bootstrap
      run "$MAKE opt" $MAKE opt
      run "$MAKE opt.opt" $MAKE opt.opt
    fi

    ;;
esac
