#!/bin/sh
# $Id$

ARGS1=
FILE=
QUIET=no
while test "" != "$1"; do
        case $1 in
        -q) QUIET=yes;;
        *.ml*) FILE=$1;;
        *) ARGS1="$ARGS1 $1";;
        esac
        shift
done

# FILE must exist and be non empty (at least one line)
test -s "$FILE" || exit 1

set - `awk 'NR == 1' "$FILE"`
if test "$2" = "camlp4r" -o "$2" = "camlp4"; then
        COMM="ocamlrun$EXE ../boot/$2$EXE -nolib -I ../boot"
        if test "`basename $OTOP`" != "ocaml_stuff"; then
            COMM="$OTOP/boot/$COMM"
        fi
        shift; shift
        ARGS2=`echo $* | sed -e "s/[()*]//g"`
        ARGS1="$ARGS1 -verbose"
        if test "$QUIET" = "no"; then echo $COMM $ARGS2 $ARGS1 $FILE; fi
        $COMM $ARGS2 $ARGS1 $FILE
else
        if test "`basename $FILE .mli`.mli" = "$FILE"; then
                OFILE=`basename $FILE .mli`.ppi
        else
                OFILE=`basename $FILE .ml`.ppo
        fi
        if test "$QUIET" = "no"; then echo cp $FILE $OFILE; fi
        cp $FILE $OFILE
fi
