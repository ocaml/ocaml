#!/bin/sh
# $Id$

ARGS1=
FILE=
while test "" != "$1"; do
        case $1 in
        *.ml*) FILE=$1;;
        *) ARGS1="$ARGS1 $1";;
        esac
        shift
done

head -1 $FILE >/dev/null 2>&1 || exit 1

set - `head -1 $FILE`
if test "$2" = "camlp4r" -o "$2" = "camlp4"; then
        COMM="../boot/$2 -nolib -I ../boot -I ../etc"
        shift; shift
        ARGS2=`echo $* | sed -e "s/[()*]//g"`
else
        COMM="../boot/camlp4 -nolib -I ../boot -I ../etc pa_o.cmo"
        ARGS2=
fi

OTOP=../..
echo $OTOP/boot/ocamlrun $COMM $ARGS2 $ARGS1 $FILE 1>&2
$OTOP/boot/ocamlrun $COMM $ARGS2 $ARGS1 $FILE
