#!/bin/sh
# $Id$

P4TOP=..
ARGS1=
FILE=
while test "" != "$1"; do
        case $1 in
        *.ml*) FILE=$1;;
        -top) P4TOP="$2"; shift;;
        *) ARGS1="$ARGS1 $1";;
        esac
        shift
done

# FILE must exist and be non empty (at least one line)
test -s "$FILE" || exit 1



set - `awk 'NR == 1' "$FILE"`
if test "$2" = "camlp4r" -o "$2" = "camlp4"; then
        COMM="$P4TOP/boot/$2 -nolib -I $P4TOP/boot -I $P4TOP/etc"
        shift; shift
        ARGS2=`echo $* | sed -e "s/[()*]//g"`
else
        COMM="$P4TOP/boot/camlp4 -nolib -I $P4TOP/boot -I $P4TOP/etc pa_o.cmo"
        ARGS2=
fi

OTOP=$P4TOP/..
echo $OTOP/boot/ocamlrun $COMM $ARGS2 $ARGS1 $FILE 1>&2
$OTOP/boot/ocamlrun $COMM $ARGS2 $ARGS1 $FILE
