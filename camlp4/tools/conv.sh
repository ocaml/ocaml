#!/bin/sh
DIR=`expr "$0" : "\(.*\)/.*" "|" "."`

INCL=
FILE=
while test "" != "$1"; do
        case $1 in
        -I) INCL="$INCL -I $2"; shift;;
        *) FILE=$1;;
        esac
        shift
done

set - `awk 'NR == 1' "$FILE"`
if test "$2" = "camlp4r" -o "$2" = "camlp4"; then
        COMM="$OTOP/boot/ocamlrun $DIR/../boot/$2 -nolib -I $DIR/../boot $INCL $DIR/../etc/pr_o.cmo"
        shift; shift
        ARGS=`echo $* | sed -e "s/[()*]//g"`
        $COMM $ARGS -ss $FILE
else
        cat $FILE
fi
