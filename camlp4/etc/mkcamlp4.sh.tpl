#!/bin/sh
# $Id$

OLIB="`ocamlc -where`"
LIB="LIBDIR/camlp4"

# automatically define VERSION here:

INTERFACES=
OPTS=
INCL="-I ."
while test "" != "$1"; do
    case "$1" in
    -I) INCL="$INCL -I $2"; shift;;
    -version) echo "mkcamlp4, version $VERSION"; exit;;
    [a-zA-Z]*.cmi)
        j=`basename "$1" .cmi`
        first="`expr "$j" : '\(.\)' | tr 'a-z' 'A-Z'`"
        rest="`expr "$j" : '.\(.*\)'`"
        INTERFACES="$INTERFACES $first$rest"
        ;;
    *) OPTS="$OPTS $1";;
    esac
    shift
done

CRC=crc_$$
set -e
trap 'rm -f $CRC.ml $CRC.cmi $CRC.cmo' 0 2
$OLIB/extract_crc -I $OLIB $INCL $INTERFACES > $CRC.ml
echo "let _ = Dynlink.add_available_units crc_unit_list" >> $CRC.ml
ocamlc -I $LIB odyl.cma camlp4.cma $CRC.ml $INCL $OPTS odyl.cmo -linkall
rm -f $CRC.ml $CRC.cmi $CRC.cmo

