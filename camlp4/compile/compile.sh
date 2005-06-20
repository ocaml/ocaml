#!/bin/sh -e

ARGS=
FILES=
ENTRIES=
while test "" != "$1"; do
        case $1 in
        -e)
           shift;
           if test "$ENTRIES" != ""; then ENTRIES="$ENTRIES; "; fi
           ENTRIES="$ENTRIES$1";;
        *.ml*) FILES="$FILES $1";;
        *) ARGS="$ARGS $1";;
        esac
        shift
done

cat $FILES | sed -e 's/Pcaml.parse_i.*$//' > tmp.ml
echo "Compile.entries.val := [$ENTRIES];" >> tmp.ml
> tmp.mli
$OTOP/boot/ocamlrun$EXE $OTOP/boot/ocamlc -I $OTOP/boot -c tmp.mli
$OTOP/boot/ocamlrun$EXE ../meta/camlp4r$EXE -I ../meta pa_extend.cmo q_MLast.cmo -meta_action tmp.ml -o tmp.ppo
$OTOP/boot/ocamlrun$EXE $OTOP/boot/ocamlc -I $OTOP/boot -I ../lib -I ../camlp4 -c -impl tmp.ppo
rm tmp.ppo
> tmp.null
$OTOP/boot/ocamlrun$EXE ../camlp4/camlp4$EXE ./compile.cmo ./tmp.cmo ../etc/pr_r.cmo ../etc/pr_rp.cmo $ARGS -sep "\n\n" -impl tmp.null && rm tmp.*
