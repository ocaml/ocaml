#!/bin/sh -e

ARGS=
FILES=
while test "" != "$1"; do
	case $1 in
	*.ml*) FILES="$FILES $1";;
	*) ARGS="$ARGS $1";;
	esac
	shift
done

cat $FILES > tmp.ml
> tmp.mli
$OTOP/boot/ocamlrun$EXE $OTOP/boot/ocamlc -I $OTOP/boot -c tmp.mli
$OTOP/boot/ocamlrun$EXE ../meta/camlp4r$EXE -I ../meta pa_extend.cmo q_MLast.cmo -meta_action tmp.ml -o tmp.ppo
$OTOP/boot/ocamlrun$EXE $OTOP/boot/ocamlc -I $OTOP/boot -I ../lib -I ../camlp4 -c -impl tmp.ppo
rm tmp.ppo
$OTOP/boot/ocamlrun$EXE ../camlp4/camlp4$EXE ./tmp.cmo ./compile.cmo ../etc/pr_r.cmo ../etc/pr_rp.cmo $ARGS -sep "\n\n" -impl /dev/null
rm tmp.*
