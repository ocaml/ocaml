#!/bin/sh
# $Id$

(
sed -e '/^EXTEND$/,$d' q_MLast.ml
echo EXTEND
../../boot/ocamlrun ../boot/camlp4r -I ../boot -I ../etc q_MLast.cmo pa_extend.cmo pr_r.cmo pr_extend.cmo -quotify pa_r.ml | sed -e '1,/^EXTEND$/d' -e '/^END;$/,$d'
echo '  (* Antiquotations for local entries *)'
sed -e '1,/Antiquotations for local entries/d' q_MLast.ml
)
