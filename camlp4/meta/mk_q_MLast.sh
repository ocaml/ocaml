#!/bin/sh
# $Id$

IFILE=pa_r.ml
OFILE=q_MLast.ml
(
sed -e '/^EXTEND$/,$d' $OFILE
echo EXTEND
../../boot/ocamlrun ./camlp4r -I . -I ../etc q_MLast.cmo pa_extend.cmo pr_r.cmo pr_extend.cmo -quotify $IFILE | sed -e '1,/^EXTEND$/d' -e '/^END;$/,$d'
echo '  (* Antiquotations for local entries *)'
sed -e '1,/Antiquotations for local entries/d' $OFILE
)
