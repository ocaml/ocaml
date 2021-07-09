(* TEST
readonly_files = "mapping.ml range_intf.ml ranged_intf.ml range.ml ranged.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-no-alias-deps -w -49 -o Pr8548__Mapping"
module = "mapping.ml"
** ocamlc.byte
flags = "-no-alias-deps -open Pr8548__Mapping -o pr8548__Range_intf.cmo"
module = "range_intf.ml"
*** ocamlc.byte
flags = "-no-alias-deps -open Pr8548__Mapping -o pr8548__Range.cmo"
module = "range.ml"
*** ocamlc.byte
flags = "-no-alias-deps -open Pr8548__Mapping -o pr8548__Ranged_intf.cmo"
module = "ranged_intf.ml"
**** ocamlc.byte
flags = "-no-alias-deps -open Pr8548__Mapping -o pr8548__Ranged.cmo"
module = "ranged.ml"
ocamlc_byte_exit_status = "0"
*)
