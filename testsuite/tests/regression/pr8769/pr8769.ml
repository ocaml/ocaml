(* TEST
modules = "nocrypto.mli fortuna.ml rng.ml"

* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "nocrypto.mli"
** ocamlc.byte
flags = "-for-pack Nocrypto"
module = "fortuna.ml"
** ocamlc.byte
flags = "-for-pack Nocrypto"
module = "rng.ml"
** ocamlc.byte
program = "nocrypto.cmo"
flags = "-pack"
all_modules = "fortuna.cmo rng.cmo"

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
module = "nocrypto.mli"
** ocamlopt.byte
flags = "-for-pack Nocrypto"
module = "fortuna.ml"
** ocamlopt.byte
flags = "-for-pack Nocrypto"
module = "rng.ml"
** ocamlopt.byte
program = "nocrypto.cmx"
flags = "-pack"
all_modules = "fortuna.cmx rng.cmx"

*)
