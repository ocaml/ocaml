(* TEST

subdirectories = "liba libb libc"

compile_only = "true"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-I liba -nocwd"
all_modules = "liba/a.ml"

** ocamlc.byte
flags = "-I liba -I libb -nocwd"
all_modules = "libb/b.ml"

*** ocamlc.byte
(* Test hiding completely A cmi *)
flags = "-I libb -nocwd"
all_modules = "libc/c2.ml"

*** ocamlc.byte
(* Test hiding completely A cmi but use it *)
flags = "-I libb -nocwd"
all_modules = "libc/c.ml"
ocamlc_byte_exit_status = "2"

*** ocamlc.byte
(* Test showing A cmi *)
flags = "-I liba -I libb -nocwd"
all_modules = "libc/c.ml"

*** ocamlc.byte
(* Test hidding A cmi *)
flags = "-H liba -I libb -nocwd"
all_modules = "libc/c.ml"

*** ocamlc.byte
(* Test hidding A cmi and using it *)
flags = "-H liba -I libb -nocwd"
all_modules = "libc/d.ml"
ocamlc_byte_exit_status = "2"

*)
