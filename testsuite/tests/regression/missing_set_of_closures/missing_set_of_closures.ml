(* TEST
files = "a.ml b.ml b2.ml"
* setup-ocamlopt.byte-build-env
** script 
script = "mkdir -p dir"
*** script
script = "cp ${test_source_directory}/dir/c.ml dir/"
**** ocamlopt.byte
module = "a.ml"
***** ocamlopt.byte
module = "b.ml"
****** ocamlopt.byte
module = "b2.ml"
******* script
script = "cp b.cmx b.cmi b2.cmx b2.cmi dir/"
******** cd
cwd = "dir"
********* ocamlopt.byte
module = "c.ml"
flags = "-w -58"
********** check-ocamlopt.byte-output
*)
