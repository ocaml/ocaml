(* TEST
modules = "stub.c"
* pass
** bytecode
** native
* pass
flags = "-ccopt -DCAML_NAME_SPACE"
** bytecode
** native
*)

external retrieve_young_limit : 'a -> nativeint = "retrieve_young_limit"

let bar =
  let foo = Bytes.create 4 in
  retrieve_young_limit foo
