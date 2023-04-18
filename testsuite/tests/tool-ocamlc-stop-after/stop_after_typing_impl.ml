(* TEST_BELOW
                       
              
                                                            
                                 
                            
*)

(* we intentionally write an output that is type-correct
   but will be rejected before bytecode compilation
   due to the incorrect type given to the %apply
   compiler primitive. *)
external apply: int -> int = "%apply"

(* TEST
{
  setup-ocamlc.byte-build-env;

  flags = "-stop-after typing -dno-unique-ids -dtypedtree";
  ocamlc_byte_exit_status = "0";
  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
