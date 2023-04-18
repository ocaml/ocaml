(* TEST_BELOW
                       
              
                                             
                                 
                            
*)

(* we intentionally write ill-typed output;
   if `-stop-after parsing` was not supported properly,
   the test would fail with an error *)
let _ = (1 + "true") + x

(* TEST
{
  setup-ocamlc.byte-build-env;

  flags = "-stop-after parsing -dparsetree";
  ocamlc_byte_exit_status = "0";
  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
