(* TEST_BELOW
             
                             
              
                                                                                   
                            
                    
                                                                          

                             
              
                                                                                
                            
                    
                                                                       
*)
let rec fib = function
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

(* TEST
{
  compile_only = "true";
  {
    setup-ocamlc.byte-build-env;

    flags = "-g -dno-unique-ids -dno-locations -dsource -dparsetree -dtypedtree -dlambda";
    ocamlc.byte;

    compiler_reference = "${test_source_directory}/test_locations.dno-locations.ocamlc.reference";
    check-ocamlc.byte-output;
  }{
    setup-ocamlc.byte-build-env;

    flags = "-g -dno-unique-ids -dlocations -dsource -dparsetree -dtypedtree -dlambda";
    ocamlc.byte;

    compiler_reference = "${test_source_directory}/test_locations.dlocations.ocamlc.reference";
    check-ocamlc.byte-output;
  }
}
*)
