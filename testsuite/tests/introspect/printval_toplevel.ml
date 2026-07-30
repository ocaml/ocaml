(* TEST
   readonly_files = "printval.ml";
   {
     introspect;
     reference = "${test_source_directory}/printval_toplevel.compilers.reference";
   }{
     no-introspect;
     reference = "${test_source_directory}/printval_toplevel.compilers.no-introspect.reference";
   }{
     toplevel;
   }{
     toplevel.opt;
   }
*)

#use "printval.ml";;
