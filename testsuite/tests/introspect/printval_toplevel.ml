(* TEST
   script = "sh ${test_source_directory}/has-introspect.sh";
   script;
   readonly_files = "printval.ml";
   {
     toplevel;
   }{
     toplevel.opt;
   }
*)

#use "printval.ml";;
