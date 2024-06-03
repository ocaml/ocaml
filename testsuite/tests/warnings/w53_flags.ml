(* TEST
   readonly_files = "w53.ml";
   setup-ocamlc.byte-build-env;
   module = "w53.ml";

   (* We don't issue warning 53 when -i is passed *)
   {
     flags = "-warn-error +53 -i -w +A-22-27-32-60-67-70-71-72";
     compile_only = "true";
     ocamlc_byte_exit_status = "0";
     ocamlc.byte;
   }

   (* We don't issue warning 53 when -stop-after parsing or -stop-after typing
      is passed *)
   {
     flags =
       "-warn-error +53 -stop-after parsing -w +A-22-27-32-60-67-70-71-72";
     compile_only = "true";
     ocamlc_byte_exit_status = "0";
     ocamlc.byte;
   }
   {
     flags =
       "-warn-error +53 -stop-after typing -w +A-22-27-32-60-67-70-71-72";
     compile_only = "true";
     ocamlc_byte_exit_status = "0";
     ocamlc.byte;
   }

   (* We do issue warning 53 when -stop-after lambda (or later) is passed *)
   {
     flags =
       "-warn-error +53 -stop-after lambda -w +A-22-27-32-60-67-70-71-72";
     compile_only = "true";
     ocamlc_byte_exit_status = "2";
     ocamlc.byte;
   }
*)
