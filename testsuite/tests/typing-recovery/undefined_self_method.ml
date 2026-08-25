(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let _ =
  (object(self)
    method foo = self # bar
  end) # foo

let _ =
  (object(self)
    method bar = object
      method foo = 10
      end
    method foo = self # bar
  end) # foo # foo + 10

let t : bool = 10
