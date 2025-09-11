(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let foo x = function
  | 0 .. 10 -> ()
  | _ -> begin match x with
      | "foo" .. "bar" -> ()
      | _ ->
          for (Some i) = 1 to 10 do
            print_endline "Message"
          done
    end

let f () = 10

let g = f (foo "foo" 10)
