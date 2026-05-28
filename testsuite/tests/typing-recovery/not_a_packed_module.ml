(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let f x =
  x + 1

let x =
  let a = f (module struct end)
  and b = f 10 in
  a + b

module U = struct
  let x =
    let a = f (module struct end)
    and b = f 10 in
    a + b
end

let t : string = x + U.x
