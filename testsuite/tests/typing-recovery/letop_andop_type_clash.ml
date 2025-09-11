(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)


module A = struct
  let (let+) = 7
  let (and+) = not

  let ill_typed =
    let+ x = 1 and+ y = 11 in x + y
end

module F = struct
  let (let+) x f = f x
  let (and+) = not

  let ill_typed =
    let+ x = 1 and+ y = 11 in x + y
end
