(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

module R = struct
  let f =
    let x = ([`B] :> [`A]) in
    `A :: x

  let r = `A :: f
  let t : string = 10
end

let t : string = 10
let _ = t ^ R.t
