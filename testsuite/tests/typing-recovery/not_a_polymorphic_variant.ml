(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type t = int

let f = function #t -> assert false
let e x =
  let r = match x with
    | #t -> 10
    | _ -> 11
  in
  r + 10

module T = struct
  let e x =
    let r = match x with
      | #t -> 10
      | _ -> 11
    in
    r + 10
end

let u = (e 10) + (T.e 10) + 7

let t : string = u
