(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let id x = x

let a ?(id : 'a. 'a -> 'a) () = id 3, id "three"
let b ?(id : 'a. int -> int) () = id 3

let r =
  let x = fst (a ~id ())
  and y = (b ~id ()) in
  x + y

let t : string = r
