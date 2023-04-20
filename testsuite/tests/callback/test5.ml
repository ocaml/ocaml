(* TEST
 include unix;
 modules = "test5_.c";
 libunix;
 {
   bytecode;
 }{
   native;
 }
*)

(* Tests nested calls from C (main C) to OCaml (main OCaml) to C (caml_to_c) to
 * OCaml (c_to_caml) to C (printf functions). Test calls with arguments passed
 * on the stack from C to OCaml and OCaml to C. *)

let printf = Printf.printf

let c_to_caml n =
  printf "[Caml] Enter c_to_caml\n%!";
  printf "c_to_caml: n=%d\n" n;
  printf "[Caml] Leave c_to_caml\n%!"

let _ = Callback.register "c_to_caml" c_to_caml

external caml_to_c : int -> int -> int -> int -> int
                  -> int -> int -> int -> int -> int
                  -> int -> unit = "caml_to_c_bytecode" "caml_to_c_native"

let _ =
    printf "[Caml] Call caml_to_c\n%!";
    caml_to_c 1 2 3 4 5 6 7 8 9 10 11;
    printf "[Caml] Return from caml_to_c\n%!"
