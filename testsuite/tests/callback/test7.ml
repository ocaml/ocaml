(* Tests nested calls from C (main C) to OCaml (main OCaml) to C (caml_to_c) to
 * OCaml (c_to_caml) to C (printf functions). Effect E is performed in the
 * callback, which does not have a handler. *)

effect E : unit

let printf = Printf.printf

let c_to_caml () =
  printf "[Caml] Enter c_to_caml\n%!";
  printf "[Caml] c_to_caml: perform effect\n%!";
  perform E

let _ = Callback.register "c_to_caml" c_to_caml

external caml_to_c : unit -> unit = "caml_to_c"

let _ =
  try
    printf "[Caml] Call caml_to_c\n%!";
    begin try
      caml_to_c ()
    with Unhandled ->
      (printf "[Caml] Caught Unhandled, perform effect\n%!";
       perform E)
    end;
    printf "[Caml] Return from caml_to_c\n%!"
  with effect E k -> printf "[Caml] Caught effect\n%!"
