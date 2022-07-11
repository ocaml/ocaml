(* TEST
   include unix
   modules = "test7_.c"
   * libunix
   ** bytecode
   ** native
*)

(* Tests nested calls from C (main C) to OCaml (main OCaml) to C (caml_to_c) to
 * OCaml (c_to_caml) to C (printf functions). Effect E is performed in the
 * callback, which does not have a handler. *)

open Effect
open Effect.Deep

type _ t += E : unit t

let printf = Printf.printf

let c_to_caml () =
  printf "[Caml] Enter c_to_caml\n%!";
  printf "[Caml] c_to_caml: perform effect\n%!";
  perform E

let _ = Callback.register "c_to_caml" c_to_caml

external caml_to_c : unit -> unit = "caml_to_c"

let _ =
  try_with (fun () ->
    printf "[Caml] Call caml_to_c\n%!";
    begin try
      caml_to_c ()
    with Unhandled_effect E ->
      (printf "[Caml] Caught Unhandled_effect, perform effect\n%!";
       perform E)
    end;
    printf "[Caml] Return from caml_to_c\n%!") ()
  { effc = fun (type a) (e : a t) ->
      match e with
      | E -> Some (fun k -> printf "[Caml] Caught effect\n%!")
      | _ -> None }
