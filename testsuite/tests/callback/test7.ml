(* TEST
 include unix;
 modules = "test7_.c";
 libunix;
 {
   bytecode;
 }{
   native;
 }
*)

(* Tests nested calls from C (main C) to OCaml (main OCaml) to C (caml_to_c) to
 * OCaml (c_to_caml) to C (printf functions). Effect E is performed in the
 * callback, which does not have a handler. *)

open Effect

type eff = effect E : unit

let eff = Effect.create ()

let printf = Printf.printf

let c_to_caml () =
  printf "[Caml] Enter c_to_caml\n%!";
  printf "[Caml] c_to_caml: perform effect\n%!";
  perform eff E

let _ = Callback.register "c_to_caml" c_to_caml

external caml_to_c : unit -> unit = "caml_to_c"

let _ =
  run_with eff (fun () ->
    printf "[Caml] Call caml_to_c\n%!";
    begin try
      caml_to_c ()
    with Effect.Unhandled(e, op) as exn ->
      match Effect.equal e eff with
      | None -> raise exn
      | Some Equal ->
          printf "[Caml] Caught Effect.Unhandled, perform effect\n%!";
          perform eff E
    end;
    printf "[Caml] Return from caml_to_c\n%!") ()
  { result = Fun.id;
    exn = raise;
    operation =
      (fun (type a) (E : (a, eff) operation) k ->
        printf "[Caml] Caught effect\n%!") }
