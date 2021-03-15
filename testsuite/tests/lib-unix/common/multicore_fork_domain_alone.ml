(* TEST
include unix
* hasunix
** bytecode
** native
*)

(* on Multicore, fork is not allowed is another domain is, and was running. *)
(* this test checks that we can't fork if a domain is currently running. *)

let expect_exn ="Unix.fork may not be called while other domains were created"

let () =
  let _ = Domain.spawn (fun () -> Unix.sleep 1) in
  match Unix.fork () with
  | exception Failure msg ->
     if String.equal msg expect_exn then
       print_endline "OK"
     else
       Printf.printf "failed: expected Failure: %s, got %s\n" expect_exn msg
  | _ -> print_endline "NOK"
