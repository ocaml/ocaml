(* TEST
 include unix;
 hasunix;
 not-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* on Multicore, fork is not allowed is another domain is, and was running. *)
(* this test checks that we can't fork if another domain ran before. *)

let () =
  let d = Domain.spawn (fun () -> ()) in
  Domain.join d;
  let res = match Unix.fork () with
    | exception Failure _ -> 0
    | _ -> 1
  in
  exit res
