(* TEST
   flags = "-g"
   ocamlrunparam += ",b=1"
   compare_programs = "false"
*)

(* A test for stack backtraces *)

exception Error of string

let rec f msg n =
  if n = 0 then raise(Error msg) else 1 + f msg (n-1)

let g msg =
  match
    f msg 5
  with
  | _ ->
     (* value return does not happen *)
     assert false
  | exception (Error "a") ->
      print_string "a"; print_newline(); 0
  | exception (Error "b" as exn) ->
      (* this should Re-raise, appending to the current backtrace *)
      print_string "b"; print_newline(); raise exn
  | exception (Error "c") ->
      (* according to the current re-raise policy (a static condition),
         this does not re-raise *)
      print_string "c"; print_newline(); raise (Error "c")
  | exception (Error "d" as exn as _exn2) ->
      (* this should Re-raise, appending to the current backtrace *)
      print_string "d"; print_newline(); raise exn
  | exception (Error "e" as _exn as exn2) ->
      (* this should Re-raise, appending to the current backtrace *)
      print_string "e"; print_newline(); raise exn2
  | exception (exn as exn2) ->
      match exn with
      | Error "f" ->
          (* this should Re-raise, appending to the current backtrace *)
          print_string "f"; print_newline(); raise exn
      | Error "g" ->
          (* this should Re-raise, appending to the current backtrace *)
          print_string "g"; print_newline(); raise exn2
      | x ->
          (* this should *not* Re-raise *)
          raise x

let run args =
  try
    ignore (g args.(0)); print_string "No exception\n"
  with exn ->
    Printf.printf "Uncaught exception %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stdout

let _ =
  Printexc.record_backtrace true;
  run [| "a" |];
  run [| "b" |];
  run [| "c" |];
  run [| "d" |];
  run [| "e" |];
  run [| "f" |];
  run [| "g" |];
  run [| "h" |];
  run [| |]
