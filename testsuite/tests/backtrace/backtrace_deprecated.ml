(* TEST
   flags = "-g"
   ocamlrunparam += ",b=1"
*)

(* A test for stack backtraces *)

external get_backtrace : unit -> Printexc.backtrace_slot array option
  = "caml_get_exception_backtrace"

exception Error of string

let rec f msg n =
  if n = 0 then raise(Error msg) else 1 + f msg (n-1)

let g msg =
  try
    f msg 5
  with Error "a" -> print_string "a"; print_newline(); 0
     | Error "b" as exn -> print_string "b"; print_newline(); raise exn
     | Error "c" -> raise (Error "c")

let run args =
  try
    ignore (g args.(0)); print_string "No exception\n"
  with exn ->
    Printf.printf "Uncaught exception %s\n" (Printexc.to_string exn);
    get_backtrace () |> function
    | None -> ()
    | Some trace ->
      Array.iteri
        (fun i slot -> match Printexc.Slot.format i slot with
          | None -> ()
          | Some line -> print_endline line)
        trace

let _ =
  run [| "a" |];
  run [| "b" |];
  run [| "c" |];
  run [| "d" |];
  run [| |]
