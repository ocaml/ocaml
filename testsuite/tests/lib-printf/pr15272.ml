(* TEST
 arch64;
 not windows;
 bytecode;
 native;
*)

(* Restricted to 64-bit: the failure needs a precision beyond INT_MAX, and
   on a 32-bit target max_int is small enough that vsnprintf succeeds.

   Skipped on Windows: there caml_alloc_sprintf_l goes through
   _vscprintf_l, whose negative return is still unchecked. *)

let show f =
  match f () with
  | s -> Printf.printf "ok, length %d\n" (String.length s)
  | exception e -> print_endline (Printexc.to_string e)

let () =
  show (fun () -> Printf.sprintf "%.*f" max_int 1.0);
  show (fun () -> Printf.sprintf "%20.*f" max_int 1.0)
