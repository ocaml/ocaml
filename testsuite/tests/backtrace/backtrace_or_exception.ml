(* TEST
   flags = "-g"
   ocamlrunparam += ",b=1"
   compare_programs = "false"
*)

exception Exn

let return_exn ?(raise_it_instead=false) () =
  if raise_it_instead then
    raise Exn
  else
    Exn
[@@inline never]

let without_reraise () =
  match return_exn () with
  | Exn as exn
  | exception (Exn as exn) ->
    raise exn
  | _ -> assert false

let with_reraise () =
  match return_exn ~raise_it_instead:true () with
  | Exn as exn
  | exception (Exn as exn) ->
    raise exn
  | _ -> assert false

let trickier () =
  try raise Not_found
  with e ->
    match return_exn () with
    | Exn as exn
    | exception (Exn as exn) ->
      raise exn
    | _ -> assert false

let run f =
  try f ()
  with exn ->
    Printf.printf "exception %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stdout;
    Printf.printf "---------------------------\n%!"

let _ =
  Printexc.record_backtrace true;
  run without_reraise;
  run with_reraise;
  run trickier
