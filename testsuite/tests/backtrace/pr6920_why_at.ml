(* TEST
   flags = "-g"
   ocamlrunparam += ",b=1"
   ocamlopt_flags = "-inline 0"
   exit_status = "2"
   compare_programs = "false"
*)

let why : unit -> unit = fun () -> raise Exit [@@inline never]
let f () =
  why @@ ();
  ignore (3 + 2);
  () [@@inline never]

let () =
  Printexc.record_backtrace true;
  f ()
