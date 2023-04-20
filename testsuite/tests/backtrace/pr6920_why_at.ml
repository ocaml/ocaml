(* TEST_BELOW
(* Blank lines added here to preserve locations. *)



*)

let why : unit -> unit = fun () -> raise Exit [@@inline never]
let f () =
  why @@ ();
  ignore (3 + 2);
  () [@@inline never]

let () =
  f ()

(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
 ocamlopt_flags = "-inline 0";
 exit_status = "2";
*)
