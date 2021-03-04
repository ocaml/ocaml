(* TEST
   flags = "-g"
   ocamlrunparam += ",b=1"
   ocamlopt_flags = "-inline 0"
   exit_status = "2"
*)

let why : unit -> unit = fun () -> raise Exit [@@inline never]
let f () =
  for i = 1 to 10 do
    why @@ ();
  done;
  ignore (3 + 2);
  () [@@inline never]

let () =
  f ()
