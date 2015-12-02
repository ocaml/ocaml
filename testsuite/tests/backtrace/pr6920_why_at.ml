let why : unit -> unit = fun () -> raise Exit
let f () =
  why @@ ();
  ignore (3 + 2);
  ()

let () =
  Printexc.record_backtrace true;
  f ()
