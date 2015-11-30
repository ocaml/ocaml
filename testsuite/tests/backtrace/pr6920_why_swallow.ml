let why : unit -> unit = fun () -> raise Exit
let f () =
  for i = 1 to 10 do
    why @@ ();
  done;
  ignore (3 + 2);
  ()

let () =
  Printexc.record_backtrace true;
  f ()
