(* TEST
   * flambda
   * native
*)

let do_something () =
  Printf.printf "Hello world\n%!"; Ok ()
[@@inline never]

let f x =
  match do_something () with
  | Ok () -> x
  | Error r -> let _ = !r in x
[@@inline never]

let () = f ()
