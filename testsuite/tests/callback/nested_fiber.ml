(* TEST
   include unix
   modules = "nested_fiber_.c"
   * libunix
   ** bytecode
   ** native
*)

external caml_to_c : (unit -> 'a) -> 'a = "caml_to_c"

effect E : unit

let g () =
  caml_to_c (fun () ->
      Gc.full_major ();
      Printf.printf "%d\n%!" 1)

let f () =
  let x = ref 2 in
  match g () with
  | effect E k -> assert false
  | () ->
     Printf.printf "%d\n" !x

let () =
  match f () with
  | effect E k -> assert false
  | () -> ()
