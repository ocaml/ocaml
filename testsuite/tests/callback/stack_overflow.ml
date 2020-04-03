(* TEST
   include unix
   modules = "stack_overflow_.c"
   * libunix
   ** bytecode
   ** native
*)

external caml_to_c : (unit -> 'a) -> 'a = "caml_to_c"

let rec deep = function
  | 0 ->
     ref 42
  | n ->
     caml_to_c (fun () -> deep (n-1))

effect E : unit

let () =
  Printf.printf "%d\n%d\n%!"
    (!(deep 1000))
    (match deep 1000 with x -> !x | effect E k -> assert false)
