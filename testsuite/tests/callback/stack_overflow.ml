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

open Effect
open Effect.Deep

type _ t += E : unit t

let () =
  Printf.printf "%d\n%d\n%!"
    (!(deep 1000))
    (match_with deep 1000
     { retc = (fun x -> !x);
       exnc = (fun e -> raise e);
       effc = fun (type a) (e : a t) ->
         match e with
         | E -> Some (fun k -> assert false)
         | _ -> None })
