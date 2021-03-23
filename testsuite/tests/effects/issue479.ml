(* TEST
   * toplevel
*)

(* https://github.com/ocaml-multicore/ocaml-multicore/issues/479 *)

[@@@warning "-5-26"];;

type ('a, 'container) iterator = ('a -> unit) -> 'container -> unit;;
type 'a generator = unit -> 'a option;;

type ('a,'container) iter2gen =
  ('a, 'container) iterator     (* List.iter *)
  -> 'container
  -> 'a generator;;

effect Hold: unit;;
let iter2gen : _ iter2gen = fun iter c ->
  let r = ref None in
  let suspending_f x =
    r:=Some x;
    (perform Hold : unit)
  in
  let next =
    match iter suspending_f c with
    | () -> fun ()->None
    | effect Hold k ->
       fun()->
         let x = !r in
         Printf.printf "Hold %s\n%!" (match x with None->"?" | Some x->string_of_int x);
         continue k ();
         x

   in
   fun()-> next();;


let f () =
  let gen = iter2gen List.iter in
  let gen = gen [1;2;3] in
  let gen() = match gen() with None->"?" | Some x-> string_of_int x in
  Printf.printf "%s\n%!" (gen());
  Printf.printf "%s\n%!" (gen());;

f ();;
