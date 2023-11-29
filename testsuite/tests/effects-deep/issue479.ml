(* TEST
 toplevel;
*)

(* https://github.com/ocaml-multicore/ocaml-multicore/issues/479 *)

open Effect

[@@@warning "-5-26"];;

Printexc.record_backtrace false;;

type ('a, 'container) iterator = ('a -> unit) -> 'container -> unit;;
type 'a generator = unit -> 'a option;;

type ('a,'container) iter2gen =
  ('a, 'container) iterator     (* List.iter *)
  -> 'container
  -> 'a generator;;

type hold = effect Hold: unit

let hold = Effect.create ()

let iter2gen : _ iter2gen = fun iter c ->
  let r = ref None in
  let suspending_f x =
    r:=Some x;
    Effect.perform hold Hold
  in
  let next =
    Effect.run_with hold (iter suspending_f) c
    { result = (fun _ -> fun () -> None);
      exn = (fun e -> raise e);
      operation =
        (fun (type a) (Hold : (a, _) operation) (k : (a, _) continuation) ->
          (fun () ->
            let x = !r in
            Printf.printf "Hold %s\n%!"
              (match x with
               | None -> "?"
               | Some x -> string_of_int x);
            Effect.continue k ();
            x)) }
   in
   fun () -> next ();;

let f () =
  let gen = iter2gen List.iter in
  let gen = gen [1;2;3] in
  let gen() = match gen() with None->"?" | Some x-> string_of_int x in
  Printf.printf "%s\n%!" (gen());
  Printf.printf "%s\n%!" (gen());;

f ();;
