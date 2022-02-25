(* TEST
   include unix
   modules = "nested_fiber_.c"
   * libunix
   ** bytecode
   ** native
*)

external caml_to_c : (unit -> 'a) -> 'a = "caml_to_c"

open Effect
open Effect.Deep

type _ t += E : unit t

type 'a tree = Empty | Node of 'a tree * 'a tree

let rec make d =
  match d with
  | 0 -> Node(Empty, Empty)
  | _ -> let d = d - 1 in Node(make d, make d)

let rec check = function Empty -> 0 | Node(l, r) -> 1 + check l + check r

let g () =
  caml_to_c (fun () ->
      Gc.full_major ();
      let x = make 10 in
      Printf.printf "g() check %d\n%!" (check x))

let f () =
  let x = make 3 in
  let z = ref 1 in
  match_with g ()
  { retc = (fun () -> Printf.printf "g() returned: %d\n%!" !z);
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a t) ->
          match e with
          | E -> Some (fun (k : (a, _) continuation) -> assert false)
          | _ -> None };
  Printf.printf "f() check: %d\n%!" (check x)

let () =
  let x = make 3 in
  let z = ref 2 in
  match_with f ()
  { retc = (fun () -> Printf.printf "f() returned: %d\n%!" !z);
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a t) ->
      match e with
      | E -> Some (fun k -> assert false)
      | _ -> None };
  Printf.printf "() check: %d\n%!" (check x)
