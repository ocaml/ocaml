(* TEST
   include unix
   modules = "nested_fiber_.c"
   * libunix
   ** bytecode
   ** native
*)

external caml_to_c : (unit -> 'a) -> 'a = "caml_to_c"

effect E : unit

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
  (match g () with
  | effect E k -> assert false
  | () ->
     Printf.printf "g() returned: %d\n%!" !z);
  Printf.printf "f() check: %d\n%!" (check x)

let () =
  let x = make 3 in
  let z = ref 2 in
  (match f () with
  | effect E k -> assert false
  | () ->
     Printf.printf "f() returned: %d\n%!" !z);
  Printf.printf "() check: %d\n%!" (check x)
