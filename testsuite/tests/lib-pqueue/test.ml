(* TEST *)

module Q = Pqueue.Make(Int)

let does_raise f q =
  try
    ignore (f q);
    false
  with Q.Empty ->
    true

let check_is_empty q =
  assert (Q.length q = 0); assert (Q.is_empty q);
  assert (does_raise Q.min_elt q); assert (Q.min_elt_opt q = None);
  assert (does_raise Q.pop_min q); assert (Q.pop_min_opt q = None);
  assert (does_raise Q.remove_min q)

let () =
  let q = Q.create () in
  check_is_empty q;
  Q.add q 1;
  assert (Q.length q = 1); assert (not (Q.is_empty q));
  assert (Q.min_elt q = 1); assert (Q.min_elt_opt q = Some 1);
  assert (Q.length q = 1);
  assert (Q.pop_min q = 1); check_is_empty q;
  Q.add q 2;
  Q.add q 1;
  assert (Q.min_elt q = 1); assert (Q.min_elt_opt q = Some 1);
  assert (Q.pop_min_opt q = Some 1); assert (Q.length q = 1);
  assert (Q.min_elt q = 2); assert (Q.min_elt_opt q = Some 2);
  Q.remove_min q; check_is_empty q;
  Q.add q 2;
  Q.add q 1;
  Q.clear q; check_is_empty q;
  ()

let () =
  let q = Q.create () in
  Q.add q 1; Q.add q 2;
  let q' = Q.copy q in
  Q.clear q; check_is_empty q;
  assert (Q.length q' = 2)

let () =
  let q = Q.create () in
  for n = 0 to 10 do
    for x = n-1 downto 0 do Q.add q x done;
    for x = 0 to n-1 do assert (Q.pop_min q = x) done;
    check_is_empty q
  done

let () =
  let q = Q.create () in
  for n = 0 to 10 do
    for x = n-1 downto 0 do Q.add q x done;
    for x = 0 to n-1 do assert (Q.pop_min q = x) done;
    check_is_empty q
  done

let () =
  for n = 0 to 10 do
    let a = Array.init n (fun i -> i/3) in
    let q = Q.of_array a in
    assert (Q.length q = n);
    for i = 0 to n - 1 do assert (Q.pop_min_opt q = Some a.(i)) done;
    check_is_empty q
  done

let () =
  let q = Q.create () in
  let s = List.to_seq [2;3;1;4;0] in
  Q.add_seq q s;
  assert (Q.min_elt q = 0);
  assert (Q.fold (+) 0 q = 10)

(* check that min_elt and pop_min are consistent
   (we cannot do that with integers) *)
module Qs = Pqueue.Make(struct
  type t = int * string
  let compare x y = Int.compare (fst x) (fst y)
end)

let () =
  let q = Qs.create () in
  Qs.add q (1, "b"); Qs.add q (1, "a"); Qs.add q (1, "d"); Qs.add q (1, "c");
  for _ = 1 to 4 do
    let x = Qs.min_elt q in assert (x = Qs.pop_min q)
  done;
  ()

let () = print_endline "OK"
