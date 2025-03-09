(* TEST *)

(* testing with pairs (integer priority, string) *)
module E = struct
  type t = int * string
  let compare (p1,_) (p2,_) = Int.compare p1 p2
end
module Q = Pqueue.MakeMin(E)

let does_raise f q =
  try
    ignore (f q);
    false
  with Invalid_argument _ ->
    true

let check_is_empty q =
  assert (Q.length q = 0); assert (Q.is_empty q);
  assert (does_raise Q.get_min_elt q); assert (Q.min_elt q = None);
  assert (Q.pop_min q = None); Q.remove_min q; assert (Q.length q = 0)

let () =
  let q = Q.create () in
  check_is_empty q;
  Q.add q (1, "a");
  assert (Q.length q = 1); assert (not (Q.is_empty q));
  assert (Q.get_min_elt q = (1, "a")); assert (Q.min_elt q = Some (1, "a"));
  assert (Q.length q = 1);
  assert (Q.pop_min q = Some (1, "a")); check_is_empty q;
  Q.add q (2, "b");
  Q.add q (1, "a");
  assert (Q.get_min_elt q = (1, "a")); assert (Q.min_elt q = Some (1, "a"));
  assert (Q.pop_min q = Some (1, "a")); assert (Q.length q = 1);
  assert (Q.get_min_elt q = (2, "b")); assert (Q.min_elt q = Some (2, "b"));
  Q.remove_min q; check_is_empty q;
  Q.add q (2, "b");
  Q.add q (1, "a");
  Q.clear q; check_is_empty q;
  ()

let () =
  let q = Q.create () in
  Q.add q (1, "a"); Q.add q (2, "b");
  let q' = Q.copy q in
  Q.clear q; check_is_empty q;
  assert (Q.length q' = 2)

let () =
  let q = Q.create () in
  for n = 0 to 10 do
    for x = n-1 downto 0 do Q.add q (x, "") done;
    for x = 0 to n-1 do assert (Q.pop_min q = Some (x, "")) done;
    check_is_empty q
  done

let () =
  let q = Q.create () in
  for n = 0 to 10 do
    for x = n-1 downto 0 do Q.add q (x, "") done;
    for x = 0 to n-1 do assert (Q.pop_min q = Some (x, "")) done;
    check_is_empty q
  done

(* check iter_unordered and fold_unordered *)
let () =
  let q = Q.create () in
  for n = 0 to 10 do Q.add q (n, "") done;
  let r = ref 0 in Q.iter_unordered (fun (x,_) -> r := !r + x) q;
  assert (!r = 55);
  assert (Q.fold_unordered (fun acc (x,_) -> acc+x) 0 q = 55)

let () =
  for n = 0 to 10 do
    let a = Array.init n (fun i -> (i/3, string_of_int i)) in
    let q = Q.of_array a in
    assert (Q.length q = n);
    for i = 0 to n - 1 do match Q.pop_min q with
                          | None -> assert false
                          | Some (x, _) -> assert (x = fst a.(i))
    done;
    check_is_empty q
  done

let () =
  let q = Q.create () in
  let l = [2, "b"; 3, "c"; 1, "a"; 4, "d"; 0, ""] in
  Q.add_iter q List.iter l;
  assert (Q.min_elt q = Some (0, ""));
  assert (Q.fold_unordered (fun acc (x, _) -> acc+x) 0 q = 10)

(* check that min_elt and pop_min are consistent when several elements
   have the same priority *)
let () =
  let q = Q.create () in
  Q.add q (1, "b"); Q.add q (1, "a"); Q.add q (1, "d"); Q.add q (1, "c");
  for _ = 1 to 4 do
    let x = Q.min_elt q in assert (x = Q.pop_min q)
  done;
  ()

(* check that Max is indeed a max-pqueue *)
let () =
  let open Pqueue.MakeMax(E) in
  let q = create () in
  add q (2, "b"); add q (1, "a"); add q (4, "d"); add q (3, "c");
  for i = 4 downto 1 do match pop_max q with
                        | None -> assert false
                        | Some (x, _) -> assert (x = i)
  done

(* testing with string elements *)
let () =
  let open Pqueue.MakeMin(String) in
  let a = [| "b"; "bar"; "a"; "aa"; "foo"; "ocaml" |] in
  let n = Array.length a in
  let q = create () in
  for i = 0 to n - 1 do add q a.(i) done;
  assert (length q = n);
  Array.sort String.compare a;
  for i = 0 to n - 1 do match pop_min q with
                          | None -> assert false
                          | Some x -> assert (x = a.(i))
  done;
  assert (is_empty q)

(* check the usage scenario from the .mli *)
module Prio : Pqueue.OrderedType = Int

module PrioQueue = Pqueue.MakeMinPoly(struct
  type 'a t = Prio.t * 'a
  let compare (p1, _) (p2, _) = Prio.compare p1 p2
end)


let () = print_endline "OK"
