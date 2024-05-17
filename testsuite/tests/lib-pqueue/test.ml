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
  Q.add q (1, "a");
  assert (Q.length q = 1); assert (not (Q.is_empty q));
  assert (Q.min_elt q = (1, "a")); assert (Q.min_elt_opt q = Some (1, "a"));
  assert (Q.length q = 1);
  assert (Q.pop_min q = (1, "a")); check_is_empty q;
  Q.add q (2, "b");
  Q.add q (1, "a");
  assert (Q.min_elt q = (1, "a")); assert (Q.min_elt_opt q = Some (1, "a"));
  assert (Q.pop_min_opt q = Some (1, "a")); assert (Q.length q = 1);
  assert (Q.min_elt q = (2, "b")); assert (Q.min_elt_opt q = Some (2, "b"));
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
    for x = 0 to n-1 do assert (Q.pop_min q = (x, "")) done;
    check_is_empty q
  done

let () =
  let q = Q.create () in
  for n = 0 to 10 do
    for x = n-1 downto 0 do Q.add q (x, "") done;
    for x = 0 to n-1 do assert (Q.pop_min q = (x, "")) done;
    check_is_empty q
  done

let () =
  for n = 0 to 10 do
    let a = Array.init n (fun i -> (i/3, string_of_int i)) in
    let q = Q.of_array a in
    assert (Q.length q = n);
    for i = 0 to n - 1 do match Q.pop_min_opt q with
                          | None -> assert false
                          | Some (x, _) -> assert (x = fst a.(i))
    done;
    check_is_empty q
  done

let () =
  let q = Q.create () in
  let s = List.to_seq [2, "b"; 3, "c"; 1, "a"; 4, "d"; 0, ""] in
  Q.add_seq q s;
  assert (Q.min_elt q = (0, ""));
  assert (Q.fold (fun acc (x, _) -> acc+x) 0 q = 10)

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
  for i = 4 downto 1 do let x = pop_max q in assert (fst x = i) done

(* testing with string elements *)
let () =
  let open Pqueue.MakeMin(String) in
  let a = [| "b"; "bar"; "a"; "aa"; "foo"; "ocaml" |] in
  let n = Array.length a in
  let q = create () in
  for i = 0 to n - 1 do add q a.(i) done;
  assert (length q = n);
  Array.sort String.compare a;
  for i = 0 to n - 1 do match pop_min_opt q with
                          | None -> assert false
                          | Some x -> assert (x = a.(i))
  done;
  assert (is_empty q)

let () = print_endline "OK"
