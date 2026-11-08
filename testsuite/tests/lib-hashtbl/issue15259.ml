(* TEST
 expect;
*)

(* https://github.com/ocaml/ocaml/issues/15259

   If [f] raises, [Hashtbl.filter_map_inplace] must leave the table in a
   consistent state: bindings for which [f] returned [None] are removed,
   bindings for which it returned [Some v] are updated, the others are left
   untouched, and [Hashtbl.length] is the number of bindings that [iter]
   can reach. *)

(* Keys [k] and [k'] share a bucket iff [k / 100 = k' / 100], so each test
   controls the exact bucket layout (the tables below stay small enough
   not to be resized). The functor is applied locally, to keep its
   signature out of the expected output. *)
module Buckets = struct
  type t = int
  let equal = Int.equal
  let hash k = k / 100
end

exception Boom

(* [f ~drop ~fail] drops the keys in [drop], raises [Boom] on the keys in
   [fail], and multiplies the other values by 10. *)
let f ~drop ~fail k v =
  if List.mem k drop then None
  else if List.mem k fail then raise Boom
  else Some (v * 10)

(* [run ?rounds ~visit f] builds a table with bindings [k -> k] that
   [filter_map_inplace] visits in the order [visit] (which must list buckets
   in increasing order), calls [filter_map_inplace f] [rounds] times
   catching [Boom], and returns [(length, sorted reachable bindings)]. *)
let run ?(rounds = 1) ~visit f =
  let module H = Hashtbl.Make (Buckets) in
  let h = H.create 16 in
  List.iter (fun k -> H.add h k k) (List.rev visit);
  for _ = 1 to rounds do
    try H.filter_map_inplace f h with Boom -> ()
  done;
  H.length h, List.sort compare (H.fold (fun k v acc -> (k, v) :: acc) h [])
[%%expect{|
module Buckets :
  sig type t = int val equal : int -> int -> bool val hash : int -> int end
exception Boom
val f : drop:'a list -> fail:'a list -> 'a -> int -> int option = <fun>
val run :
  ?rounds:int ->
  visit:int list ->
  (Buckets.t -> int -> int option) -> int * (Buckets.t * int) list = <fun>
|}]

(* The reproduction from the issue: drop 2, then raise on 1; repeated four
   times, then [clear]. *)
let _ =
  let module H = Hashtbl.Make (Buckets) in
  let h = H.create 16 in
  H.add h 0 0; H.add h 1 1; H.add h 2 2;
  for _ = 1 to 4 do
    (try H.filter_map_inplace
           (fun k _ -> if k = 2 then None else raise Boom) h
     with Boom -> ())
  done;
  let reachable () = H.fold (fun _ _ n -> n + 1) h 0 in
  let before_clear = (H.length h, reachable (), H.mem h 2) in
  H.clear h;
  before_clear, (H.length h, reachable ())
[%%expect{|
- : (int * int * bool) * (int * int) = ((2, 2, false), (0, 0))
|}]

(* No exception: sanity checks. *)

let _ = run ~visit:[] (f ~drop:[] ~fail:[])
[%%expect{|
- : int * (Buckets.t * int) list = (0, [])
|}]

let _ = run ~visit:[1; 2; 3] (f ~drop:[] ~fail:[])
[%%expect{|
- : int * (Buckets.t * int) list = (3, [(1, 10); (2, 20); (3, 30)])
|}]

let _ = run ~visit:[1; 2; 3] (f ~drop:[1; 2; 3] ~fail:[])
[%%expect{|
- : int * (Buckets.t * int) list = (0, [])
|}]

let _ = run ~visit:[1; 2; 3] (f ~drop:[1] ~fail:[])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(2, 20); (3, 30)])
|}]

let _ = run ~visit:[1; 2; 3] (f ~drop:[2] ~fail:[])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(1, 10); (3, 30)])
|}]

let _ = run ~visit:[1; 2; 3] (f ~drop:[3] ~fail:[])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(1, 10); (2, 20)])
|}]

let _ = run ~visit:[1; 2; 3; 4] (f ~drop:[1; 3] ~fail:[])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(2, 20); (4, 40)])
|}]

let _ = run ~visit:[1; 2; 3; 4] (f ~drop:[2; 4] ~fail:[])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(1, 10); (3, 30)])
|}]

let _ =
  run ~visit:[1; 2; 201; 202; 203; 301] (f ~drop:[1; 202; 203; 301] ~fail:[])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(2, 20); (201, 2010)])
|}]

(* [f] raises. Below, the visit order is written as e.g. "D K R":
   drop, keep, raise. *)

(* R: nothing processed, the table is unchanged. *)
let _ = run ~visit:[1; 2; 3] (f ~drop:[2] ~fail:[1])
[%%expect{|
- : int * (Buckets.t * int) list = (3, [(1, 1); (2, 2); (3, 3)])
|}]

(* D R *)
let _ = run ~visit:[2; 1; 0] (f ~drop:[2] ~fail:[1])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(0, 0); (1, 1)])
|}]

(* D R K, four rounds: the dropped binding must not be visited again. *)
let _ = run ~rounds:4 ~visit:[1; 2; 3] (f ~drop:[1] ~fail:[2])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(2, 2); (3, 3)])
|}]

(* D K R: a kept binding between the dropped one and the raise. *)
let _ = run ~visit:[1; 2; 3] (f ~drop:[1] ~fail:[3])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(2, 20); (3, 3)])
|}]

(* D D R *)
let _ = run ~visit:[1; 2; 3] (f ~drop:[1; 2] ~fail:[3])
[%%expect{|
- : int * (Buckets.t * int) list = (1, [(3, 3)])
|}]

(* K D R *)
let _ = run ~visit:[1; 2; 3] (f ~drop:[2] ~fail:[3])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(1, 10); (3, 3)])
|}]

(* D K D R *)
let _ = run ~visit:[1; 2; 3; 4] (f ~drop:[1; 3] ~fail:[4])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(2, 20); (4, 4)])
|}]

(* K D K D R *)
let _ = run ~visit:[1; 2; 3; 4; 5] (f ~drop:[2; 4] ~fail:[5])
[%%expect{|
- : int * (Buckets.t * int) list = (3, [(1, 10); (3, 30); (5, 5)])
|}]

(* D K R D: the binding after the raise is not visited, hence kept. *)
let _ = run ~visit:[1; 2; 3; 4] (f ~drop:[1; 4] ~fail:[3])
[%%expect{|
- : int * (Buckets.t * int) list = (3, [(2, 20); (3, 3); (4, 4)])
|}]

(* D K R, three rounds *)
let _ = run ~rounds:3 ~visit:[1; 2; 3] (f ~drop:[1] ~fail:[3])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(2, 2000); (3, 3)])
|}]

(* D K D | R: drops in a fully processed bucket, raise in a later one. *)
let _ = run ~visit:[1; 2; 3; 101] (f ~drop:[1; 3] ~fail:[101])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(2, 20); (101, 101)])
|}]

(* K D | D R *)
let _ = run ~visit:[1; 2; 101; 102] (f ~drop:[2; 101] ~fail:[102])
[%%expect{|
- : int * (Buckets.t * int) list = (2, [(1, 10); (102, 102)])
|}]

(* D K | K D R | D *)
let _ =
  run ~visit:[1; 2; 101; 102; 103; 201]
    (f ~drop:[1; 102; 201] ~fail:[103])
[%%expect{|
- : int * (Buckets.t * int) list =
(4, [(2, 20); (101, 1010); (103, 103); (201, 201)])
|}]

(* Random bucket layouts and outcomes (fixed seed), each followed by one to
   three calls. The table must contain exactly the bindings that [f] did not
   drop, and [length] must agree. Returns the number of failing layouts. *)
let _ =
  let module H = Hashtbl.Make (Buckets) in
  Random.init 42;
  let failures = ref 0 in
  for _ = 1 to 1000 do
    let h = H.create 16 in
    let keys =
      List.concat
        (List.init (1 + Random.int 4) (fun b ->
             List.init (Random.int 7) (fun i -> b * 100 + i))) in
    List.iter (fun k -> H.add h k k) keys;
    let dropped = ref [] in
    for _ = 1 to 1 + Random.int 3 do
      try
        H.filter_map_inplace
          (fun k v ->
             match Random.int 10 with
             | 0 -> raise Boom
             | n when n < 5 -> dropped := k :: !dropped; None
             | _ -> Some v)
          h
      with Boom -> ()
    done;
    let expected = List.filter (fun k -> not (List.mem k !dropped)) keys in
    let actual = H.fold (fun k _ acc -> k :: acc) h [] in
    if List.sort compare expected <> List.sort compare actual
    || H.length h <> List.length actual
    then incr failures
  done;
  !failures
[%%expect{|
- : int = 0
|}]
