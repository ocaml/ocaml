(* TEST *)

(* Tests for the Hash_set module *)

(* Helpers for building sets from lists and checking their contents *)

let of_list l =
  let s = Hash_set.create (List.length l) in
  List.iter (Hash_set.add s) l;
  s

let to_sorted_list s =
  Hash_set.fold (fun x acc -> x :: acc) s []
  |> List.sort compare

(* ===== Generic polymorphic interface ===== *)

(* --- create / length / add / mem --- *)

let () =
  let s = Hash_set.create 16 in
  assert (Hash_set.length s = 0);
  Hash_set.add s 1;
  assert (Hash_set.length s = 1);
  assert (Hash_set.mem s 1);
  assert (not (Hash_set.mem s 2));
  (* Adding a duplicate element leaves the set unchanged *)
  Hash_set.add s 1;
  assert (Hash_set.length s = 1);
  Hash_set.add s 2;
  assert (Hash_set.length s = 2);
  assert (Hash_set.mem s 2);
  (* It's a set, values are only added once; *)
  Hash_set.add s 2;
  assert (Hash_set.length s = 2)

(* --- singleton --- *)

let () =
  let s = Hash_set.singleton 42 in
  assert (Hash_set.length s = 1);
  assert (Hash_set.mem s 42);
  assert (not (Hash_set.mem s 0));
  (* singleton with explicit ~random:false *)
  let s2 = Hash_set.singleton ~random:false "hello" in
  assert (Hash_set.length s2 = 1);
  assert (Hash_set.mem s2 "hello")

(* --- remove --- *)

let () =
  let s = of_list [10; 20; 30] in
  (* Remove existing element *)
  Hash_set.remove s 20;
  assert (to_sorted_list s = [10; 30]);
  (* Removing absent element is a no-op *)
  Hash_set.remove s 99;
  assert (to_sorted_list s = [10; 30])

(* --- clear --- *)

let () =
  let s = of_list (List.init 20 (fun i -> i + 1)) in
  assert (Hash_set.length s = 20);
  Hash_set.clear s;
  assert (to_sorted_list s = [])

(* --- reset --- *)

let () =
  let s = of_list (List.init 20 (fun i -> i + 1)) in
  Hash_set.reset s;
  assert (to_sorted_list s = [])

(* --- copy --- *)

let () =
  let s = of_list [1; 2; 3] in
  let s2 = Hash_set.copy s in
  assert (to_sorted_list s2 = [1; 2; 3]);
  (* Mutations to the copy do not affect the original *)
  Hash_set.add s2 4;
  assert (to_sorted_list s = [1; 2; 3]);
  (* Mutations to the original do not affect the copy *)
  Hash_set.remove s 1;
  assert (to_sorted_list s2 = [1; 2; 3; 4])

(* --- iter --- *)

let () =
  let s = of_list [1; 2; 3; 4; 5] in
  let seen = ref [] in
  Hash_set.iter (fun x -> seen := x :: !seen) s;
  assert (List.sort compare !seen = [1; 2; 3; 4; 5])

(* --- fold --- *)

let () =
  let s = of_list [1; 2; 3; 4; 5] in
  assert (Hash_set.fold (fun x acc -> x + acc) s 0 = 15);
  (* fold over empty set returns the initial accumulator *)
  let empty = Hash_set.create 4 in
  assert (Hash_set.fold (fun _ _ -> assert false) empty 99 = 99)

(* --- filter_inplace --- *)

let () =
  let s = of_list [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  Hash_set.filter_inplace (fun x -> x mod 2 = 0) s;
  assert (to_sorted_list s = [2; 4; 6; 8; 10]);
  (* Keeping all elements *)
  let s2 = of_list [10; 20; 30] in
  Hash_set.filter_inplace (fun _ -> true) s2;
  assert (to_sorted_list s2 = [10; 20; 30]);
  (* Removing all elements *)
  Hash_set.filter_inplace (fun _ -> false) s2;
  assert (to_sorted_list s2 = [])

(* --- subseteq --- *)

let () =
  let s1 = of_list [1; 2; 3] in
  let s2 = of_list [1; 2; 3; 4; 5] in
  assert (Hash_set.subseteq s1 s2);
  assert (not (Hash_set.subseteq s2 s1));
  (* Empty set is a subset of everything *)
  let empty = Hash_set.create 4 in
  assert (Hash_set.subseteq empty s1);
  assert (Hash_set.subseteq empty empty);
  (* A set is a subset of itself *)
  assert (Hash_set.subseteq s1 s1);
  (* Disjoint sets: neither is a subset of the other *)
  let s3 = of_list [10; 11; 12] in
  assert (not (Hash_set.subseteq s1 s3));
  assert (not (Hash_set.subseteq s3 s1))

(* --- equal --- *)

let () =
  let s1 = of_list [1; 2; 3] in
  let s2 = of_list [1; 2; 3] in
  assert (Hash_set.equal s1 s2);
  Hash_set.add s2 4;
  assert (not (Hash_set.equal s1 s2));
  (* Empty sets are equal *)
  assert (Hash_set.equal (Hash_set.create 1) (Hash_set.create 1));
  (* A set is equal to itself *)
  assert (Hash_set.equal s1 s1);
  (* Different elements of the same cardinality *)
  assert (not (Hash_set.equal s1 (of_list [1; 2; 99])));
  (* A copy of a hashset is equal to it *)
  assert (Hash_set.equal s1 (Hash_set.copy s1))

(* --- to_seq / add_seq / of_seq --- *)

let () =
  let s = Hash_set.of_seq (List.to_seq [1; 2; 3; 4; 5]) in
  assert (to_sorted_list s = [1; 2; 3; 4; 5]);
  (* to_seq round-trip *)
  assert (Hash_set.equal s (Hash_set.of_seq (Hash_set.to_seq s)));
  (* add_seq *)
  let s2 = Hash_set.create 4 in
  Hash_set.add_seq s2 (List.to_seq [10; 20; 30]);
  assert (to_sorted_list s2 = [10; 20; 30]);
  (* of_seq with duplicates collapses to unique elements *)
  assert (Hash_set.length (Hash_set.of_seq (List.to_seq [1; 1; 2; 2; 3])) = 3)

(* --- rebuild --- *)

let () =
  let s = of_list [1; 2; 3; 4; 5] in
  assert (Hash_set.equal s (Hash_set.rebuild ~random:false s))

(* --- stats --- *)

let () =
  let s = of_list (List.init 10 (fun i -> i + 1)) in
  let st = Hash_set.stats s in
  assert (st.Hashtbl.num_bindings = 10);
  assert (st.Hashtbl.num_buckets > 0)

(* ===== Functorial interface: Hash_set.Make ===== *)

module IntHash = struct
  type t = int
  let equal (i : int) j = i = j
  let hash i = i land max_int
end

module IntSet = Hash_set.Make (IntHash)

let int_set_of_list l =
  let s = IntSet.create (List.length l) in
  List.iter (IntSet.add s) l;
  s

let int_set_to_sorted_list s =
  IntSet.fold (fun x acc -> x :: acc) s []
  |> List.sort compare

let () =
  (* create / add / mem / length *)
  let s = int_set_of_list [1; 2; 3] in
  assert (int_set_to_sorted_list s = [1; 2; 3]);
  assert (not (IntSet.mem s 99));
  (* Duplicate add is idempotent *)
  IntSet.add s 2;
  assert (int_set_to_sorted_list s = [1; 2; 3]);
  (* remove *)
  IntSet.remove s 2;
  assert (int_set_to_sorted_list s = [1; 3]);
  (* singleton *)
  assert (int_set_to_sorted_list (IntSet.singleton 42) = [42]);
  (* copy: independent from original *)
  let s2 = IntSet.copy s in
  IntSet.add s2 99;
  assert (not (IntSet.mem s 99));
  (* fold / iter agree on contents *)
  assert (IntSet.fold (fun x acc -> x + acc) s 0 = 4); (* 1 + 3 *)
  let seen = ref [] in
  IntSet.iter (fun x -> seen := x :: !seen) s;
  assert (List.sort compare !seen = [1; 3]);
  (* filter_inplace *)
  let s3 = int_set_of_list [1; 2; 3; 4; 5; 6] in
  IntSet.filter_inplace (fun x -> x > 3) s3;
  assert (int_set_to_sorted_list s3 = [4; 5; 6]);
  (* subseteq / equal *)
  let sa = int_set_of_list [1; 2] in
  let sb = int_set_of_list [1; 2; 3] in
  assert (IntSet.subseteq sa sb);
  assert (not (IntSet.subseteq sb sa));
  assert (not (IntSet.equal sa sb));
  assert (IntSet.equal sa (int_set_of_list [1; 2]));
  (* to_seq / of_seq round-trip *)
  let s4 = int_set_of_list [10; 20; 30] in
  assert (IntSet.equal s4 (IntSet.of_seq (IntSet.to_seq s4)));
  (* clear / reset *)
  IntSet.clear s4;
  assert (int_set_to_sorted_list s4 = []);
  let s5 = int_set_of_list [1; 2; 3] in
  IntSet.reset s5;
  assert (int_set_to_sorted_list s5 = [])

(* ===== Functorial interface: Hash_set.MakeSeeded ===== *)

module StringSeededHash = struct
  type t = string
  let equal (a : string) b = String.equal a b
  let seeded_hash seed s = Hashtbl.seeded_hash seed s
end

module StringSet = Hash_set.MakeSeeded (StringSeededHash)

let str_set_of_list l =
  let s = StringSet.create ~random:false (List.length l) in
  List.iter (StringSet.add s) l;
  s

let str_set_to_sorted_list s =
  StringSet.fold (fun x acc -> x :: acc) s []
  |> List.sort compare

let () =
  (* create / add / mem / length *)
  let s = str_set_of_list ["foo"; "bar"; "baz"] in
  assert (str_set_to_sorted_list s = ["bar"; "baz"; "foo"]);
  assert (not (StringSet.mem s "qux"));
  (* Duplicate add *)
  StringSet.add s "foo";
  assert (StringSet.length s = 3);
  (* remove *)
  StringSet.remove s "bar";
  assert (str_set_to_sorted_list s = ["baz"; "foo"]);
  (* singleton with ~random:false *)
  let s2 = StringSet.singleton ~random:false "hello" in
  assert (str_set_to_sorted_list s2 = ["hello"]);
  (* equal *)
  assert (StringSet.equal s (str_set_of_list ["foo"; "baz"]));
  (* subseteq *)
  let s3 = str_set_of_list ["foo"] in
  assert (StringSet.subseteq s3 s);
  assert (not (StringSet.subseteq s s3));
  (* iter counts elements *)
  let count = ref 0 in
  StringSet.iter (fun _ -> incr count) s;
  assert (!count = 2);
  (* filter_inplace *)
  let s4 = str_set_of_list ["foo"; "bar"; "baz"; "qux"] in
  StringSet.filter_inplace (fun x -> x.[0] = 'b') s4;
  assert (str_set_to_sorted_list s4 = ["bar"; "baz"]);
  (* add_seq *)
  let s5 = StringSet.create 4 in
  StringSet.add_seq s5 (List.to_seq ["x"; "y"; "z"]);
  assert (str_set_to_sorted_list s5 = ["x"; "y"; "z"]);
  (* stats *)
  assert ((StringSet.stats s).Hashtbl.num_bindings = 2)

(* ===== Stress test ===== *)

let () =
  let n = 10_000 in
  let s = of_list (List.init n (fun i -> i)) in
  assert (Hash_set.length s = n);
  (* Remove even elements *)
  for i = 0 to n - 1 do
    if i mod 2 = 0 then Hash_set.remove s i
  done;
  assert (to_sorted_list s = List.init (n / 2) (fun i -> 2 * i + 1));
  (* filter_inplace: remove elements divisible by 3 *)
  Hash_set.filter_inplace (fun x -> x mod 3 <> 0) s;
  Hash_set.iter (fun x -> assert (x mod 3 <> 0)) s;
  (* of_seq / to_seq round-trip *)
  assert (Hash_set.equal s (Hash_set.of_seq (Hash_set.to_seq s)))

let () = print_endline "OK"
