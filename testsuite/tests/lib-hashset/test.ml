(* TEST *)

(* Tests for the Hash_set module *)

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
  assert (Hash_set.mem s 2)

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
  let s = Hash_set.create 8 in
  Hash_set.add s 10;
  Hash_set.add s 20;
  Hash_set.add s 30;
  (* Remove existing element *)
  Hash_set.remove s 20;
  assert (Hash_set.length s = 2);
  assert (not (Hash_set.mem s 20));
  assert (Hash_set.mem s 10);
  assert (Hash_set.mem s 30);
  (* Removing absent element is a no-op *)
  Hash_set.remove s 99;
  assert (Hash_set.length s = 2)

(* --- clear --- *)

let () =
  let s = Hash_set.create 8 in
  for i = 1 to 20 do Hash_set.add s i done;
  assert (Hash_set.length s = 20);
  Hash_set.clear s;
  assert (Hash_set.length s = 0);
  assert (not (Hash_set.mem s 1))

(* --- reset --- *)

let () =
  let s = Hash_set.create 8 in
  for i = 1 to 20 do Hash_set.add s i done;
  Hash_set.reset s;
  assert (Hash_set.length s = 0);
  assert (not (Hash_set.mem s 1))

(* --- copy --- *)

let () =
  let s = Hash_set.create 8 in
  Hash_set.add s "a";
  Hash_set.add s "b";
  let s2 = Hash_set.copy s in
  assert (Hash_set.length s2 = 2);
  assert (Hash_set.mem s2 "a");
  assert (Hash_set.mem s2 "b");
  (* Mutations to the copy do not affect the original *)
  Hash_set.add s2 "c";
  assert (Hash_set.length s = 2);
  assert (not (Hash_set.mem s "c"));
  (* Mutations to the original do not affect the copy *)
  Hash_set.remove s "a";
  assert (Hash_set.mem s2 "a")

(* --- iter --- *)

let () =
  let s = Hash_set.create 8 in
  List.iter (Hash_set.add s) [1; 2; 3; 4; 5];
  let seen = ref [] in
  Hash_set.iter (fun x -> seen := x :: !seen) s;
  let sorted = List.sort compare !seen in
  assert (sorted = [1; 2; 3; 4; 5])

(* --- fold --- *)

let () =
  let s = Hash_set.create 8 in
  List.iter (Hash_set.add s) [1; 2; 3; 4; 5];
  let sum = Hash_set.fold (fun x acc -> x + acc) s 0 in
  assert (sum = 15);
  (* fold over empty set returns the initial accumulator *)
  let empty = Hash_set.create 4 in
  assert (Hash_set.fold (fun _ _ -> assert false) empty 99 = 99)

(* --- filter_inplace --- *)

let () =
  let s = Hash_set.create 8 in
  List.iter (Hash_set.add s) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
  Hash_set.filter_inplace (fun x -> x mod 2 = 0) s;
  assert (Hash_set.length s = 5);
  for i = 1 to 10 do
    if i mod 2 = 0
    then assert (Hash_set.mem s i)
    else assert (not (Hash_set.mem s i))
  done;
  (* Keeping all elements *)
  let s2 = Hash_set.create 4 in
  List.iter (Hash_set.add s2) [10; 20; 30];
  Hash_set.filter_inplace (fun _ -> true) s2;
  assert (Hash_set.length s2 = 3);
  (* Removing all elements *)
  Hash_set.filter_inplace (fun _ -> false) s2;
  assert (Hash_set.length s2 = 0)

(* --- subseteq --- *)

let () =
  let s1 = Hash_set.create 8 in
  let s2 = Hash_set.create 8 in
  List.iter (Hash_set.add s1) [1; 2; 3];
  List.iter (Hash_set.add s2) [1; 2; 3; 4; 5];
  assert (Hash_set.subseteq s1 s2);
  assert (not (Hash_set.subseteq s2 s1));
  (* Empty set is a subset of everything *)
  let empty = Hash_set.create 4 in
  assert (Hash_set.subseteq empty s1);
  assert (Hash_set.subseteq empty empty);
  (* A set is a subset of itself *)
  assert (Hash_set.subseteq s1 s1);
  (* Disjoint sets: neither is a subset of the other *)
  let s3 = Hash_set.create 4 in
  List.iter (Hash_set.add s3) [10; 11; 12];
  assert (not (Hash_set.subseteq s1 s3));
  assert (not (Hash_set.subseteq s3 s1))

(* --- equal --- *)

let () =
  let s1 = Hash_set.create 8 in
  let s2 = Hash_set.create 8 in
  List.iter (Hash_set.add s1) [1; 2; 3];
  List.iter (Hash_set.add s2) [1; 2; 3];
  assert (Hash_set.equal s1 s2);
  Hash_set.add s2 4;
  assert (not (Hash_set.equal s1 s2));
  (* Empty sets are equal *)
  assert (Hash_set.equal (Hash_set.create 1) (Hash_set.create 1));
  (* A set is equal to itself *)
  assert (Hash_set.equal s1 s1);
  (* Different elements of the same cardinality *)
  let s3 = Hash_set.create 4 in
  List.iter (Hash_set.add s3) [1; 2; 99];
  assert (not (Hash_set.equal s1 s3));
  (* A copy of a hashet is equal to it. *)
  let s4 = Hash_set.copy s1 in
  assert (Hash_set.equal s1 s4)

(* --- to_seq / add_seq / of_seq --- *)

let () =
  let elems = [1; 2; 3; 4; 5] in
  let s = Hash_set.of_seq (List.to_seq elems) in
  assert (Hash_set.length s = 5);
  List.iter (fun x -> assert (Hash_set.mem s x)) elems;
  (* to_seq round-trip *)
  let s2 = Hash_set.of_seq (Hash_set.to_seq s) in
  assert (Hash_set.equal s s2);
  (* add_seq *)
  let s3 = Hash_set.create 4 in
  Hash_set.add_seq s3 (List.to_seq [10; 20; 30]);
  assert (Hash_set.length s3 = 3);
  assert (Hash_set.mem s3 10);
  assert (Hash_set.mem s3 20);
  assert (Hash_set.mem s3 30);
  (* of_seq with duplicates collapses to unique elements *)
  let s4 = Hash_set.of_seq (List.to_seq [1; 1; 2; 2; 3]) in
  assert (Hash_set.length s4 = 3)

(* --- rebuild --- *)

let () =
  let s = Hash_set.create ~random:false 8 in
  List.iter (Hash_set.add s) [1; 2; 3; 4; 5];
  let s2 = Hash_set.rebuild ~random:false s in
  assert (Hash_set.equal s s2)

(* --- stats --- *)

let () =
  let s = Hash_set.create 16 in
  for i = 1 to 10 do Hash_set.add s i done;
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

let () =
  let s = IntSet.create 16 in
  assert (IntSet.length s = 0);
  IntSet.add s 1;
  IntSet.add s 2;
  IntSet.add s 3;
  assert (IntSet.length s = 3);
  assert (IntSet.mem s 1);
  assert (IntSet.mem s 2);
  assert (IntSet.mem s 3);
  assert (not (IntSet.mem s 99));
  (* Duplicate add is idempotent *)
  IntSet.add s 2;
  assert (IntSet.length s = 3);
  IntSet.remove s 2;
  assert (IntSet.length s = 2);
  assert (not (IntSet.mem s 2));
  (* singleton *)
  let s2 = IntSet.singleton 42 in
  assert (IntSet.length s2 = 1);
  assert (IntSet.mem s2 42);
  (* copy *)
  let s3 = IntSet.copy s in
  assert (IntSet.equal s s3);
  IntSet.add s3 99;
  assert (not (IntSet.mem s 99));
  (* fold *)
  let sum = IntSet.fold (fun x acc -> x + acc) s 0 in
  assert (sum = 4); (* 1 + 3 *)
  (* iter *)
  let seen = ref [] in
  IntSet.iter (fun x -> seen := x :: !seen) s;
  assert (List.sort compare !seen = [1; 3]);
  (* filter_inplace *)
  let s4 = IntSet.of_seq (List.to_seq [1; 2; 3; 4; 5; 6]) in
  IntSet.filter_inplace (fun x -> x > 3) s4;
  assert (IntSet.length s4 = 3);
  assert (IntSet.mem s4 4);
  assert (IntSet.mem s4 5);
  assert (IntSet.mem s4 6);
  assert (not (IntSet.mem s4 1));
  (* subseteq / equal *)
  let sa = IntSet.of_seq (List.to_seq [1; 2]) in
  let sb = IntSet.of_seq (List.to_seq [1; 2; 3]) in
  assert (IntSet.subseteq sa sb);
  assert (not (IntSet.subseteq sb sa));
  assert (not (IntSet.equal sa sb));
  let sc = IntSet.of_seq (List.to_seq [1; 2]) in
  assert (IntSet.equal sa sc);
  (* to_seq / of_seq round-trip *)
  let s5 = IntSet.of_seq (List.to_seq [10; 20; 30]) in
  let s6 = IntSet.of_seq (IntSet.to_seq s5) in
  assert (IntSet.equal s5 s6);
  (* clear / reset *)
  IntSet.clear s5;
  assert (IntSet.length s5 = 0);
  let s7 = IntSet.of_seq (List.to_seq [1; 2; 3]) in
  IntSet.reset s7;
  assert (IntSet.length s7 = 0)

(* ===== Functorial interface: Hash_set.MakeSeeded ===== *)

module StringSeededHash = struct
  type t = string
  let equal (a : string) b = String.equal a b
  let seeded_hash seed s = Hashtbl.seeded_hash seed s
end

module StringSet = Hash_set.MakeSeeded (StringSeededHash)

let () =
  let s = StringSet.create ~random:false 8 in
  StringSet.add s "foo";
  StringSet.add s "bar";
  StringSet.add s "baz";
  assert (StringSet.length s = 3);
  assert (StringSet.mem s "foo");
  assert (StringSet.mem s "bar");
  assert (not (StringSet.mem s "qux"));
  (* Duplicate add *)
  StringSet.add s "foo";
  assert (StringSet.length s = 3);
  StringSet.remove s "bar";
  assert (StringSet.length s = 2);
  assert (not (StringSet.mem s "bar"));
  (* singleton with ~random:false *)
  let s2 = StringSet.singleton ~random:false "hello" in
  assert (StringSet.length s2 = 1);
  assert (StringSet.mem s2 "hello");
  (* equal *)
  let s3 = StringSet.of_seq (List.to_seq ["foo"; "baz"]) in
  assert (StringSet.equal s s3);
  (* subseteq *)
  let s4 = StringSet.of_seq (List.to_seq ["foo"]) in
  assert (StringSet.subseteq s4 s);
  assert (not (StringSet.subseteq s s4));
  (* fold *)
  let concat = StringSet.fold (fun x acc -> acc ^ x) s "" in
  (* Result depends on iteration order, just check it contains all elements *)
  assert (String.length concat = String.length "foobaz");
  (* iter *)
  let count = ref 0 in
  StringSet.iter (fun _ -> incr count) s;
  assert (!count = 2);
  (* filter_inplace *)
  let s5 = StringSet.of_seq (List.to_seq ["foo"; "bar"; "baz"; "qux"]) in
  StringSet.filter_inplace (fun x -> x.[0] = 'b') s5;
  assert (StringSet.length s5 = 2);
  assert (StringSet.mem s5 "bar");
  assert (StringSet.mem s5 "baz");
  (* add_seq *)
  let s6 = StringSet.create 4 in
  StringSet.add_seq s6 (List.to_seq ["x"; "y"; "z"]);
  assert (StringSet.length s6 = 3);
  (* stats *)
  let st = StringSet.stats s in
  assert (st.Hashtbl.num_bindings = 2)

(* ===== Stress test ===== *)

let () =
  let n = 10_000 in
  let s = Hash_set.create n in
  (* Add elements 0 to n-1 *)
  for i = 0 to n - 1 do Hash_set.add s i done;
  assert (Hash_set.length s = n);
  (* All elements present *)
  for i = 0 to n - 1 do assert (Hash_set.mem s i) done;
  (* Remove even elements *)
  for i = 0 to n - 1 do
    if i mod 2 = 0 then Hash_set.remove s i
  done;
  assert (Hash_set.length s = n / 2);
  for i = 0 to n - 1 do
    if i mod 2 = 0
    then assert (not (Hash_set.mem s i))
    else assert (Hash_set.mem s i)
  done;
  (* filter_inplace removes remaining elements divisible by 3 *)
  Hash_set.filter_inplace (fun x -> x mod 3 <> 0) s;
  Hash_set.iter (fun x -> assert (x mod 3 <> 0)) s;
  (* of_seq / to_seq round-trip *)
  let s2 = Hash_set.of_seq (Hash_set.to_seq s) in
  assert (Hash_set.equal s s2)

let () = print_endline "OK"
