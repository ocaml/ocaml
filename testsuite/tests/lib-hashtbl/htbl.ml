(* TEST *)

(* Hashtable operations, using maps as a reference *)

open Printf

module type KeyS = sig
  type t
  val to_string : t -> string
end

module type HtblSeededS = sig
    type key
    type !'a t
    val create : ?random:bool -> int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find_and_remove : 'a t -> key -> 'a option
    val find : 'a t -> key -> 'a
    val find_opt: 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val find_and_replace :'a t -> key -> 'a -> 'a option
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : _ t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
end

module Test (K: KeyS)
  (H: HtblSeededS with type key = K.t)
  (M: Map.S with type key = K.t)
= struct

  let incl_mh m h =
    try
      M.iter
        (fun k d ->
          let d' = H.find h k in if d <> d' then raise Exit)
        m;
      true
    with Exit | Not_found -> false

  let domain_hm h m =
    try
      H.iter
        (fun k d -> if not (M.mem k m) then raise Exit)
        h;
      true
    with Exit -> false

  let incl_hm h m =
    try
      H.iter
        (fun k d ->
           let d' = M.find k m in if d <> d' then raise Exit)
        h;
      true
    with Exit | Not_found -> false

  let to_list_ h : _ list =
    H.fold (fun k v acc -> (k,v) :: acc) h []
    |> List.sort Stdlib.compare

  let print_list val_to_string l =
    print_string "[|";
    List.iter (fun (k, v) ->
      print_string "("; print_string (K.to_string k); print_string ", ";
      print_string (val_to_string v); print_string ") |")
      l;
    print_endline "]"
  
  let check_to_seq val_to_string h =
    let l = to_list_ h in
    let l2 = List.of_seq (H.to_seq h) in
    let sorted = List.sort Stdlib.compare l2 in
    if l <> sorted then begin
      print_list val_to_string l;
      print_list val_to_string sorted
    end;
    assert (l = sorted)

  let check_to_seq_of_seq h =
    let h' = H.create (H.length h) in
    H.add_seq h' (H.to_seq h);
    (*printf "h.len=%d, h'.len=%d\n" (List.length @@ to_list_ h)
                                     (List.length @@ to_list_ h');*)
    assert (to_list_ h = to_list_ h')

  let test val_to_string data =
    let n = Array.length data in
    let h = H.create 51 and m = ref M.empty in
    (* Insert all data with H.add *)
    Array.iter
      (fun (k, d) -> H.add h k d; m := M.add k d !m)
      data;
    printf "Insertion: %s\n"
           (if incl_mh !m h && domain_hm h !m then "passed" else "FAILED");
    check_to_seq_of_seq h;
    check_to_seq val_to_string h;
    (* Insert all data with H.replace *)
    H.clear h; m := M.empty;
    Array.iter
      (fun (k, d) -> H.replace h k d; m := M.add k d !m)
      data;
    printf "Insertion: %s\n"
           (if incl_mh !m h && incl_hm h !m then "passed" else "FAILED");
    check_to_seq_of_seq h;
    check_to_seq val_to_string h;
    (* Remove some of the data *)
    for i = 0 to n/3 - 1 do
      let (k, _) = data.(i) in H.remove h k; m := M.remove k !m
    done;
    printf "Removal: %s\n"
      (if incl_mh !m h && incl_hm h !m then "passed" else "FAILED");
    check_to_seq_of_seq h;
    check_to_seq val_to_string h;
    H.reset h;
    ()

end

module IntKey = struct
  type t = int
  let to_string = Int.to_string
end

module StringKey = struct
  type t = string
  let to_string s = s
end

module StringPairKey = struct
  type t = string * string
  let to_string (s1, s2) = "(" ^ s1 ^ ", " ^ s2 ^ ")"
end

module StringListKey = struct
  type t = string list
  let to_string = String.concat ", "
end

module SS = struct
  type t = string
  let compare (x:t) (y:t) = Stdlib.compare x y
  let equal (x:t) (y:t) = x=y
  let seeded_hash = Hashtbl2.seeded_hash
end

module SI = struct
  type t = int
  let compare (x:t) (y:t) = Stdlib.compare x y
  let equal (x:t) (y:t) = x=y
  let seeded_hash = Hashtbl2.seeded_hash
end

module SSP = struct
  type t = string*string
  let compare (x:t) (y:t) = Stdlib.compare x y
  let equal (x:t) (y:t) = x=y
  let seeded_hash = Hashtbl2.seeded_hash
end

module SSL = struct
  type t = string list
  let compare (x:t) (y:t) = Stdlib.compare x y
  let equal (x:t) (y:t) = x=y
  let seeded_hash = Hashtbl2.seeded_hash
end

module SSA = struct
  type t = string array
  let compare (x:t) (y:t) = Stdlib.compare x y
  let equal (x:t) (y:t) = x=y
  let seeded_hash = Hashtbl2.seeded_hash
end

module MS = Map.Make(SS)
module MI = Map.Make(SI)
module MSP = Map.Make(SSP)
module MSL = Map.Make(SSL)
module MSA = Map.Make(SSA)


(* Generic hash wrapped as a functorial hash *)

module H1ofM (M: Map.S) : HtblSeededS with type key = M.key =
  struct
    type key = M.key
    type 'a t = (key, 'a) Hashtbl.t
    let create ?random:bool s = Hashtbl.create s
    let clear = Hashtbl.clear
    let reset = Hashtbl.reset
    let copy = Hashtbl.copy
    let add = Hashtbl.add
    let remove = Hashtbl.remove
    let find_and_remove = Hashtbl.find_and_remove
    let find = Hashtbl.find
    let find_opt = Hashtbl.find_opt
    let find_all = Hashtbl.find_all
    let replace = Hashtbl.replace
    let find_and_replace = Hashtbl.find_and_replace
    let mem = Hashtbl.mem
    let iter = Hashtbl.iter
    let fold = Hashtbl.fold
    let length = Hashtbl.length
    let stats = Hashtbl.stats
    let filter_map_inplace = Hashtbl.filter_map_inplace
    let to_seq = Hashtbl.to_seq
    let to_seq_keys = Hashtbl.to_seq_keys
    let to_seq_values = Hashtbl.to_seq_values
    let of_seq = Hashtbl.of_seq
    let add_seq = Hashtbl.add_seq
    let replace_seq = Hashtbl.replace_seq
  end

module H2ofM (M: Map.S) : HtblSeededS with type key = M.key =
  struct
    type key = M.key
    type 'a t = (key, 'a) Hashtbl2.t
    let create ?random:bool s = Hashtbl2.create s
    let clear = Hashtbl2.clear
    let reset = Hashtbl2.reset
    let copy = Hashtbl2.copy
    let add = Hashtbl2.add
    let remove = Hashtbl2.remove
    let find_and_remove = Hashtbl2.find_and_remove
    let find = Hashtbl2.find
    let find_opt = Hashtbl2.find_opt
    let find_all = Hashtbl2.find_all
    let replace = Hashtbl2.replace
    let find_and_replace = Hashtbl2.find_and_replace
    let mem = Hashtbl2.mem
    let iter = Hashtbl2.iter
    let fold = Hashtbl2.fold
    let length = Hashtbl2.length
    let stats = Hashtbl2.stats
    let filter_map_inplace = Hashtbl2.filter_map_inplace
    let to_seq = Hashtbl2.to_seq
    let to_seq_keys = Hashtbl2.to_seq_keys
    let to_seq_values = Hashtbl2.to_seq_values
    let of_seq = Hashtbl2.of_seq
    let add_seq = Hashtbl2.add_seq
    let replace_seq = Hashtbl2.replace_seq
  end

module HS1 = H1ofM(MS)
module HI1 = H1ofM(MI)
module HSP = H1ofM(MSP)
module HSL = H1ofM(MSL)
module H2S1 = H2ofM(MS)
module H2I1 = H2ofM(MI)
module H2SP = H2ofM(MSP)
module H2SL = H2ofM(MSL)

(* Specific functorial hashes *)

module HS2 = Hashtbl.MakeSeeded(SS)
module HS3 = Hashtbl.MakeSeeded(String)
module HI2 = Hashtbl.MakeSeeded(SI)
module H2S2 = Hashtbl2.MakeSeeded(SS)
module H2S3 = Hashtbl2.MakeSeeded(String)
module H2I2 = Hashtbl2.MakeSeeded(SI)

(* Specific weak functorial hashes *)
module WS = Ephemeron.K1.MakeSeeded(SS)
module WSP1 = Ephemeron.K1.MakeSeeded(SSP)
module WSP2 = Ephemeron.K2.MakeSeeded(SS)(SS)
module WSL = Ephemeron.K1.MakeSeeded(SSL)
module WSA = Ephemeron.Kn.MakeSeeded(SS)

(* Instantiating the test *)

module TS1 = Test(StringKey)(HS1)(MS)
module TS2 = Test(StringKey)(HS2)(MS)
module TS3 = Test(StringKey)(HS3)(MS)
module TI1 = Test(IntKey)(HI1)(MI)
module TI2 = Test(IntKey)(HI2)(MI)
module TSP = Test(StringPairKey)(HSP)(MSP)
module TSL = Test(StringListKey)(HSL)(MSL)
module T2S1 = Test(StringKey)(H2S1)(MS)
module T2S2 = Test(StringKey)(H2S2)(MS)
module T2S3 = Test(StringKey)(H2S3)(MS)
module T2I1 = Test(IntKey)(H2I1)(MI)
module T2I2 = Test(IntKey)(H2I2)(MI)
module T2SP = Test(StringPairKey)(H2SP)(MSP)
module T2SL = Test(StringListKey)(H2SL)(MSL)

(* Data set: strings from a file, associated with their line number *)

let file_data filename =
  let ic = open_in filename in
  let lineno = ref 0 in
  let data = ref [] in
  begin try
    while true do
      let l = input_line ic in
      incr lineno;
      data := (l, !lineno) :: !data
    done
  with End_of_file -> ()
  end;
  close_in ic;
  Array.of_list !data

(* Data set: fixed strings *)

let string_data = [|
  "Si", 0; "non", 1; "e", 2; "vero", 3; "e", 4; "ben", 5; "trovato", 6;
  "An", 10; "apple", 11; "a", 12; "day", 13; "keeps", 14; "the", 15;
  "doctor", 16; "away", 17;
  "Pierre", 20; "qui", 21; "roule", 22; "n'amasse", 23; "pas", 24; "mousse", 25;
  "Asinus", 30; "asinum", 31; "fricat", 32
|]

(* Data set: random integers *)

let random_integers num range =
  let data = Array.make num (0,0) in
  for i = 0 to num - 1 do
    data.(i) <- (Random.int range, i)
  done;
  data

(* Data set: pairs *)

let pair_data data =
  Array.map (fun (k, d) -> ((k, k), d)) data

(* Data set: lists *)

let list_data data =
  let d = Array.make (Array.length data / 10) ([], "0") in
  let j = ref 0 in
  let rec mklist n =
    if n <= 0 || !j >= Array.length data then [] else begin
      let hd = fst data.(!j) in
      incr j;
      let tl = mklist (n-1) in
      hd :: tl
    end in
  for i = 0 to Array.length d - 1 do
    d.(i) <- (mklist (Random.int 16), Int.to_string i)
  done;
  d

(* The test *)

  (* for Hashtbl *)

let _ =
  print_endline "Testing Hashtbl :";
  printf "-- Random integers, large range\n%!";
  TI1.test Int.to_string (random_integers 5 250);
  printf "-- Random integers, narrow range\n%!";
  TI2.test Int.to_string (random_integers 5 250);
  let d =
    try file_data "../../LICENSE" with Sys_error _ -> string_data in
  printf "-- Strings, generic interface\n%!";
  TS1.test Int.to_string d;
  printf "-- Strings, functorial interface\n%!";
  TS2.test Int.to_string d;
  printf "-- Strings, functorial(String) interface\n%!";
  TS3.test Int.to_string d;
  printf "-- Pairs of strings\n%!";
  TSP.test Int.to_string (pair_data d);
  printf "-- Lists of strings\n%!";
  TSL.test (fun v -> v) (list_data d)

let () =
  let h = Hashtbl.create 16 in
  for i = 1 to 1000 do Hashtbl.add h i (i * 2) done;
  Printf.printf "%i elements\n" (Hashtbl.length h);
  let () =
    (* Check that filter_map_inplace of nothing changes nothing *)
    let marshaled_before = Marshal.to_string h [] in
    Hashtbl.filter_map_inplace (fun _k v -> Some v) h;
    let marshaled_after = Marshal.to_string h [] in
    assert (marshaled_before = marshaled_after) in
    Hashtbl.filter_map_inplace (fun k v ->
      if k mod 100 = 0 then (Some (v / 100)) else None)
    h;
  let l = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [] in
  let l = List.sort compare l in
  List.iter (fun (k, v) -> Printf.printf "%i,%i\n" k v) l;
  Printf.printf "%i elements\n" (Hashtbl.length h)

let () =
  let h = Hashtbl.create 16 in
  Hashtbl.add h 0 0;
  assert (Hashtbl.find_and_replace h 0 1 = Some 0);
  assert (Hashtbl.find_and_remove h 0 = Some 1);
  assert (Hashtbl.find_and_remove h 0 = None);
  assert (Hashtbl.find_and_replace h 0 1 = None);
  assert (Hashtbl.find_and_remove h 0 = Some 1);
  Hashtbl.clear h;
  Hashtbl.add h 0 0;
  Hashtbl.add h 0 1;
  assert (Hashtbl.find_and_replace h 0 2 = Some 1);
  assert (Hashtbl.find_and_remove h 0 = Some 2);
  assert (Hashtbl.find_and_remove h 0 = Some 0);
  assert (Hashtbl.find_and_remove h 0 = None)

  (* for Hashtbl2 *)

let _ =
  print_endline "Testing Hashtbl2 :";
  printf "-- Random integers, large range\n%!";
  T2I1.test Int.to_string (random_integers 5 250);
  printf "-- Random integers, narrow range\n%!";
  T2I2.test Int.to_string (random_integers 5 250);
  let d =
    try file_data "../../LICENSE" with Sys_error _ -> string_data in
  printf "-- Strings, generic interface\n%!";
  T2S1.test Int.to_string d;
  printf "-- Strings, functorial interface\n%!";
  T2S2.test Int.to_string d;
  printf "-- Strings, functorial(String) interface\n%!";
  T2S3.test Int.to_string d;
  printf "-- Pairs of strings\n%!";
  T2SP.test Int.to_string (pair_data d);
  printf "-- Lists of strings\n%!";
  T2SL.test (fun v -> v) (list_data d)

let () =
  let h = Hashtbl2.create 16 in
  for i = 1 to 1000 do Hashtbl2.add h i (i * 2) done;
  Printf.printf "%i elements\n" (Hashtbl2.length h);
  let () =
    (* Check that filter_map_inplace of nothing changes nothing *)
    let marshaled_before = Marshal.to_string h [] in
    Hashtbl2.filter_map_inplace (fun _k v -> Some v) h;
    let marshaled_after = Marshal.to_string h [] in
    assert (marshaled_before = marshaled_after) in
    Hashtbl2.filter_map_inplace (fun k v ->
      if k mod 100 = 0 then (Some (v / 100)) else None)
    h;
  let l = Hashtbl2.fold (fun k v acc -> (k, v) :: acc) h [] in
  let l = List.sort compare l in
  List.iter (fun (k, v) -> Printf.printf "%i,%i\n" k v) l;
  Printf.printf "%i elements\n" (Hashtbl2.length h)

let () =
  let h = Hashtbl2.create 16 in
  Hashtbl2.add h 0 0;
  assert (Hashtbl2.find_and_replace h 0 1 = Some 0);
  assert (Hashtbl2.find_and_remove h 0 = Some 1);
  assert (Hashtbl2.find_and_remove h 0 = None);
  assert (Hashtbl2.find_and_replace h 0 1 = None);
  assert (Hashtbl2.find_and_remove h 0 = Some 1);
  Hashtbl2.clear h;
  Hashtbl2.add h 0 0;
  Hashtbl2.add h 0 1;
  assert (Hashtbl2.find_and_replace h 0 2 = Some 1);
  assert (Hashtbl2.find_and_remove h 0 = Some 2);
  assert (Hashtbl2.find_and_remove h 0 = Some 0);
  assert (Hashtbl2.find_and_remove h 0 = None);