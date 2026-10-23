(* ../ocamlopt.opt -nostdlib -I ../stdlib hashtbl_vs_hashtbl2.ml \
   -o hashtbl_vs_hashtbl2.exe *)

let get_param p =
  try Sys.getenv p with _ ->
    Printf.ksprintf failwith
      "The environment variable %S must be defined." p

let get_int_param p =
  let s = get_param p in
  try int_of_string s with _ ->
    Printf.ksprintf failwith
      "The environment variable %S=%S must be an integer."
      p s

let get_dict_param p dict =
  let s = get_param p in
  try List.assoc s dict with _ ->
    Printf.ksprintf failwith
      "The environment variable %S=%S must be among [ %s ]."
      p s (String.concat ", " (List.map fst dict))

type impl =
  | Hashtbl
  | Hashtbl2

type workload =
  | Find_replace
  | Add
  | Add_remove
  | Fold
  | Filter_map_inplace

let impl = get_dict_param "IMPL" [
  "hashtbl", Hashtbl;
  "hashtbl2", Hashtbl2;
]

let workload = get_dict_param "FUNCTION" [
  "find_replace", Find_replace;
  "add", Add;
  "add_remove", Add_remove;
  "fold", Fold;
  "filter_map_inplace", Filter_map_inplace;
]

module type HtblSeededS = sig
    type key
    type !'a t
    val create : ?random:bool -> int -> 'a t
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val replace : 'a t -> key -> 'a -> unit
    val find : 'a t -> key -> 'a
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Benchmarks (H: HtblSeededS with type key = int) = struct

  let adds () =
    let size = get_int_param "SIZE" in
    let iterations = get_int_param "ITERATIONS" in 
    for j = 1 to iterations do 
      let table = H.create 0 in
      for i = 0 to size do
        H.add table i i
      done;
    done
  
  let add_and_remove () =
    let add_number = get_int_param "ADD" in
    let remove_number = get_int_param "REMOVE" in
    let iterations = get_int_param "ITERATIONS" in
    let table = H.create 0 in
    for i = 1 to iterations do
      for j = 1 to add_number do
        H.replace table j j
      done;
      for k = 1 to remove_number do
        H.remove table k
      done
    done

  let find_and_replace () =
    let size = get_int_param "SIZE" in
    let find_number = get_int_param "FIND" in
    let replace_number = get_int_param "REPLACE" in
    let iterations = get_int_param "ITERATIONS" in
    let indices num = Array.init num (fun _ -> Random.int size) in
    let find_indices = indices find_number in
    let replace_indices = indices replace_number in
    for j = 1 to iterations do
      let table = H.create 0 in
      for i = 1 to size do
        H.add table i i
      done;
      for i = 0 to find_number - 1 do
        let idx = find_indices.(i) in
        try ignore (H.find table idx) with Not_found -> ()
      done;
      for i = 0 to replace_number - 1 do
        let idx = replace_indices.(i) in
        H.replace table idx i
      done;
    done

  let fold () =
    let size = get_int_param "SIZE" in
    let iterations = get_int_param "ITERATIONS" in
    let table = H.create 0 in
    for i = 1 to size do
      H.add table i i
    done;
    for i = 1 to iterations do
      ignore (H.fold (fun _ i acc -> acc + i) table 0)
    done

  let filter_map_inplace () =
    let size = get_int_param "SIZE" in
    let ratio = get_int_param "RATIO" in
    let iterations = get_int_param "ITERATIONS" in
    let alea = Array.init size (fun _ -> Random.int 100) in
    let table = H.create 0 in
    for i = 1 to size do
      H.add table i i
    done;
    for i = 1 to iterations do
      let copy = H.copy table in
      for l = 1 to 10 do
        H.filter_map_inplace (fun k v ->
          if alea.(k mod size) < ratio then Some (v+1) else None) copy
        done
    done

end

module Impl1 = struct

  type key = int
  type !'a t = (key, 'a) Hashtbl.t
  let create = Hashtbl.create
  let copy = Hashtbl.copy
  let add = Hashtbl.add
  let remove = Hashtbl.remove
  let replace = Hashtbl.replace
  let find = Hashtbl.find
  let iter = Hashtbl.iter
  let filter_map_inplace = Hashtbl.filter_map_inplace
  let fold = Hashtbl.fold

end

module Impl2 = struct

  type key = int
  type !'a t = (key, 'a) Hashtbl2.t
  let create = Hashtbl2.create
  let copy = Hashtbl2.copy
  let add = Hashtbl2.add
  let remove = Hashtbl2.remove
  let replace = Hashtbl2.replace
  let find = Hashtbl2.find
  let iter = Hashtbl2.iter
  let filter_map_inplace = Hashtbl2.filter_map_inplace
  let fold = Hashtbl2.fold

end

module Bench1 = Benchmarks (Impl1)
module Bench2 = Benchmarks (Impl2)

let work =
  match impl with
  | Hashtbl ->
    begin match workload with
      | Find_replace -> Bench1.find_and_replace
      | Add -> Bench1.adds
      | Add_remove -> Bench1.add_and_remove
      | Fold -> Bench1.fold
      | Filter_map_inplace -> Bench1.filter_map_inplace
    end
  | Hashtbl2 ->
    begin match workload with
      | Find_replace -> Bench2.find_and_replace
      | Add -> Bench2.adds
      | Add_remove -> Bench2.add_and_remove
      | Fold -> Bench2.fold
      | Filter_map_inplace -> Bench2.filter_map_inplace
    end

let () = work ()
