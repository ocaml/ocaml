(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Misc

module type PrintableHashOrdered = sig
  type t
  val compare : t -> t -> int
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
end

module type ExtMap = sig
  module M : PrintableHashOrdered
  include Map.S with type key = M.t
                 and type 'a t = 'a Map.Make(M).t
  val map_option : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val of_list : (key * 'a) list -> 'a t
  val disjoint_union : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
  val union_right : 'a t -> 'a t -> 'a t
  val union_left : 'a t -> 'a t -> 'a t
  val union_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val rename : key t -> key -> key
  val map_keys : (key -> key) -> 'a t -> 'a t
  val keys : 'a t -> Set.Make(M).t
  val of_set : (key -> 'a) -> Set.Make(M).t -> 'a t
  val revert : key t -> key t
  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type ExtSet = sig
  module M : PrintableHashOrdered
  include Set.S with type elt = M.t
                 and type t = Set.Make(M).t
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
end

module type ExtHashtbl = sig
  module M : PrintableHashOrdered
  include Hashtbl.S with type key = M.t
                     and type 'a t = 'a Hashtbl.Make(M).t
  val to_map : 'a t -> 'a Map.Make(M).t
  val memoize : 'a t -> (key -> 'a) -> key -> 'a
end


module ExtMap(M:PrintableHashOrdered) : ExtMap with module M := M =
struct
  include Map.Make(M)
  let map_option f m =
    fold (fun id v map ->
        match f id v with
        | None -> map
        | Some r -> add id r map) m empty
  let of_list l =
    List.fold_left (fun map (id,v) -> add id v map) empty l
  let disjoint_union ?eq m1 m2 =
    merge (fun id x y -> match x, y with
        | None, None -> None
        | None, Some v | Some v, None -> Some v
        | Some v1, Some v2 ->
            let ok = match eq with
              | None -> false
              | Some eq -> eq v1 v2 in
            if not ok
            then
              let err = Format.asprintf "ExtMap.disjoint_union %a" M.print id in
              fatal_error err
            else Some v1) m1 m2

  let union_right m1 m2 =
    merge (fun id x y -> match x, y with
        | None, None -> None
        | None, Some v
        | Some v, None
        | Some _, Some v -> Some v) m1 m2

  let union_left m1 m2 = union_right m2 m1

  let union_merge f m1 m2 =
    let aux _ m1 m2 =
      match m1, m2 with
      | None, m | m, None -> m
      | Some m1, Some m2 ->
        Some (f m1 m2) in
    merge aux m1 m2

  let rename m v =
    try find v m with Not_found -> v
  let map_keys f m =
    of_list (List.map (fun (k,v) -> f k, v) (bindings m))
  let print f ppf s =
    let elts ppf s = iter (fun id v ->
        Format.fprintf ppf "@ (@[%a@ %a@])" M.print id f v) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

  module MSet = Set.Make(M)

  let keys map = fold (fun k _ set -> MSet.add k set) map MSet.empty
  let of_set f set = MSet.fold (fun e map -> add e (f e) map) set empty

  let revert map = fold (fun k v m -> add v k m) map empty

end

module ExtSet(M:PrintableHashOrdered) : ExtSet with module M := M =
struct
  include Set.Make(M)
  let output oc s =
    Printf.fprintf oc "( ";
    iter (fun v -> Printf.fprintf oc "%a " M.output v) s;
    Printf.fprintf oc ")"
  let print ppf s =
    let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" M.print e) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s
  let to_string s = Format.asprintf "%a" print s
  let of_list l = match l with
    | [] -> empty
    | [t] -> singleton t
    | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q
  let map f s = of_list (List.map f (elements s))
end

module ExtHashtbl(M:PrintableHashOrdered) : ExtHashtbl with module M := M =
struct
  include Hashtbl.Make(M)
  module MMap = Map.Make(M)
  let to_map v = fold MMap.add v MMap.empty
  let memoize t f = fun key ->
    try find t key with
    | Not_found ->
        let r = f key in
        add t key r;
        r
end

module type BaseId = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val name : t -> string option
  val to_string : t -> string
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
end

module type Id = sig
  include BaseId
  val create : ?name:string -> unit -> t
end

module type UnitId = sig
  module Compilation_unit : PrintableHashOrdered
  include BaseId
  val create : ?name:string -> Compilation_unit.t -> t
  val unit : t -> Compilation_unit.t
end

module Id(E:sig end) : Id = struct
  type t = int * string
  let empty_string = ""
  let create = let r = ref 0 in
    fun  ?(name=empty_string) () -> incr r; !r, name
  let equal (t1,_) (t2,_) = (t1:int) = t2
  let compare (t1,_) (t2,_) = t1 - t2
  let hash (t,_) = t
  let name (_,name) =
    if name == empty_string
    then None
    else Some name
  let to_string (t,name) =
    if name == empty_string
    then string_of_int t
    else Printf.sprintf "%s_%i" name t
  let output fd t = output_string fd (to_string t)
  let print ppf v = Format.pp_print_string ppf (to_string v)
end

module UnitId(Innerid:Id)(Compilation_unit:PrintableHashOrdered) :
  UnitId with module Compilation_unit := Compilation_unit = struct
  type t = {
    id : Innerid.t;
    unit : Compilation_unit.t;
  }
  let compare x y =
    let c = Innerid.compare x.id y.id in
    if c <> 0
    then c
    else Compilation_unit.compare x.unit y.unit
  let output oc x =
    Printf.fprintf oc "%a.%a"
      Compilation_unit.output x.unit
      Innerid.output x.id
  let print ppf x =
    Format.fprintf ppf "%a.%a"
      Compilation_unit.print x.unit
      Innerid.print x.id
  let hash off = Hashtbl.hash off
  let equal o1 o2 = compare o1 o2 = 0
  let name o = Innerid.name o.id
  let to_string x =
    Format.asprintf "%a.%a"
      Compilation_unit.print x.unit
      Innerid.print x.id
  let create ?name unit =
    let id = Innerid.create ?name () in
    { id; unit }
  let unit x = x.unit
end

module Int = struct
  type t = int
  let compare x y = x - y
  let output oc x = Printf.fprintf oc "%i" x
  let hash i = i
  let equal (i:int) j = i = j
  let print = Format.pp_print_int
end

module String_M = struct
  type t = string
  let compare = String.compare
  let output = output_string
  let hash (s:string) = Hashtbl.hash s
  let equal (s1:string) s2 = s1 = s2
  let print = Format.pp_print_string
end

module IntSet = ExtSet(Int)
module IntMap = ExtMap(Int)
module IntTbl = ExtHashtbl(Int)

module StringSet = ExtSet(String_M)
module StringMap = ExtMap(String_M)
module StringTbl = ExtHashtbl(String_M)
