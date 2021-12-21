(* TEST
   flags = "-dshape"
   * expect
*)

(* Make sure that shapes of compilation units are never eagerly loaded,
   regardless of the context. *)

module Mdirect = Stdlib__Unit
[%%expect{|
{
 ("Mdirect", module) -> CU Stdlib__Unit;
 }
module Mdirect = Unit
|}]

module Mproj = Stdlib.Unit
[%%expect{|
{
 ("Mproj", module) -> (CU Stdlib . "Unit"[module])<.1>;
 }
module Mproj = Unit
|}]

module F (X : sig type t end) = X
[%%expect{|
{
 ("F", module) -> Abs<.4>(X/282, X/282<.3>);
 }
module F : functor (X : sig type t end) -> sig type t = X.t end
|}]

module App_direct = F (Stdlib__Unit)
[%%expect{|
{
 ("App_direct", module) -> CU Stdlib__Unit;
 }
module App_direct : sig type t = Unit.t end
|}]

module App_proj = F (Stdlib.Unit)
[%%expect{|
{
 ("App_proj", module) -> (CU Stdlib . "Unit"[module])<.6>;
 }
module App_proj : sig type t = Unit.t end
|}]

module App_direct_indir = F (Mdirect)
[%%expect{|
{
 ("App_direct_indir", module) -> CU Stdlib__Unit;
 }
module App_direct_indir : sig type t = Mdirect.t end
|}]

module App_proj_indir = F (Mproj)
[%%expect{|
{
 ("App_proj_indir", module) -> (CU Stdlib . "Unit"[module])<.1>;
 }
module App_proj_indir : sig type t = Mproj.t end
|}]

(* In the following the shape are not loaded, we just know what the signature
   are and build shapes from them. *)

include Stdlib__Unit
[%%expect{|
{
 ("compare", value) -> CU Stdlib__Unit . "compare"[value];
 ("equal", value) -> CU Stdlib__Unit . "equal"[value];
 ("t", type) -> CU Stdlib__Unit . "t"[type];
 ("to_string", value) -> CU Stdlib__Unit . "to_string"[value];
 }
type t = unit = ()
val equal : t -> t -> bool = <fun>
val compare : t -> t -> int = <fun>
val to_string : t -> string = <fun>
|}]

include Stdlib.Unit
[%%expect{|
{
 ("compare", value) -> CU Stdlib . "Unit"[module] . "compare"[value];
 ("equal", value) -> CU Stdlib . "Unit"[module] . "equal"[value];
 ("t", type) -> CU Stdlib . "Unit"[module] . "t"[type];
 ("to_string", value) -> CU Stdlib . "Unit"[module] . "to_string"[value];
 }
type t = unit = ()
val equal : t -> t -> bool = <fun>
val compare : t -> t -> int = <fun>
val to_string : t -> string = <fun>
|}]

module Without_constraint = Set.Make(Int)
[%%expect{|
{
 ("Without_constraint", module) ->
     CU Stdlib . "Set"[module] . "Make"[module](
     CU Stdlib . "Int"[module])<.9>;
 }
module Without_constraint :
  sig
    type elt = Int.t
    type t = Set.Make(Int).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
|}]

module With_identity_constraint : sig
  module M : Set.S
end = struct
  module M = Set.Make(Int)
end
[%%expect{|
{
 ("With_identity_constraint", module) ->
     {<.12>
      ("M", module) ->
          {<.10>
           ("add", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "add"[value];
           ("add_seq", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "add_seq"[value];
           ("cardinal", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "cardinal"[value];
           ("choose", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "choose"[value];
           ("choose_opt", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "choose_opt"[value];
           ("compare", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "compare"[value];
           ("diff", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "diff"[value];
           ("disjoint", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "disjoint"[value];
           ("elements", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "elements"[value];
           ("elt", type) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "elt"[type];
           ("empty", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "empty"[value];
           ("equal", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "equal"[value];
           ("exists", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "exists"[value];
           ("filter", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "filter"[value];
           ("filter_map", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "filter_map"[value];
           ("find", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "find"[value];
           ("find_first", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "find_first"[value];
           ("find_first_opt", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "find_first_opt"[value];
           ("find_last", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "find_last"[value];
           ("find_last_opt", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "find_last_opt"[value];
           ("find_opt", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "find_opt"[value];
           ("fold", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "fold"[value];
           ("for_all", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "for_all"[value];
           ("inter", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "inter"[value];
           ("is_empty", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "is_empty"[value];
           ("iter", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "iter"[value];
           ("map", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "map"[value];
           ("max_elt", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "max_elt"[value];
           ("max_elt_opt", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "max_elt_opt"[value];
           ("mem", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "mem"[value];
           ("min_elt", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "min_elt"[value];
           ("min_elt_opt", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "min_elt_opt"[value];
           ("of_list", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "of_list"[value];
           ("of_seq", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "of_seq"[value];
           ("partition", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "partition"[value];
           ("remove", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "remove"[value];
           ("singleton", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "singleton"[value];
           ("split", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "split"[value];
           ("subset", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "subset"[value];
           ("t", type) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "t"[type];
           ("to_rev_seq", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "to_rev_seq"[value];
           ("to_seq", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "to_seq"[value];
           ("to_seq_from", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "to_seq_from"[value];
           ("union", value) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.10> . "union"[value];
           };
      };
 }
module With_identity_constraint : sig module M : Set.S end
|}]

module With_constraining_constraint : sig
  module M : sig type t end
end = struct
  module M = Set.Make(Int)
end
[%%expect{|
{
 ("With_constraining_constraint", module) ->
     {<.16>
      ("M", module) ->
          {<.13>
           ("t", type) ->
               CU Stdlib . "Set"[module] . "Make"[module](
               CU Stdlib . "Int"[module])<.13> . "t"[type];
           };
      };
 }
module With_constraining_constraint : sig module M : sig type t end end
|}]
