(* TEST
   flags = "-dshape"
   * expect
*)

(* Make sure that shapes of compilation units are never eagerly loaded,
   regardless of the context. *)

module Mdirect = Stdlib__Unit
[%%expect{|
{
 "Mdirect"[module] -> CU Stdlib__Unit;
 }
module Mdirect = Unit
|}]

module Mproj = Stdlib.Unit
[%%expect{|
{
 "Mproj"[module] -> (CU Stdlib . "Unit"[module])<.1>;
 }
module Mproj = Unit
|}]

module F (X : sig type t end) = X
[%%expect{|
{
 "F"[module] -> Abs<.4>(X/278, X/278<.3>);
 }
module F : functor (X : sig type t end) -> sig type t = X.t end
|}]

module App_direct = F (Stdlib__Unit)
[%%expect{|
{
 "App_direct"[module] -> CU Stdlib__Unit;
 }
module App_direct : sig type t = Unit.t end
|}]

module App_proj = F (Stdlib.Unit)
[%%expect{|
{
 "App_proj"[module] -> (CU Stdlib . "Unit"[module])<.6>;
 }
module App_proj : sig type t = Unit.t end
|}]

module App_direct_indir = F (Mdirect)
[%%expect{|
{
 "App_direct_indir"[module] -> CU Stdlib__Unit;
 }
module App_direct_indir : sig type t = Mdirect.t end
|}]

module App_proj_indir = F (Mproj)
[%%expect{|
{
 "App_proj_indir"[module] -> (CU Stdlib . "Unit"[module])<.1>;
 }
module App_proj_indir : sig type t = Mproj.t end
|}]

(* In the following the shape are not loaded, we just know what the signature
   are and build shapes from them. *)

include Stdlib__Unit
[%%expect{|
{
 "compare"[value] -> CU Stdlib__Unit . "compare"[value];
 "equal"[value] -> CU Stdlib__Unit . "equal"[value];
 "t"[type] -> CU Stdlib__Unit . "t"[type];
 "to_string"[value] -> CU Stdlib__Unit . "to_string"[value];
 }
type t = unit = ()
val equal : t -> t -> bool = <fun>
val compare : t -> t -> int = <fun>
val to_string : t -> string = <fun>
|}]

include Stdlib.Unit
[%%expect{|
{
 "compare"[value] -> CU Stdlib . "Unit"[module] . "compare"[value];
 "equal"[value] -> CU Stdlib . "Unit"[module] . "equal"[value];
 "t"[type] -> CU Stdlib . "Unit"[module] . "t"[type];
 "to_string"[value] -> CU Stdlib . "Unit"[module] . "to_string"[value];
 }
type t = unit = ()
val equal : t -> t -> bool = <fun>
val compare : t -> t -> int = <fun>
val to_string : t -> string = <fun>
|}]

module Without_constraint = Set.Make(Int)
[%%expect{|
{
 "Without_constraint"[module] ->
   CU Stdlib . "Set"[module] . "Make"[module](CU Stdlib . "Int"[module])<.9>;
 }
module Without_constraint :
  sig
    type elt = Int.t
    type t = Set.Make(Int).t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
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
 "With_identity_constraint"[module] ->
   {<.12>
    "M"[module] ->
      CU Stdlib . "Set"[module] . "Make"[module](
      CU Stdlib . "Int"[module])<.10>;
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
 "With_constraining_constraint"[module] ->
   {<.16>
    "M"[module] ->
      {<.13>
       "t"[type] ->
         CU Stdlib . "Set"[module] . "Make"[module](
         CU Stdlib . "Int"[module])<.13> . "t"[type];
       };
    };
 }
module With_constraining_constraint : sig module M : sig type t end end
|}]
