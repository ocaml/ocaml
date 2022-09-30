(* TEST
   * expect
*)

open Set.Make(String);;
[%%expect{|
type elt = String.t
type t = Set.Make(String).t
val empty : t = <abstr>
val add : elt -> t -> t = <fun>
val singleton : elt -> t = <fun>
val remove : elt -> t -> t = <fun>
val union : t -> t -> t = <fun>
val inter : t -> t -> t = <fun>
val disjoint : t -> t -> bool = <fun>
val diff : t -> t -> t = <fun>
val cardinal : t -> int = <fun>
val elements : t -> elt list = <fun>
val min_elt : t -> elt = <fun>
val min_elt_opt : t -> elt option = <fun>
val max_elt : t -> elt = <fun>
val max_elt_opt : t -> elt option = <fun>
val choose : t -> elt = <fun>
val choose_opt : t -> elt option = <fun>
val find : elt -> t -> elt = <fun>
val find_opt : elt -> t -> elt option = <fun>
val find_first : (elt -> bool) -> t -> elt = <fun>
val find_first_opt : (elt -> bool) -> t -> elt option = <fun>
val find_last : (elt -> bool) -> t -> elt = <fun>
val find_last_opt : (elt -> bool) -> t -> elt option = <fun>
val iter : (elt -> unit) -> t -> unit = <fun>
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a = <fun>
val map : (elt -> elt) -> t -> t = <fun>
val filter : (elt -> bool) -> t -> t = <fun>
val filter_map : (elt -> elt option) -> t -> t = <fun>
val partition : (elt -> bool) -> t -> t * t = <fun>
val split : elt -> t -> t * bool * t = <fun>
val is_empty : t -> bool = <fun>
val mem : elt -> t -> bool = <fun>
val equal : t -> t -> bool = <fun>
val compare : t -> t -> int = <fun>
val subset : t -> t -> bool = <fun>
val for_all : (elt -> bool) -> t -> bool = <fun>
val exists : (elt -> bool) -> t -> bool = <fun>
val to_list : t -> elt list = <fun>
val of_list : elt list -> t = <fun>
val to_seq_from : elt -> t -> elt Seq.t = <fun>
val to_seq : t -> elt Seq.t = <fun>
val to_rev_seq : t -> elt Seq.t = <fun>
val add_seq : elt Seq.t -> t -> t = <fun>
val of_seq : elt Seq.t -> t = <fun>
|}]

let e = empty;;
[%%expect{|
val e : t = <abstr>
|}]

open struct
  let x = singleton "hidden"
end;;
[%%expect{|
val x : t = <abstr>
|}];;

elements (union x (of_list ["a"; "b"]));;
[%%expect{|
- : elt list = ["a"; "b"; "hidden"]
|}]

let f =
  let open Set.Make(Int32) in
  let e2 = empty in
  let open struct
    let y = 3
  end in
  (e, e2, y);;
[%%expect{|
val f : t * Set.Make(Int32).t * int = (<abstr>, <abstr>, 3)
|}]

module type S = sig
  open Set.Make(Bool)

  type nonrec t = t
end;;
[%%expect{|
module type S = sig type nonrec t = Set.Make(Bool).t end
|}]

let hd _ = ();;
[%%expect{|
val hd : 'a -> unit = <fun>
|}]

open (List : sig val map : ('a -> 'b) -> 'a list -> 'b list end);;
[%%expect{|
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
|}]

let l = map succ [0;1;2;3]
let () = hd l;;
[%%expect{|
val l : int list = [1; 2; 3; 4]
|}]

let y = map succ [];;
[%%expect{|
val y : int list = []
|}]
