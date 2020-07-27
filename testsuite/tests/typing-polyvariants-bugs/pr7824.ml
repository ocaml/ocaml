(* TEST
   * expect
*)

module Element : sig
  type +'a t

  val from_a  : [`A] t -> unit
  val from_ab : [< `A | `B] t -> unit

  val to_a  : unit -> [`A] t
  val to_ab : unit -> [< `A | `B] t
end = struct
  type +'a t

  let from_a x = assert false
  let from_ab x = assert false

  let to_a x = assert false
  let to_ab x = assert false
end ;;
[%%expect{|
module Element :
  sig
    type +'a t
    val from_a : [ `A ] t -> unit
    val from_ab : [< `A | `B ] t -> unit
    val to_a : unit -> [ `A ] t
    val to_ab : unit -> [< `A | `B ] t
  end
|}];;

let f x =
  Element.from_a x;
  Element.from_ab x;
  match [] with
  | _::_ -> (x :> [`A | `C] Element.t)
;;
[%%expect{|
Lines 4-5, characters 2-38:
4 | ..match [] with
5 |   | _::_ -> (x :> [`A | `C] Element.t)
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]
val f : [ `A ] Element.t -> [ `A | `C ] Element.t = <fun>
|}];;

type _ t = T : 'a -> 'a t

let f x =
  Element.from_a x;
  Element.from_ab x;
  match T () with
  | T _ -> (x :> [`A | `C] Element.t)
;;
[%%expect{|
type _ t = T : 'a -> 'a t
val f : [ `A ] Element.t -> [ `A | `C ] Element.t = <fun>
|}];;

let f () =
  let open Element in
  let x = if true then to_ab () else to_a () in
  (x :> [ `A | `C ] Element.t)
;;
[%%expect{|
val f : unit -> [ `A | `C ] Element.t = <fun>
|}];;

let f () =
  let open Element in
  let x = if true then to_a () else to_ab () in
  (x :> [ `A | `C ] Element.t)
;;
[%%expect{|
val f : unit -> [ `A | `C ] Element.t = <fun>
|}];;
