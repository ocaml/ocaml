(* TEST
   * expect
*)

module M : sig
  type 'a t

  type _ typ =
    | Foo : 'a -> [`Foo of 'a] typ
    | Bar : string -> [`Bar] typ

  val use_bar : [`Bar] t -> int

  val foo : [`Foo of int] t

end = struct
  type 'a t = string

  type _ typ =
    | Foo : 'a -> [`Foo of 'a] typ
    | Bar : string -> [`Bar] typ

  let foo = "foo"

  let use_bar _ = 0
end;;
[%%expect {|
module M :
  sig
    type 'a t
    type _ typ =
        Foo : 'a -> [ `Foo of 'a ] typ
      | Bar : string -> [ `Bar ] typ
    val use_bar : [ `Bar ] t -> int
    val foo : [ `Foo of int ] t
  end
|}];;

let go (type a) (typ : a M.typ) (msg : a M.t) =
  match typ with
  | Bar s ->
    (match M.use_bar msg with _ -> ())
;;
[%%expect {|
Lines 2-4, characters 2-38:
2 | ..match typ with
3 |   | Bar s ->
4 |     (match M.use_bar msg with _ -> ())
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Foo _
val go : 'a M.typ -> 'a M.t -> unit = <fun>
|}];;
