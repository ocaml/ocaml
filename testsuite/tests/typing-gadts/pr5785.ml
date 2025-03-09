(* TEST
 expect;
*)

module Add (T : sig type two end) =
struct
  type _ t =
  | One : [`One] t
  | Two : T.two t

  let add (type a) : a t * a t -> string = function
    | One, One -> "two"
    | Two, Two -> "four"
end;;
[%%expect{|
Lines 7-9, characters 43-24:
7 | ...........................................function
8 |     | One, One -> "two"
9 |     | Two, Two -> "four"
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "(One, Two)"

module Add :
  (T : sig type two end) ->
    sig
      type _ t = One : [ `One ] t | Two : T.two t
      val add : 'a t * 'a t -> string
    end
|}];;
