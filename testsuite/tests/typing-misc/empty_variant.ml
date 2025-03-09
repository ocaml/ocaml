(* TEST
 expect;
*)

(* empty variant *)
type t = |;;
[%%expect{|
type t = |
|}];;

let f (x:t) = match x with _ -> .
[%%expect{|
val f : t -> 'a = <fun>
|}];;

type m = A of t | B of int * t | C of {g:t}
[%%expect{|
type m = A of t | B of int * t | C of { g : t; }
|}]

let g (x:m) =
  match x with
  | A _ | B _ | C _ -> .
[%%expect{|
val g : m -> 'a = <fun>
|}]

let f : t option -> int = function None -> 3
[%%expect{|
val f : t option -> int = <fun>
|}]

type nothing = |
type ('a, 'b, 'c) t = | A of 'a | B of 'b | C of 'c
module Runner : sig
  val ac : f:((unit, _, unit) t -> unit) -> unit
end = struct
  let ac ~f =
    f (A ());
    f (C ());
  ;;
end

let f () =
  Runner.ac
    ~f:(fun (abc : (_,nothing,_) t) ->
      let value =
        match abc with
        | A _ -> 1
      in
      Printf.printf "%i\n" value
    )
[%%expect{|
type nothing = |
type ('a, 'b, 'c) t = A of 'a | B of 'b | C of 'c
module Runner : sig val ac : f:((unit, 'a, unit) t -> unit) -> unit end
Lines 16-17, characters 8-18:
16 | ........match abc with
17 |         | A _ -> 1
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C ()"

val f : unit -> unit = <fun>
|}]

type nothing = |
type 'b t = A | B of 'b | C
let g (x:nothing t) = match x with A -> ()
[%%expect{|
type nothing = |
type 'b t = A | B of 'b | C
Line 3, characters 22-42:
3 | let g (x:nothing t) = match x with A -> ()
                          ^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "C"

val g : nothing t -> unit = <fun>
|}]
