(* TEST
 expect;
*)

type (_, _) comp =
  | Eq : ('a, 'a) comp
  | Diff : ('a, 'b) comp
;;

module U = struct type t = T end;;

module M : sig
  type t = T
  val comp : (U.t, t) comp
end = struct
  include U
  let comp = Eq
end;;

match M.comp with | Diff -> false;;
[%%expect{|
type (_, _) comp = Eq : ('a, 'a) comp | Diff : ('a, 'b) comp
module U : sig type t = T end
module M : sig type t = T val comp : (U.t, t) comp end
Line 16, characters 0-33:
16 | match M.comp with | Diff -> false;;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Eq"

Exception: Match_failure ("", 16, 0).
|}];;

module U = struct type t = {x : int} end;;

module M : sig
  type t = {x : int}
  val comp : (U.t, t) comp
end = struct
  include U
  let comp = Eq
end;;

match M.comp with | Diff -> false;;
[%%expect{|
module U : sig type t = { x : int; } end
module M : sig type t = { x : int; } val comp : (U.t, t) comp end
Line 11, characters 0-33:
11 | match M.comp with | Diff -> false;;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Eq"

Exception: Match_failure ("", 11, 0).
|}];;
