(* TEST
   * expect
*)

(**
   Check the behavior of with constraints with respect to
    ghost type items introduced for class and class types
 *)

module type s = sig
  class type c = object method m: int end
end with type c := <m : int >
[%%expect {|
Lines 6-8, characters 16-29:
6 | ................sig
7 |   class type c = object method m: int end
8 | end with type c := <m : int >
Error: The signature constrained by `with' has no component named c
|}]


module type s = sig
  class type ct = object method m: int end
end with type ct := <m : int >
[%%expect {|
Lines 1-3, characters 16-30:
1 | ................sig
2 |   class type ct = object method m: int end
3 | end with type ct := <m : int >
Error: The signature constrained by `with' has no component named ct
|}]

(** Check that we keep the same structure even after replacing a ghost item *)

module type s = sig
  type top
  and t = private < .. >
  and mid
  and u = private < .. >
  and v
end with type t = private < .. >
    with type u = private < .. >
[%%expect {|
module type s =
  sig
    type top
    and t = private < .. >
    and mid
    and u = private < .. >
    and v
  end
|}]
