(* TEST
 expect;
 *)

type nothing = |
type (_, _) eq = Eq : ('a, 'a) eq
[%%expect{|
type nothing = |
type (_, _) eq = Eq : ('a, 'a) eq
|}];;

(* This definition is a bit unusual, as [type a] is unified with a function type.
   This avoids unsoundness.
 *)
let ok (type a) ?opt:((Eq : (a, int -> int) eq) = assert false) () : a =
  function x -> x + 1;;
[%%expect{|
val ok : ?opt:('a -> 'b, int -> int) eq -> unit -> 'a -> 'b = <fun>
|}];;

let (x : string -> nothing) = ok ();;
x "hello";;
[%%expect{|
val x : string -> nothing = <fun>
Exception: Assert_failure ("", 1, 50).
|}];;

(* Notably you can't give [x] non-arrow types; that would be unsound *)
print_endline (ok ());;
[%%expect{|
Line 1, characters 14-21:
1 | print_endline (ok ());;
                  ^^^^^^^
Error: This expression has type "'a -> 'b"
       but an expression was expected of type "string"
  Hint: This function application is partial, maybe some arguments
  are missing.
|}];;

(* And the fully polymorphic definition is rejected. *)
let bad : type a. ?opt:(a, int -> int) eq -> unit -> a =
  fun ?opt:((Eq : (a, int -> int) eq) = assert false) () x -> x + 1;;

[%%expect{|
Line 2, characters 2-67:
2 |   fun ?opt:((Eq : (a, int -> int) eq) = assert false) () x -> x + 1;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The syntactic arity of the function doesn't match the type constraint:
       This function has 3 syntactic arguments, but its type is constrained to
         "?opt:(a, int -> int) eq -> unit -> a".
        Hint: consider splitting the function definition into
          "fun ... gadt_pat -> fun ..."
          where "gadt_pat" is the pattern with the GADT constructor that
          introduces the local type equation on "a".
|}];;

(* Workaround 1: no GADT in default argument pattern *)

let workaround1 (type a) ?opt:((opt : (a, int -> int) eq) = assert false) () : a =
  match opt with
  | Eq -> (function x -> x + 1);;

[%%expect{|
val workaround1 : ?opt:('a, int -> int) eq -> unit -> 'a = <fun>
|}];;

(* Workaround 2: Use an expression body instead of a function body *)

let workaround2 (type a) ?opt:((Eq : (a, int -> int) eq) = assert false) () : a =
  (function x -> x + 1);;
[%%expect{|
val workaround2 : ?opt:('a, int -> int) eq -> unit -> 'a = <fun>
|}];;

let (x : nothing) = workaround2 ();;
[%%expect{|
Exception: Assert_failure ("", 1, 59).
|}];;

(* The corresponding check for partial matches on GADTs *)
type (_, _) eq_or_not =
  | Eq : ('a, 'a) eq_or_not
  | Neq : ('a, 'b) eq_or_not

let ok (type a) (Eq : (a, int -> int) eq_or_not) : a =
  function x -> x + 1;;

[%%expect{|
type (_, _) eq_or_not = Eq : ('a, 'a) eq_or_not | Neq : ('a, 'b) eq_or_not
Line 5, characters 16-48:
5 | let ok (type a) (Eq : (a, int -> int) eq_or_not) : a =
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Neq"

val ok : ('a -> 'b, int -> int) eq_or_not -> 'a -> 'b = <fun>
|}];;

let bad : type a. (a, int -> int) eq_or_not -> a =
  fun (Eq : (a, int -> int) eq_or_not) x -> x + 1;;

[%%expect{|
Line 2, characters 6-38:
2 |   fun (Eq : (a, int -> int) eq_or_not) x -> x + 1;;
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Neq"

Line 2, characters 2-49:
2 |   fun (Eq : (a, int -> int) eq_or_not) x -> x + 1;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The syntactic arity of the function doesn't match the type constraint:
       This function has 2 syntactic arguments, but its type is constrained to
         "(a, int -> int) eq_or_not -> a".
        Hint: consider splitting the function definition into
          "fun ... gadt_pat -> fun ..."
          where "gadt_pat" is the pattern with the GADT constructor that
          introduces the local type equation on "a".
|}];;


(* The corresponding check for lazy matches on GADTs *)
let ok (type a) (lazy (Eq : (a, int -> int) eq)) : a =
  function x -> x + 1

[%%expect{|
val ok : ('a -> 'b, int -> int) eq lazy_t -> 'a -> 'b = <fun>
|}];;

let bad : type a. (a, int -> int) eq lazy_t -> a =
  fun (lazy Eq) x -> x + 1

[%%expect{|
Line 2, characters 2-26:
2 |   fun (lazy Eq) x -> x + 1
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The syntactic arity of the function doesn't match the type constraint:
       This function has 2 syntactic arguments, but its type is constrained to
         "(a, int -> int) eq lazy_t -> a".
        Hint: consider splitting the function definition into
          "fun ... gadt_pat -> fun ..."
          where "gadt_pat" is the pattern with the GADT constructor that
          introduces the local type equation on "a".
|}];;


(* This definition is rejected but isn't unsound. *)

let spurious : type a. (a, int -> int) eq -> a =
  fun Eq x -> x

[%%expect{|
Line 2, characters 2-15:
2 |   fun Eq x -> x
      ^^^^^^^^^^^^^
Error: The syntactic arity of the function doesn't match the type constraint:
       This function has 2 syntactic arguments, but its type is constrained to
         "(a, int -> int) eq -> a".
        Hint: consider splitting the function definition into
          "fun ... gadt_pat -> fun ..."
          where "gadt_pat" is the pattern with the GADT constructor that
          introduces the local type equation on "a".
|}];;
