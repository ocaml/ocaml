(* TEST
 expect;
*)
let foo ?x = ()
[%%expect{|
Line 1, characters 9-10:
1 | let foo ?x = ()
             ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

val foo : ?x:'a -> unit = <fun>
|}]

let foo ?x ~y = ()
[%%expect{|
Line 1, characters 9-10:
1 | let foo ?x ~y = ()
             ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

val foo : ?x:'a -> y:'b -> unit = <fun>
|}]

let foo ?x () = ()
[%%expect{|
val foo : ?x:'a -> unit -> unit = <fun>
|}]

let foo ?x ~y () = ()
[%%expect{|
val foo : ?x:'a -> y:'b -> unit -> unit = <fun>
|}]

class bar ?x = object end
[%%expect{|
Line 1, characters 11-12:
1 | class bar ?x = object end
               ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

class bar : ?x:'a -> object  end
|}]

class bar ?x ~y = object end
[%%expect{|
Line 1, characters 11-12:
1 | class bar ?x ~y = object end
               ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

class bar : ?x:'a -> y:'b -> object  end
|}]

class bar ?x () = object end
[%%expect{|
class bar : ?x:'a -> unit -> object  end
|}]

class foo ?x ~y () = object end
[%%expect{|
class foo : ?x:'a -> y:'b -> unit -> object  end
|}]

let baz y =
  let foo ?x = y in
  ignore (y : unit);
  foo
[%%expect{|
Line 2, characters 11-12:
2 |   let foo ?x = y in
               ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

val baz : unit -> ?x:'a -> unit = <fun>
|}]

#rectypes

let rec baz ?x = baz
[%%expect{|
Line 1, characters 13-14:
1 | let rec baz ?x = baz
                 ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

val baz : ?x:'b -> 'a as 'a = <fun>
|}, Principal{|
Line 1, characters 13-14:
1 | let rec baz ?x = baz
                 ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

val baz : ?x:'a -> (?x:'a -> 'b as 'b) = <fun>
|}]

let rec baz (type a) ?x = baz
[%%expect{|
Line 1, characters 22-23:
1 | let rec baz (type a) ?x = baz
                          ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

val baz : ?x:'b -> 'a as 'a = <fun>
|}]

(* Test that warnings can preceed type errors, this tests the 'eager' path
   of the implementation for warning 16 in typecore.ml *)
let _ =
  let warn_me ?arg = () in
  warn_me + 0
[%%expect{|
Line 2, characters 15-18:
2 |   let warn_me ?arg = () in
                   ^^^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

Line 3, characters 2-9:
3 |   warn_me + 0
      ^^^^^^^
Error: The value "warn_me" has type "?arg:'a -> unit"
       but an expression was expected of type "int"
|}]

(* https://github.com/ocaml/ocaml/issues/14622 *)
module type Show = sig
  type t

  val show : t -> string
end

type 'a t =
  | A :
    { x : string option
      ; show : 'a -> string
      }
    -> 'a t

let test (type a) ?x (module M : Show with type t = a) =
  A { x; show = M.show }
[%%expect{|
module type Show = sig type t val show : t -> string end
type 'a t = A : { x : string option; show : 'a -> string; } -> 'a t
val test : ?x:string -> (module M : Show with type t = 'a) -> M.t t = <fun>
|}]
