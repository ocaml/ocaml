(* TEST
   * expect
*)

(* The goal of this test is exhaustively cover all spellcheck-style
   error messages in the compiler. *)

(* {1 toplevel/common.ml} *)

#directery "foo/";;
[%%expect {|
Unknown directive `directery'.
Hint: Did you mean directory?
|}];;


(* {1 typing/env.ml} *)

let _ = Fun.pratect
[%%expect {|
Line 1, characters 8-19:
1 | let _ = Fun.pratect
            ^^^^^^^^^^^
Error: Unbound value Fun.pratect
Hint: Did you mean protect?
|}];;

type 'a t = 'a aray
[%%expect {|
Line 1, characters 15-19:
1 | type 'a t = 'a aray
                   ^^^^
Error: Unbound type constructor aray
Hint: Did you mean array?
|}];;

module _ = Stdlib.Aray
[%%expect {|
Line 1, characters 11-22:
1 | module _ = Stdlib.Aray
               ^^^^^^^^^^^
Error: Unbound module Stdlib.Aray
Hint: Did you mean Array?
|}];;

let x = Same 42
[%%expect {|
Line 1, characters 8-12:
1 | let x = Same 42
            ^^^^
Error: Unbound constructor Same
Hint: Did you mean Some?
|}];;

let x : int option = Same 42
[%%expect {|
Line 1, characters 21-25:
1 | let x : int option = Same 42
                         ^^^^
Error: This variant expression is expected to have type int option
       There is no constructor Same within type option
Hint: Did you mean Some?
|}];;

let x = { content = 42 }
[%%expect {|
Line 1, characters 10-17:
1 | let x = { content = 42 }
              ^^^^^^^
Error: Unbound record field content
Hint: Did you mean contents?
|}];;

let x : int ref = { content = 42 }
[%%expect {|
Line 1, characters 20-27:
1 | let x : int ref = { content = 42 }
                        ^^^^^^^
Error: This record expression is expected to have type int ref
       There is no field content within type ref
Hint: Did you mean contents?
|}];;

class foobar = object end
let _ = object inherit foobaz end
[%%expect {|
class foobar : object  end
Line 2, characters 23-29:
2 | let _ = object inherit foobaz end
                           ^^^^^^
Error: Unbound class foobaz
Hint: Did you mean foobar?
|}];;

module type Foobar = sig end
module Foo : Foobaz = struct end
[%%expect {|
module type Foobar = sig end
Line 2, characters 13-19:
2 | module Foo : Foobaz = struct end
                 ^^^^^^
Error: Unbound module type Foobaz
Hint: Did you mean Foobar?
|}];;

class type foobar = object end
let _ : #foobaz = object end
[%%expect {|
class type foobar = object  end
Line 2, characters 9-15:
2 | let _ : #foobaz = object end
             ^^^^^^
Error: Unbound class type foobaz
Hint: Did you mean foobar?
|}];;

let _ =
  let foobaz = 42 in
  object
    val mutable foobar = foobaz
    method update n = foobaz <- n
  end
[%%expect {|
Line 5, characters 22-33:
5 |     method update n = foobaz <- n
                          ^^^^^^^^^^^
Error: The value foobaz is not an instance variable
Hint: Did you mean foobar?
|}];;


(* {1 typing/typecore.ml} *)

let _ = function (foobar | foobaz) -> ()
[%%expect {|
Line 1, characters 17-34:
1 | let _ = function (foobar | foobaz) -> ()
                     ^^^^^^^^^^^^^^^^^
Error: Variable foobar must occur on both sides of this | pattern
Hint: Did you mean foobaz?
|}];;

type foo = { foobar : int }
let _ = fun {foobaz} -> ()
[%%expect {|
type foo = { foobar : int; }
Line 2, characters 13-19:
2 | let _ = fun {foobaz} -> ()
                 ^^^^^^
Error: Unbound record field foobaz
Hint: Did you mean foobar?
|}];;
let _ = { foobaz = 42 }
[%%expect {|
Line 1, characters 10-16:
1 | let _ = { foobaz = 42 }
              ^^^^^^
Error: Unbound record field foobaz
Hint: Did you mean foobar?
|}];;
let _ = fun x -> x.foobaz
[%%expect {|
Line 1, characters 19-25:
1 | let _ = fun x -> x.foobaz
                       ^^^^^^
Error: Unbound record field foobaz
Hint: Did you mean foobar?
|}];;

type bar = Foobar of int
let _ = Foobaz 42
[%%expect {|
type bar = Foobar of int
Line 2, characters 8-14:
2 | let _ = Foobaz 42
            ^^^^^^
Error: Unbound constructor Foobaz
Hint: Did you mean Foobar?
|}];;

type baz = K of { foobar : int }
let _ = K { foobaz = 42 }
[%%expect {|
type baz = K of { foobar : int; }
Line 2, characters 12-18:
2 | let _ = K { foobaz = 42 }
                ^^^^^^
Error: The field foobaz is not part of the record argument for the baz.K constructor
Hint: Did you mean foobar?
|}];;

let _ = object (self)
  method foobar = 42
  method other = self#foobaz
end
[%%expect {|
Line 3, characters 17-21:
3 |   method other = self#foobaz
                     ^^^^
Error: This expression has no method foobaz
Hint: Did you mean foobar?
|}];;


let _ = object
  val foobar = 42
  method myself = {< foobaz = 42 >}
end
[%%expect {|
Line 3, characters 18-35:
3 |   method myself = {< foobaz = 42 >}
                      ^^^^^^^^^^^^^^^^^
Error: Unbound instance variable foobaz
Hint: Did you mean foobar?
|}];;
