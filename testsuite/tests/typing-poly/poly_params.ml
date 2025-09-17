(* TEST
 expect;
*)

let poly1 (id : 'a. 'a -> 'a) = id 3, id "three"
[%%expect {|
val poly1 : ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly1 (fun x -> x)
[%%expect {|
- : int * string = (3, "three")
|}];;

let _ = poly1 (fun x -> x + 1)
[%%expect {|
Line 1, characters 14-30:
1 | let _ = poly1 (fun x -> x + 1)
                  ^^^^^^^^^^^^^^^^
Error: This argument has type "int -> int" which is less general than
         "'a. 'a -> 'a"
       The type "int" is not a type variable.
|}];;

let id x = x
let _ = poly1 id
[%%expect {|
val id : 'a -> 'a = <fun>
- : int * string = (3, "three")
|}];;

let _ = poly1 (id (fun x -> x))
[%%expect {|
Line 1, characters 14-31:
1 | let _ = poly1 (id (fun x -> x))
                  ^^^^^^^^^^^^^^^^^
Error: This argument has type "'a -> 'a" which is less general than
         "'a0. 'a0 -> 'a0"
       The type variable "'a" is not generalizable to an universal
       type variable.
|}];;

let _ = poly1 (let r = ref None in fun x -> r := Some x; x)
[%%expect {|
Line 1, characters 14-59:
1 | let _ = poly1 (let r = ref None in fun x -> r := Some x; x)
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This argument has type "'a -> 'a" which is less general than
         "'a0. 'a0 -> 'a0"
       The type variable "'a" is not generalizable to an universal
       type variable.
|}];;

let escape f = poly1 (fun x -> f x; x)
[%%expect {|
Line 1, characters 21-38:
1 | let escape f = poly1 (fun x -> f x; x)
                         ^^^^^^^^^^^^^^^^^
Error: This argument has type "'a -> 'a" which is less general than
         "'a0. 'a0 -> 'a0"
       The type variable "'a" is not generalizable to an universal
       type variable.
|}];;

let poly2 : ('a. 'a -> 'a) -> int * string =
  fun id -> id 3, id "three"
[%%expect {|
val poly2 : ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly2 (fun x -> x)
[%%expect {|
- : int * string = (3, "three")
|}];;

let _ = poly2 (fun x -> x + 1)
[%%expect {|
Line 1, characters 14-30:
1 | let _ = poly2 (fun x -> x + 1)
                  ^^^^^^^^^^^^^^^^
Error: This argument has type "int -> int" which is less general than
         "'a. 'a -> 'a"
       The type "int" is not a type variable.
|}];;

let poly3 : 'b. ('a. 'a -> 'a) -> 'b -> 'b * 'b option =
  fun id x -> id x, id (Some x)
[%%expect {|
val poly3 : ('a. 'a -> 'a) -> 'b -> 'b * 'b option = <fun>
|}];;

let _ = poly3 (fun x -> x) 8
[%%expect {|
- : int * int option = (8, Some 8)
|}];;

let _ = poly3 (fun x -> x + 1) 8
[%%expect {|
Line 1, characters 14-30:
1 | let _ = poly3 (fun x -> x + 1) 8
                  ^^^^^^^^^^^^^^^^
Error: This argument has type "int -> int" which is less general than
         "'a. 'a -> 'a"
       The type "int" is not a type variable.
|}];;

let rec poly4 p (id : 'a. 'a -> 'a) =
  if p then poly4 false id else id 4, id "four"
[%%expect {|
val poly4 : bool -> ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly4 true (fun x -> x)
[%%expect {|
- : int * string = (4, "four")
|}];;

let _ = poly4 true (fun x -> x + 1)
[%%expect {|
Line 1, characters 19-35:
1 | let _ = poly4 true (fun x -> x + 1)
                       ^^^^^^^^^^^^^^^^
Error: This argument has type "int -> int" which is less general than
         "'a. 'a -> 'a"
       The type "int" is not a type variable.
|}];;

let rec poly5 : bool -> ('a. 'a -> 'a) -> int * string =
  fun p id ->
    if p then poly5 false id else id 5, id "five"
[%%expect {|
val poly5 : bool -> ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly5 true (fun x -> x)
[%%expect {|
- : int * string = (5, "five")
|}];;

let _ = poly5 true (fun x -> x + 1)
[%%expect {|
Line 1, characters 19-35:
1 | let _ = poly5 true (fun x -> x + 1)
                       ^^^^^^^^^^^^^^^^
Error: This argument has type "int -> int" which is less general than
         "'a. 'a -> 'a"
       The type "int" is not a type variable.
|}];;


let rec poly6 : 'b. bool -> ('a. 'a -> 'a) -> 'b -> 'b * 'b option =
  fun p id x ->
    if p then poly6 false id x else id x, id (Some x)
[%%expect {|
val poly6 : bool -> ('a. 'a -> 'a) -> 'b -> 'b * 'b option = <fun>
|}];;

let _ = poly6 true (fun x -> x) 8
[%%expect {|
- : int * int option = (8, Some 8)
|}];;

let _ = poly6 true (fun x -> x + 1) 8
[%%expect {|
Line 1, characters 19-35:
1 | let _ = poly6 true (fun x -> x + 1) 8
                       ^^^^^^^^^^^^^^^^
Error: This argument has type "int -> int" which is less general than
         "'a. 'a -> 'a"
       The type "int" is not a type variable.
|}];;

let needs_magic (magic : 'a 'b. 'a -> 'b) = (magic 5 : string)
let _ = needs_magic (fun x -> x)
[%%expect {|
val needs_magic : ('a 'b. 'a -> 'b) -> string = <fun>
Line 2, characters 20-32:
2 | let _ = needs_magic (fun x -> x)
                        ^^^^^^^^^^^^
Error: This argument has type "'b. 'b -> 'b" which is less general than
         "'a 'b. 'a -> 'b"
       The universal type variable "'b" in the first type matches multiple
       distinct variables in the second type.
|}];;

let with_id (f : ('a. 'a -> 'a) -> 'b) = f (fun x -> x)
[%%expect {|
val with_id : (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;

let _ = with_id (fun id -> id 4, id "four")
[%%expect {|
- : int * string = (4, "four")
|}];;

let non_principal1 p f =
  if p then with_id f
  else f (fun x -> x)
[%%expect {|
val non_principal1 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}, Principal{|
Line 3, characters 7-21:
3 |   else f (fun x -> x)
           ^^^^^^^^^^^^^^
Warning 18 [not-principal]: applying a higher-rank function here is not
  principal.

val non_principal1 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;

let non_principal2 p f =
  if p then f (fun x -> x)
  else with_id f
[%%expect {|
Line 3, characters 15-16:
3 |   else with_id f
                   ^
Error: The value "f" has type "('b -> 'b) -> 'c"
       but an expression was expected of type "('a. 'a -> 'a) -> 'd"
       The universal variable "'a" would escape its scope
|}];;

let principal1 p (f : ('a. 'a -> 'a) -> 'b) =
  if p then f (fun x -> x)
  else with_id f
[%%expect {|
val principal1 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;

let principal2 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b =
  fun p f ->
    if p then f (fun x -> x)
    else with_id f
[%%expect {|
val principal2 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;

type poly = ('a. 'a -> 'a) -> int * string

let principal3 : poly option list = [ None; Some (fun x -> x 5, x "hello") ]
[%%expect {|
type poly = ('a. 'a -> 'a) -> int * string
val principal3 : poly option list = [None; Some <fun>]
|}];;

let non_principal3 =
  [ (Some (fun x -> x 5, x "hello") : poly option);
    Some (fun y -> y 6, y "goodbye") ]
[%%expect {|
val non_principal3 : poly option list = [Some <fun>; Some <fun>]
|}, Principal{|
Line 3, characters 9-36:
3 |     Some (fun y -> y 6, y "goodbye") ]
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this higher-rank function is not principal.

val non_principal3 : poly option list = [Some <fun>; Some <fun>]
|}];;

let non_principal4 =
  [ Some (fun y -> y 6, y "goodbye");
    (Some (fun x -> x 5, x "hello") : poly option) ]
[%%expect {|
Line 2, characters 26-35:
2 |   [ Some (fun y -> y 6, y "goodbye");
                              ^^^^^^^^^
Error: This constant has type "string" but an expression was expected of type
         "int"
|}];;

(* Functions with polymorphic parameters are separate from other functions *)
type 'a arg = 'b
  constraint 'a = 'b -> 'c
type really_poly = (('a. 'a -> 'a) -> string) arg
[%%expect {|
type 'a arg = 'b constraint 'a = 'b -> 'c
Line 3, characters 20-44:
3 | type really_poly = (('a. 'a -> 'a) -> string) arg
                        ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type "('a. 'a -> 'a) -> string" should be an instance of type
         "'b -> 'c"
       The universal variable "'a" would escape its scope
|}];;

(* Polymorphic parameters are (mostly) treated as invariant *)
type p1 = ('a. 'a -> 'a) -> int
type p2 = ('a 'b. 'a -> 'b) -> int
[%%expect {|
type p1 = ('a. 'a -> 'a) -> int
type p2 = ('a 'b. 'a -> 'b) -> int
|}];;

let foo (f : p1) : p2 = f
[%%expect {|
Line 1, characters 24-25:
1 | let foo (f : p1) : p2 = f
                            ^
Error: The value "f" has type "p1" = "('a. 'a -> 'a) -> int"
       but an expression was expected of type "p2" = "('a 'b. 'a -> 'b) -> int"
       The universal variables "'a" and "'b" are distinct.
|}];;

let foo f = (f : p1 :> p2)
[%%expect {|
Line 1, characters 12-26:
1 | let foo f = (f : p1 :> p2)
                ^^^^^^^^^^^^^^
Error: Type "p1" = "('a. 'a -> 'a) -> int" is not a subtype of
         "p2" = "('a 'b. 'a -> 'b) -> int"
       The universal variables "'b" and "'a" are distinct.
|}];;

module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
[%%expect {|
Line 1, characters 59-60:
1 | module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
                                                               ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : p1 end
       is not included in
         sig val f : p2 end
       Values do not match: val f : p1 is not included in val f : p2
       The type "p1" = "('a. 'a -> 'a) -> int" is not compatible with the type
         "p2" = "('a 'b. 'a -> 'b) -> int"
       The universal variables "'a" and "'b" are distinct.
|}];;

let foo (f : p1) : p2 = (fun id -> f id)
[%%expect {|
val foo : p1 -> p2 = <fun>
|}];;

(* Following the existing behaviour for polymorphic methods, you can
   subtype from a polymorphic parameter to a monomorphic
   parameter. Elsewhere it still behaves as invariant. *)
type p1 = (bool -> bool) -> int
type p2 = ('a. 'a -> 'a) -> int

let foo (x : p1) : p2 = x
[%%expect {|
type p1 = (bool -> bool) -> int
type p2 = ('a. 'a -> 'a) -> int
Line 4, characters 24-25:
4 | let foo (x : p1) : p2 = x
                            ^
Error: The value "x" has type "p1" = "(bool -> bool) -> int"
       but an expression was expected of type "p2" = "('a. 'a -> 'a) -> int"
       Type "bool" is not compatible with type "'a"
|}];;

let foo x = (x : p1 :> p2)
[%%expect {|
val foo : p1 -> p2 = <fun>
|}];;

module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
[%%expect {|
Line 1, characters 59-60:
1 | module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
                                                               ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : p1 end
       is not included in
         sig val f : p2 end
       Values do not match: val f : p1 is not included in val f : p2
       The type "p1" = "(bool -> bool) -> int" is not compatible with the type
         "p2" = "('a. 'a -> 'a) -> int"
       Type "bool" is not compatible with type "'a"
|}];;

let foo (f : p1) : p2 = (fun id -> f id)
[%%expect {|
val foo : p1 -> p2 = <fun>
|}];;

class c (f: 'a. 'a -> 'a) = object
  method m = f 0
  method n = f "a"
end;;
[%%expect {|
Line 1, characters 9-24:
1 | class c (f: 'a. 'a -> 'a) = object
             ^^^^^^^^^^^^^^^
Error: Class parameters cannot be polymorphic.
|}];;

class c' (f: 'a. int -> int) = object
  method m = f 0
end;;
[%%expect {|
Line 1, characters 10-27:
1 | class c' (f: 'a. int -> int) = object
              ^^^^^^^^^^^^^^^^^
Error: Class parameters cannot be polymorphic.
|}];;

let poly1' ~(id : 'a. 'a -> 'a) = id 3, id "three"
[%%expect {|
val poly1' : id:('a. 'a -> 'a) -> int * string = <fun>
|}];;

let poly2' ?(id : 'a. 'a -> 'a) = id 3, id "three"
[%%expect {|
Line 1, characters 13-30:
1 | let poly2' ?(id : 'a. 'a -> 'a) = id 3, id "three"
                 ^^^^^^^^^^^^^^^^^
Error: The optional parameter "id" cannot have a polymorphic type.
|}];;

let poly3' ?(id : 'a. int -> int) = id 3
[%%expect {|
Line 1, characters 13-32:
1 | let poly3' ?(id : 'a. int -> int) = id 3
                 ^^^^^^^^^^^^^^^^^^^
Error: The optional parameter "id" cannot have a polymorphic type.
|}];;

(* This test illustrate a new occurrence of the bug discussed in
   https://github.com/ocaml/ocaml/pull/13984*)

module type T = sig type 'a t = 'a list  end

let rec f (x : (module T)) =
  let (module LocalModule) = x in (assert false : ('a. 'a LocalModule.t) -> unit)

[%%expect{|
module type T = sig type 'a t = 'a list end
Line 4, characters 58-69:
4 |   let (module LocalModule) = x in (assert false : ('a. 'a LocalModule.t) -> unit)
                                                              ^^^^^^^^^^^
Error: Unbound module "LocalModule"
|}]

(* The following test requires full translation in the [approx_type] function if
   the annotation is partial. *)
let rec f () = g () Fun.id
and g () : ('a. 'a -> 'a) -> unit = fun _ -> () ;;

[%%expect{|
val f : unit -> unit = <fun>
val g : unit -> ('a. 'a -> 'a) -> unit = <fun>
|}]

let rec f () = g () Fun.id
and g : unit -> ('a. 'a -> 'a) -> unit = fun () _ -> () ;;

[%%expect{|
val f : unit -> unit = <fun>
val g : unit -> ('a. 'a -> 'a) -> unit = <fun>
|}]


(* Attempts at breaking type_pattern_approx *)
let rec f ([] : 'a. 'a list) = ()

[%%expect{|
val f : ('a. 'a list) -> unit = <fun>
|}]

let rec f () : ('a. 'a list) -> unit = fun [] -> ()

[%%expect{|
Line 1, characters 43-45:
1 | let rec f () : ('a. 'a list) -> unit = fun [] -> ()
                                               ^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "_::_"

val f : unit -> ('a. 'a list) -> unit = <fun>
|}]


(* New expert trick: use 'a. to trigger "exact approximation" *)
let rec f () = g (module Map.Make(Int)) and g (m : (module Map.S)) = ();;

[%%expect{|
Line 1, characters 17-39:
1 | let rec f () = g (module Map.Make(Int)) and g (m : (module Map.S)) = ();;
                     ^^^^^^^^^^^^^^^^^^^^^^
Error: The signature for this packaged module couldn't be inferred.
|}]

let rec f () = g (module Map.Make(Int)) and g (m : 'a. (module Map.S)) = ();;

[%%expect{|
val f : unit -> unit = <fun>
val g : (module Map.S) -> unit = <fun>
|}]

(* Check that we are getting the right behaviour for polymorphic variants
   in polymorphic parameters. *)

let poly_poly_var : [< `A | `B ] -> unit = function  | `A -> () | `B -> ()

let accept_poly_poly_var (g : 'a. ([< `A | `B ] as 'a) -> unit) = g `A

let () = accept_poly_poly_var poly_poly_var
[%%expect {|
val poly_poly_var : [< `A | `B ] -> unit = <fun>
val accept_poly_poly_var : ('a. ([< `A | `B ] as 'a) -> unit) -> unit = <fun>
|}]

let f (`B|_) = ()
let h (f:'a. ([> `A ] as 'a) -> unit ) = f `B
let error = h f
[%%expect {|
val f : [> `B ] -> unit = <fun>
val h : ('a. ([> `A ] as 'a) -> unit) -> unit = <fun>
Line 3, characters 14-15:
3 | let error = h f
                  ^
Error: The value "f" has type "[> `B ] -> unit"
       but an expression was expected of type "[> `A ] -> unit"
       The second variant type is bound to the universal type variable "'a",
       it may not allow the tag(s) "`B"
|}]

let (let*) x (id : 'a. 'a -> 'a) = id x, id 1
[%%expect {|
val ( let* ) : 'b -> ('a. 'a -> 'a) -> 'b * int = <fun>
|}]

let (let*) (x : 'a. 'a option) (id : 'a. 'a -> 'a) = id x, id 1
[%%expect {|
val ( let* ) : ('a. 'a option) -> ('a. 'a -> 'a) -> 'b option * int = <fun>
|}]

let y =
  let* x = 3. in
  x
[%%expect {|
Line 2, characters 2-6:
2 |   let* x = 3. in
      ^^^^
Error: The operator "let*" has type
         "('a. 'a option) -> ('a. 'a -> 'a) -> 'b option * int"
       but it was expected to have type "'c -> ('d -> 'e) -> 'f"
       The universal variable "'a" would escape its scope
|}]

let f ((g, x) : 'a. ('a -> int) * 'a) =
  g 3, g "three"

[%%expect{|
val f : ('a. ('a -> int) * 'a) -> int * int = <fun>
|}]


let f (x: [< `A of ('a. 'a option) -> unit ]) = match x with `A f -> f None
[%%expect{|
val f : [< `A of ('a. 'a option) -> unit & 'b option -> 'c ] -> 'c = <fun>
|}]

let f: type a. unit -> (a, ('b. 'b -> 'b) -> int) Type.eq ->  a =
  fun () Equal f -> f 0
[%%expect{|
Line 2, characters 2-23:
2 |   fun () Equal f -> f 0
      ^^^^^^^^^^^^^^^^^^^^^
Error: The syntactic arity of the function doesn't match the type constraint:
       This function has 3 syntactic arguments, but its type is constrained to
         "unit -> (a, ('b. 'b -> 'b) -> int) Type.eq -> a".
        Hint: consider splitting the function definition into
          "fun ... gadt_pat -> fun ..."
          where "gadt_pat" is the pattern with the GADT constructor that
          introduces the local type equation on "a".
|}]
