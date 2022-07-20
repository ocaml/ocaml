(* TEST
   * expect
*)

(* Subtyping is "syntactic" *)
fun (x : < x : int >) y z -> (y :> 'a), (x :> 'a), (z :> 'a);;
[%%expect{|
- : < x : int > ->
    < x : int > -> < x : int > -> < x : int > * < x : int > * < x : int >
= <fun>
|}];;
(* - : (< x : int > as 'a) -> 'a -> 'a * 'a = <fun> *)

(* Quirks of class typing.  *)
class ['a] c () = object
  method f = (new c (): int c)
end and ['a] d () = object
  inherit ['a] c ()
end;;
[%%expect{|
class ['a] c : unit -> object constraint 'a = int method f : int c end
and ['a] d : unit -> object constraint 'a = int method f : 'a c end
|}];;
(* class ['a] c : unit -> object constraint 'a = int method f : 'a c end *)
(* and ['a] d : unit -> object constraint 'a = int method f : 'a c end *)

(* 'a free in class d *)
class ['a] c () = object
  method f (x : 'a) = ()
end and d () = object
  inherit ['a] c ()
end;;
[%%expect{|
Lines 3-5, characters 4-3:
3 | ....and d () = object
4 |   inherit ['a] c ()
5 | end..
Error: Some type variables are unbound in this type:
         class d : unit -> object method f : 'a -> unit end
       The method f has type 'a -> unit where 'a is unbound
|}];;

(* Create instance #c *)
class virtual c () = object
end and ['a] d () = object
  constraint 'a = #c
  method f (x : #c) = (x#x : int)
end;;
[%%expect{|
class virtual c : unit -> object  end
and ['a] d :
  unit -> object constraint 'a = < x : int; .. > method f : 'a -> int end
|}];;
(* class virtual c : unit -> object  end *)
(* and ['a] d : *)
(*  unit -> object constraint 'a = < x : int; .. > method f : 'a -> int end *)

class ['a] c () = object
  constraint 'a = int
end and ['a] d () = object
  constraint 'a = 'b #c
end;;
[%%expect{|
class ['a] c : unit -> object constraint 'a = int end
and ['a] d : unit -> object constraint 'a = int #c end
|}];;
(* class ['a] c : unit -> object constraint 'a = int end
   and ['a] d : unit -> object constraint 'a = int #c end *)
(* Class type constraint *)
module F(X:sig type t end) = struct
  class type ['a] c = object
    method m: 'a -> X.t
  end
end
class ['a] c = object
  constraint 'a = 'a #F(Int).c
end
[%%expect {|
module F :
  functor (X : sig type t end) ->
    sig class type ['a] c = object method m : 'a -> X.t end end
class ['a] c : object constraint 'a = < m : 'a -> Int.t; .. > end
|}]

(* Self as parameter *)
class ['a] c (x : 'a) = object (self : 'b)
  constraint 'a = 'b
  method f = self
end;;
[%%expect{|
class ['a] c :
  'a -> object ('a) constraint 'a = < f : 'a; .. > method f : 'a end
|}];;
new c;;
[%%expect{|
- : ('a c as 'a) -> 'a = <fun>
|}];;
(* class ['a] c :
  'a -> object ('a) constraint 'a = < f : 'a; .. > method f : 'a end *)
(* - : ('a c as 'a) -> 'a = <fun> *)

class x () = object
  method virtual f : int
end;;
[%%expect{|
Lines 1-3, characters 13-3:
1 | .............object
2 |   method virtual f : int
3 | end..
Error: This non-virtual class has virtual methods.
       The following methods are virtual : f
|}];;
(* The class x should be virtual:  its methods f is undefined *)

(* Supplementary method g *)
class virtual c ((x : 'a): < f : int >) = object (_ : 'a) end
and virtual d x = object (_ : 'a)
  inherit c x
  method g = true
end;;
[%%expect{|
Line 1, characters 49-57:
1 | class virtual c ((x : 'a): < f : int >) = object (_ : 'a) end
                                                     ^^^^^^^^
Error: This pattern cannot match self: it only matches values of type
       < f : int >
|}];;

(* Constraint not respected *)
class ['a] c () = object
  constraint 'a = int
  method f x = (x : bool c)
end;;
[%%expect{|
Lines 1-4, characters 0-3:
1 | class ['a] c () = object
2 |   constraint 'a = int
3 |   method f x = (x : bool c)
4 | end..
Error: The abbreviation c is used with parameters bool c
       which are incompatible with constraints int c
|}];;

(* Different constraints *)
class ['a, 'b] c () = object
  constraint 'a = int -> 'c
  constraint 'b = 'a * <x : 'b> * 'c * 'd
  method f (x : 'a) (y : 'b) = ()
end;;
[%%expect{|
class ['a, 'b] c :
  unit ->
  object
    constraint 'a = int -> 'c
    constraint 'b = 'a * < x : 'b > * 'c * 'd
    method f : 'a -> 'b -> unit
  end
|}];;
class ['a, 'b] d () = object
  inherit ['a, 'b] c ()
end;;
[%%expect{|
class ['a, 'b] d :
  unit ->
  object
    constraint 'a = int -> 'd
    constraint 'b = 'a * (< x : 'b > as 'c) * 'd * 'e
    method f : (int -> 'd) -> (int -> 'd) * 'c * 'd * 'e -> unit
  end
|}];;

(* Non-generic constraint *)
let x = ref [];;
[%%expect{|
val x : '_weak1 list ref = {contents = []}
|}];;
class ['a] c () = object
  method f = (x : 'a)
end;;
[%%expect{|
Lines 1-3, characters 0-3:
1 | class ['a] c () = object
2 |   method f = (x : 'a)
3 | end..
Error: The type of this class,
       class ['a] c :
         unit -> object constraint 'a = '_weak1 list ref method f : 'a end,
       contains type variables that cannot be generalized
|}];;

(* Abbreviations *)
type 'a c = <f : 'a c; g : 'a d>
and 'a d = <f : int c>;;
[%%expect{|
Line 1, characters 0-32:
1 | type 'a c = <f : 'a c; g : 'a d>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This recursive type is not regular.
       The type constructor c is defined as
         type 'a c
       but it is used as
         int c
       after the following expansion(s):
         'a d = < f : int c >
       All uses need to match the definition for the recursive type to be regular.
|}];;
type 'a c = <f : 'a c; g : 'a d>
and 'a d = <f : 'a c>;;
[%%expect{|
type 'a c = < f : 'a c; g : 'a d >
and 'a d = < f : 'a c >
|}];;
type 'a c = <f : 'a c>
and 'a d = <f : int c>;;
[%%expect{|
type 'a c = < f : 'a c >
and 'a d = < f : int c >
|}];;
type 'a u = < x : 'a>
and 'a t = 'a t u;;
[%%expect{|
Line 2, characters 0-17:
2 | and 'a t = 'a t u;;
    ^^^^^^^^^^^^^^^^^
Error: The definition of t contains a cycle:
       'a t u
|}];; (* fails since 4.04 *)
type 'a u = 'a
and 'a t = 'a t u;;
[%%expect{|
Line 2, characters 0-17:
2 | and 'a t = 'a t u;;
    ^^^^^^^^^^^^^^^^^
Error: The type abbreviation t is cyclic
|}];;
type 'a u = 'a;;
[%%expect{|
type 'a u = 'a
|}];;
type t = t u * t u;;
[%%expect{|
Line 1, characters 0-18:
1 | type t = t u * t u;;
    ^^^^^^^^^^^^^^^^^^
Error: The type abbreviation t is cyclic
|}];;

type t = <x : 'a> as 'a;;
[%%expect{|
type t = < x : 'a > as 'a
|}];;
type 'a u = 'a;;
[%%expect{|
type 'a u = 'a
|}];;
fun (x : t) (y : 'a u) -> x = y;;
[%%expect{|
- : t -> t u -> bool = <fun>
|}];;
fun (x : t) (y : 'a u) -> y = x;;
[%%expect{|
- : t -> t u -> bool = <fun>
|}];;
(* - : t -> t u -> bool = <fun> *)

(* Modules *)
module M =
  struct
    class ['a, 'b] c (x: int) (y: 'b) = object
      constraint 'a = int -> bool
      val x : float list = []
      val y = y
      method f (x : 'a) = ()
      method g = y
    end
  end;;
[%%expect{|
module M :
  sig
    class ['a, 'b] c :
      int ->
      'b ->
      object
        constraint 'a = int -> bool
        val x : float list
        val y : 'b
        method f : 'a -> unit
        method g : 'b
      end
  end
|}];;
module M' = (M :
  sig
     class virtual ['a, 'b] c : int -> 'b -> object
       constraint 'a = int -> bool
       val x : float list
       val y : 'b
       method f : 'a -> unit
       method g : 'b
     end
   end);;
[%%expect{|
module M' :
  sig
    class virtual ['a, 'b] c :
      int ->
      'b ->
      object
        constraint 'a = int -> bool
        val x : float list
        val y : 'b
        method f : 'a -> unit
        method g : 'b
      end
  end
|}];;
class ['a, 'b] d () y = object inherit ['a, 'b] M.c 7 y end;;
[%%expect{|
class ['a, 'b] d :
  unit ->
  'b ->
  object
    constraint 'a = int -> bool
    val x : float list
    val y : 'b
    method f : (int -> bool) -> unit
    method g : 'b
  end
|}];;
class ['a, 'b] e () y = object inherit ['a, 'b] M'.c 1 y end;;
[%%expect{|
class ['a, 'b] e :
  unit ->
  'b ->
  object
    constraint 'a = int -> bool
    val x : float list
    val y : 'b
    method f : (int -> bool) -> unit
    method g : 'b
  end
|}];;
(new M.c 3 "a")#g;;
[%%expect{|
- : string = "a"
|}];;
(new d () 10)#g;;
[%%expect{|
- : int = 10
|}];;
(new e () 7.1)#g;;
[%%expect{|
- : float = 7.1
|}];;
open M;;
[%%expect{|
|}];;
(new c 5 true)#g;;
[%%expect{|
- : bool = true
|}];;

(* #cl when cl is closed *)
module M = struct class ['a] c () = object method f (x : 'a) = () end end;;
[%%expect{|
module M : sig class ['a] c : unit -> object method f : 'a -> unit end end
|}];;
module M' =
  (M : sig class ['a] c : unit -> object method f : 'a -> unit end end);;
[%%expect{|
module M' : sig class ['a] c : unit -> object method f : 'a -> unit end end
|}];;
fun x -> (x :> 'a #M.c);;
[%%expect{|
- : ('a #M.c as 'b) -> 'b = <fun>
|}];;
fun x -> (x :> 'a #M'.c);;
[%%expect{|
- : ('a #M'.c as 'b) -> 'b = <fun>
|}];;
class ['a] c (x : 'b #c) = object end;;
[%%expect{|
class ['a] c : 'a #c -> object  end
|}];;
class ['a] c (x : 'b #c) = object end;;
[%%expect{|
class ['a] c : 'a #c -> object  end
|}];;

(* Computation order *)
class c () = object method f = 1 end and d () = object method f = 2 end;;
[%%expect{|
class c : unit -> object method f : int end
and d : unit -> object method f : int end
|}];;
class e () = object inherit c () inherit d () end;;
[%%expect{|
class e : unit -> object method f : int end
|}];;
(new e ())#f;;
[%%expect{|
- : int = 2
|}];;
class c () = object val x = - true val y = -. () end;;
[%%expect{|
Line 1, characters 30-34:
1 | class c () = object val x = - true val y = -. () end;;
                                  ^^^^
Error: This expression has type bool but an expression was expected of type
         int
|}];;

class c () = object method f = 1 method g = 1 method h = 1 end;;
[%%expect{|
class c : unit -> object method f : int method g : int method h : int end
|}];;
class d () = object method h = 2 method i = 2 method j = 2 end;;
[%%expect{|
class d : unit -> object method h : int method i : int method j : int end
|}];;
class e () = object
  method f = 3
  inherit c ()
  method g = 3
  method i = 3
  inherit d ()
  method j = 3
end;;
[%%expect{|
class e :
  unit ->
  object
    method f : int
    method g : int
    method h : int
    method i : int
    method j : int
  end
|}];;
let e = new e ();;
[%%expect{|
val e : e = <obj>
|}];;
e#f, e#g, e#h, e#i, e#j;;
[%%expect{|
- : int * int * int * int * int = (1, 3, 2, 2, 3)
|}];;

class c a = object val x = 1 val y = 1 val z = 1 val a = a end;;
[%%expect{|
class c : 'a -> object val a : 'a val x : int val y : int val z : int end
|}];;
class d b = object val z = 2 val t = 2 val u = 2 val b = b end;;
[%%expect{|
class d : 'a -> object val b : 'a val t : int val u : int val z : int end
|}];;
class e () = object
  val x = 3
  inherit c 5
  val y = 3
  val t = 3
  inherit d 7
  val u = 3
  method x = x
  method y = y
  method z = z
  method t = t
  method u = u
  method a = a
  method b = b
end;;
[%%expect{|
Line 3, characters 2-13:
3 |   inherit c 5
      ^^^^^^^^^^^
Warning 13 [instance-variable-override]: the following instance variables are overridden by the class c :
  x
Line 4, characters 6-7:
4 |   val y = 3
          ^
Warning 13 [instance-variable-override]: the instance variable y is overridden.
Line 6, characters 2-13:
6 |   inherit d 7
      ^^^^^^^^^^^
Warning 13 [instance-variable-override]: the following instance variables are overridden by the class d :
  t z
Line 7, characters 6-7:
7 |   val u = 3
          ^
Warning 13 [instance-variable-override]: the instance variable u is overridden.
class e :
  unit ->
  object
    val a : int
    val b : int
    val t : int
    val u : int
    val x : int
    val y : int
    val z : int
    method a : int
    method b : int
    method t : int
    method u : int
    method x : int
    method y : int
    method z : int
  end
|}];;
let e = new e ();;
[%%expect{|
val e : e = <obj>
|}];;
e#x, e#y, e#z, e#t, e#u, e#a, e#b;;
[%%expect{|
- : int * int * int * int * int * int * int = (1, 3, 2, 2, 3, 5, 7)
|}];;

class c (x : int) (y : int) = object
  val x = x
  val y = y
  method x = x
  method y = y
end;;
[%%expect{|
class c :
  int ->
  int -> object val x : int val y : int method x : int method y : int end
|}];;
class d x y = object inherit c x y end;;
[%%expect{|
class d :
  int ->
  int -> object val x : int val y : int method x : int method y : int end
|}];;
let c = new c 1 2 in c#x, c#y;;
[%%expect{|
- : int * int = (1, 2)
|}];;
let d = new d 1 2 in d#x, d#y;;
[%%expect{|
- : int * int = (1, 2)
|}];;

(* Parameters which does not appear in the object type *)
class ['a] c (x : 'a) = object end;;
[%%expect{|
class ['a] c : 'a -> object  end
|}];;
new c;;
[%%expect{|
- : 'a -> 'a c = <fun>
|}];;

(* Private variables *)
(*
module type M = sig
  class c : unit -> object val x : int end
  class d : unit -> object inherit c val private x : int val x : bool end
end;;
[%%expect{|
foo
|}];;
class c (x : int) =
  val private mutable x = x
  method get = x
  method set y = x <- y
end;;
[%%expect{|
foo
|}];;
let c = new c 5;;
[%%expect{|
foo
|}];;
c#get;;
[%%expect{|
foo
|}];;
c#set 7; c#get;;
[%%expect{|
foo
|}];;


class c () = val x = 1 val y = 1 method c = x end;;
[%%expect{|
foo
|}];;
class d () = inherit c () val private x method d = x end;;
[%%expect{|
foo
|}];;
class e () =
  val x = 2 val y = 2 inherit d () method x = x method y = y
end;;
[%%expect{|
foo
|}];;
let e = new e () in e#x, e#y, e#c, e#d;;
[%%expect{|
foo
|}];;
*)

(* Forgotten variables in interfaces *)
module M :
  sig
    class c : unit -> object
      method xc : int
    end
  end =
  struct
    class c () = object
      val x = 1
      method xc = x
    end
  end;;
[%%expect{|
module M : sig class c : unit -> object method xc : int end end
|}];;
class d () = object
  val x = 2
  method xd = x
  inherit M.c ()
end;;
[%%expect{|
class d : unit -> object val x : int method xc : int method xd : int end
|}];;
let d = new d () in d#xc, d#xd;;
[%%expect{|
- : int * int = (1, 2)
|}];;

class virtual ['a] matrix (sz, init : int * 'a) = object
  val m = Array.make_matrix sz sz init
  method add (mtx : 'a matrix) = (mtx#m.(0).(0) : 'a)
end;;
[%%expect{|
Lines 1-4, characters 0-3:
1 | class virtual ['a] matrix (sz, init : int * 'a) = object
2 |   val m = Array.make_matrix sz sz init
3 |   method add (mtx : 'a matrix) = (mtx#m.(0).(0) : 'a)
4 | end..
Error: The abbreviation 'a matrix expands to type < add : 'a matrix -> 'a >
       but is used with type < m : 'a array array; .. >
|}];;

class c () = object method m = new c () end;;
[%%expect{|
class c : unit -> object method m : c end
|}];;
(new c ())#m;;
[%%expect{|
- : c = <obj>
|}];;
module M = struct class c () = object method m = new c () end end;;
[%%expect{|
module M : sig class c : unit -> object method m : c end end
|}];;
(new M.c ())#m;;
[%%expect{|
- : M.c = <obj>
|}];;

type uu = A of int | B of (<leq: 'a> as 'a);;
[%%expect{|
type uu = A of int | B of (< leq : 'a > as 'a)
|}];;

class virtual c () = object (_ : 'a) method virtual m : 'a end;;
[%%expect{|
class virtual c : unit -> object ('a) method virtual m : 'a end
|}];;
module S = (struct
  let f (x : #c) = x
end : sig
  val f : (#c as 'a) -> 'a
end);;
[%%expect{|
module S : sig val f : (#c as 'a) -> 'a end
|}];;
module S = (struct
  let f (x : #c) = x
end : sig
  val f : #c -> #c
end);;
[%%expect{|
Lines 1-3, characters 12-3:
1 | ............struct
2 |   let f (x : #c) = x
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig val f : (#c as 'a) -> 'a end
       is not included in
         sig val f : #c -> #c end
       Values do not match:
         val f : (#c as 'a) -> 'a
       is not included in
         val f : #c -> #c
       The type (#c as 'a) -> 'a is not compatible with the type #c -> #c
       Type #c as 'a = < m : 'a; .. > is not compatible with type
         #c as 'b = < m : 'b; .. >
       Type 'a is not compatible with type 'b
|}];;

module M = struct type t = int class t () = object end end;;
[%%expect{|
Line 1, characters 37-38:
1 | module M = struct type t = int class t () = object end end;;
                                         ^
Error: Multiple definition of the type name t.
       Names must be unique in a given structure or signature.
|}];;

fun x -> (x :> < m : 'a -> 'a > as 'a);;
[%%expect{|
- : < m : (< m : 'a > as 'b) -> 'b as 'a; .. > -> 'b = <fun>
|}];;

fun x -> (x : int -> bool :> 'a -> 'a);;
[%%expect{|
Line 1, characters 9-38:
1 | fun x -> (x : int -> bool :> 'a -> 'a);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type int -> bool is not a subtype of int -> int
       Type bool is not a subtype of int
|}];;
fun x -> (x : int -> bool :> int -> int);;
[%%expect{|
Line 1, characters 9-40:
1 | fun x -> (x : int -> bool :> int -> int);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type int -> bool is not a subtype of int -> int
       Type bool is not a subtype of int
|}];;
fun x -> (x : < > :> < .. >);;
[%%expect{|
- : <  > -> <  > = <fun>
|}];;
fun x -> (x : < .. > :> < >);;
[%%expect{|
- : < .. > -> <  > = <fun>
|}];;

let x = ref [];;
[%%expect{|
val x : '_weak2 list ref = {contents = []}
|}];;
module F(X : sig end) =
  struct type t = int let _ = (x : < m : t> list ref) end;;
[%%expect{|
module F : functor (X : sig end) -> sig type t = int end
|}];;
x;;
[%%expect{|
- : < m : int > list ref = {contents = []}
|}];;

type 'a t;;
[%%expect{|
type 'a t
|}];;
fun (x : 'a t as 'a) -> ();;
[%%expect{|
Line 1, characters 9-19:
1 | fun (x : 'a t as 'a) -> ();;
             ^^^^^^^^^^
Error: This alias is bound to type 'a t but is used as an instance of type 'a
       The type variable 'a occurs inside 'a t
|}];;
fun (x : 'a t) -> (x : 'a); ();;
[%%expect{|
Line 1, characters 19-20:
1 | fun (x : 'a t) -> (x : 'a); ();;
                       ^
Error: This expression has type 'a t but an expression was expected of type
         'a
       The type variable 'a occurs inside 'a t
|}];;
type 'a t = < x : 'a >;;
[%%expect{|
type 'a t = < x : 'a >
|}];;
fun (x : 'a t as 'a) -> ();;
[%%expect{|
- : ('a t as 'a) -> unit = <fun>
|}];;
fun (x : 'a t) -> (x : 'a); ();;
[%%expect{|
Line 1, characters 18-26:
1 | fun (x : 'a t) -> (x : 'a); ();;
                      ^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.
- : ('a t as 'a) t -> unit = <fun>
|}];;

class ['a] c () = object
  constraint 'a = < .. > -> unit
  method m = (fun x -> () : 'a)
end;;
[%%expect{|
class ['a] c :
  unit ->
  object constraint 'a = (< .. > as 'b) -> unit method m : 'b -> unit end
|}];;
class ['a] c () = object
  constraint 'a = unit -> < .. >
  method m (f : 'a) = f ()
end;;
[%%expect{|
class ['a] c :
  unit ->
  object constraint 'a = unit -> (< .. > as 'b) method m : 'a -> 'b end
|}];;

class c () = object (self)
  method private m = 1
  method n = self#m
end;;
[%%expect{|
class c : unit -> object method private m : int method n : int end
|}];;

class d () = object (self)
  inherit c ()
  method o = self#m
end;;
[%%expect{|
class d :
  unit -> object method private m : int method n : int method o : int end
|}];;

let x = new d () in x#n, x#o;;
[%%expect{|
- : int * int = (1, 1)
|}];;

class c () = object method virtual m : int method private m = 1 end;;
[%%expect{|
class c : unit -> object method m : int end
|}];;

(* Recursion (cf. PR#5291) *)

class a = let _ = new b in object end
and b = let _ = new a in object end;;
[%%expect{|
Line 1, characters 10-37:
1 | class a = let _ = new b in object end
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of recursive class expression is not allowed
|}];;

class a = let _ = new a in object end;;
[%%expect{|
Line 1, characters 10-37:
1 | class a = let _ = new a in object end;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of recursive class expression is not allowed
|}];;

(* More tests about recursion in class declarations *)
class a = let _x() = new a in object end;;
[%%expect{|
class a : object  end
|}];;

class a = object end
and b = let _x() = new a in object end;;
[%%expect{|
class a : object  end
and b : object  end
|}];;

class a = let x() = new a in let y = x() in object end;;
[%%expect{|
Line 1, characters 10-54:
1 | class a = let x() = new a in let y = x() in object end;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of recursive class expression is not allowed
|}];;

class a = object end
and b = let x() = new a in let y = x() in object end;;
[%%expect{|
Line 2, characters 8-52:
2 | and b = let x() = new a in let y = x() in object end;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of recursive class expression is not allowed
|}];;

class a = object val x = 3 val y = x + 2 end;;
[%%expect{|
Line 1, characters 35-36:
1 | class a = object val x = 3 val y = x + 2 end;;
                                       ^
Error: The instance variable x
       cannot be accessed from the definition of another instance variable
|}];;

class a = object (self) val x = self#m method m = 3 end;;
[%%expect{|
Line 1, characters 32-36:
1 | class a = object (self) val x = self#m method m = 3 end;;
                                    ^^^^
Error: The self variable self
       cannot be accessed from the definition of an instance variable
|}];;

class a = object method m = 3 end
class b = object inherit a as super val x = super#m end;;
[%%expect{|
class a : object method m : int end
Line 2, characters 44-49:
2 | class b = object inherit a as super val x = super#m end;;
                                                ^^^^^
Error: The ancestor variable super
       cannot be accessed from the definition of an instance variable
|}];;

(* Some more tests of class idiosyncrasies *)

class c = object method private m = 3 end
  and d = object method o = object inherit c end end;;
[%%expect {|
class c : object method private m : int end
and d : object method o : c end
|}];;

class c = object(_ : 'self)
  method o = object(_ : 'self) method o = assert false end
end;;
[%%expect {|
Line 2, characters 13-58:
2 |   method o = object(_ : 'self) method o = assert false end
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot close type of object literal: < o : '_weak3; _.. >
       it has been unified with the self type of a class that is not yet
       completely defined.
|}];;

class c = object
    method m = 1
    inherit object (self)
      method n = self#m
    end
  end;;
[%%expect {|
Line 4, characters 17-23:
4 |       method n = self#m
                     ^^^^^^
Warning 17 [undeclared-virtual-method]: the virtual method m is not declared.
class c : object method m : int method n : int end
|}];;

class virtual c = object (self : 'c)
  constraint 'c = < f : int; .. >
end
[%%expect {|
class virtual c : object method virtual f : int end
|}];;

class virtual c = object (self : 'c)
  constraint 'c = < f : int; .. >
  method g = self # f
end
[%%expect {|
class virtual c : object method virtual f : int method g : int end
|}];;

class [ 'a ] c = object (_ : 'a) end;;
let o = object
    method m = 1
    inherit [ < m : int > ] c
  end;;
[%%expect {|
class ['a] c : object ('a) constraint 'a = < .. > end
Line 4, characters 14-25:
4 |     inherit [ < m : int > ] c
                  ^^^^^^^^^^^
Error: The type parameter < m : int >
       does not meet its constraint: it should be < .. >
       Self type cannot be unified with a closed object type
|}];;

class type [ 'a ] d = object method a : 'a method b : 'a end
class c : ['a] d = object (self) method a = 1 method b = assert false end;;
[%%expect {|
class type ['a] d = object method a : 'a method b : 'a end
Line 2, characters 19-73:
2 | class c : ['a] d = object (self) method a = 1 method b = assert false end;;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The class type object method a : int method b : 'a end
       is not matched by the class type ['_a] d
       The class type object method a : int method b : 'a end
       is not matched by the class type
         object method a : 'a method b : 'a end
       The method a has type int but is expected to have type 'a
       Type int is not compatible with type 'a
|}];;

class type ['a] ct = object ('a) end
class c : [ < a : int; ..> ] ct = object method a = 3 end;;
[%%expect {|
class type ['a] ct = object ('a) constraint 'a = < .. > end
Line 2, characters 10-31:
2 | class c : [ < a : int; ..> ] ct = object method a = 3 end;;
              ^^^^^^^^^^^^^^^^^^^^^
Error: This non-virtual class has undeclared virtual methods.
       The following methods were not declared : a
|}];;

class virtual c : [ < a : int; ..> ] ct = object method a = 3 end;;
[%%expect {|
class virtual c : object method virtual a : int end
|}];;

class c : object
  method m : < m : 'a > as 'a
  end = object (self)
  method m = self
end;;
[%%expect {|
Lines 3-5, characters 8-3:
3 | ........object (self)
4 |   method m = self
5 | end..
Error: The class type object ('a) method m : < m : 'a; .. > as 'a end
       is not matched by the class type
         object method m : < m : 'a > as 'a end
       The method m has type < m : 'a; .. > as 'a
       but is expected to have type < m : 'b > as 'b
       Type 'a is not compatible with type <  >
|}];;

class c :
  object
    method foo : < foo : int; .. > -> < foo : int> -> unit
  end =
  object
    method foo : 'a. (< foo : int; .. > as 'a) -> 'a -> unit = assert false
  end;;
[%%expect {|
Lines 5-7, characters 2-5:
5 | ..object
6 |     method foo : 'a. (< foo : int; .. > as 'a) -> 'a -> unit = assert false
7 |   end..
Error: The class type
         object method foo : (< foo : int; .. > as 'a) -> 'a -> unit end
       is not matched by the class type
         object method foo : < foo : int; .. > -> < foo : int > -> unit end
       The method foo has type 'a. (< foo : int; .. > as 'a) -> 'a -> unit
       but is expected to have type
         'b. (< foo : int; .. > as 'b) -> < foo : int > -> unit
       Type 'c is not compatible with type <  >
|}];;


class c = (fun x -> object(_:'foo) end) 3;;
[%%expect {|
class c : object  end
|}];;

class virtual c =
  ((fun (x : 'self -> unit) -> object(_:'self) end) (fun (_ : < a : int; .. >) -> ())
   : object method virtual a : int end)
[%%expect {|
class virtual c : object method virtual a : int end
|}];;

class c = object
  val x = 3
  method o = {< x = 4; y = 5 >}
  val y = 4
end;;
[%%expect {|
class c : object ('a) val x : int val y : int method o : 'a end
|}];;

class c : object('self) method m : < m : 'a; x : int; ..> -> unit as 'a end =
    object (_ : 'self) method m (_ : 'self) = () end;;
[%%expect {|
Line 2, characters 4-52:
2 |     object (_ : 'self) method m (_ : 'self) = () end;;
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The class type
         object ('a) method m : (< m : 'a -> unit; .. > as 'a) -> unit end
       is not matched by the class type
         object method m : < m : 'a; x : int; .. > -> unit as 'a end
       The method m has type (< m : 'a -> unit; .. > as 'a) -> unit
       but is expected to have type
         'b. (< m : 'c; x : int; .. > as 'b) -> unit as 'c
       Type 'a is not compatible with type < x : int; .. >
|}];;

let is_empty (x : < >) = ()
class c = object (self) method private foo = is_empty self end;;
[%%expect {|
val is_empty : <  > -> unit = <fun>
Line 2, characters 54-58:
2 | class c = object (self) method private foo = is_empty self end;;
                                                          ^^^^
Error: This expression has type < .. > but an expression was expected of type
         <  >
       Self type cannot be unified with a closed object type
|}];;

(* Warnings about private methods implicitly made public *)
let has_foo (x : < foo : 'a; .. >) = ()

class c = object (self) method private foo = 5 initializer has_foo self end;;
[%%expect {|
val has_foo : < foo : 'a; .. > -> unit = <fun>
Line 3, characters 10-75:
3 | class c = object (self) method private foo = 5 initializer has_foo self end;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 15 [implicit-public-methods]: the following private methods were made public implicitly:
 foo.
class c : object method foo : int end
|}];;

class type c = object(< foo : 'a; ..>) method private foo : int end;;
[%%expect {|
class type c = object method foo : int end
|}];;

class ['a] p = object (_ : 'a) method private foo = 5 end;;
class c = [ < foo : int; .. > ] p;;
[%%expect {|
class ['a] p :
  object ('a) constraint 'a = < .. > method private foo : int end
class c : object method foo : int end
|}];;

(* Errors for undefined methods *)

class c = object method virtual foo : int end;;
[%%expect {|
Line 1, characters 10-45:
1 | class c = object method virtual foo : int end;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This non-virtual class has virtual methods.
       The following methods are virtual : foo
|}];;

class type ct = object method virtual foo : int end;;
[%%expect {|
Line 1, characters 16-51:
1 | class type ct = object method virtual foo : int end;;
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This non-virtual class type has virtual methods.
       The following methods are virtual : foo
|}];;

let o = object method virtual foo : int end;;
[%%expect {|
Line 1, characters 8-43:
1 | let o = object method virtual foo : int end;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This object has virtual methods.
       The following methods are virtual : foo
|}];;

class c = object(self) initializer self#foo end;;
[%%expect {|
Line 1, characters 35-39:
1 | class c = object(self) initializer self#foo end;;
                                       ^^^^
Error: This expression has no method foo
|}];;

let o = object(self) initializer self#foo end;;
[%%expect {|
Line 1, characters 33-37:
1 | let o = object(self) initializer self#foo end;;
                                     ^^^^
Error: This expression has no method foo
|}];;

let has_foo (x : < foo : int; ..>) = ()
class c = object(self) initializer has_foo self end;;
[%%expect {|
val has_foo : < foo : int; .. > -> unit = <fun>
Line 2, characters 10-51:
2 | class c = object(self) initializer has_foo self end;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This non-virtual class has undeclared virtual methods.
       The following methods were not declared : foo
|}];;

let o = object(self) initializer has_foo self end;;
[%%expect {|
Line 1, characters 41-45:
1 | let o = object(self) initializer has_foo self end;;
                                             ^^^^
Error: This expression has type <  > but an expression was expected of type
         < foo : int; .. >
       The first object type has no method foo
|}];;

class c = object(_ : < foo : int; ..>) end;;
[%%expect {|
Line 1, characters 10-42:
1 | class c = object(_ : < foo : int; ..>) end;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This non-virtual class has undeclared virtual methods.
       The following methods were not declared : foo
|}];;

class type ct = object(< foo : int; ..>) end;;
[%%expect {|
Line 1, characters 16-44:
1 | class type ct = object(< foo : int; ..>) end;;
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This non-virtual class type has undeclared virtual methods.
       The following methods were not declared : foo
|}];;

let o = object(_ : < foo : int; ..>) end;;
[%%expect {|
Line 1, characters 8-40:
1 | let o = object(_ : < foo : int; ..>) end;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This object has undeclared virtual methods.
       The following methods were not declared : foo
|}];;

(* Shadowing/overriding methods in class types *)

class type c = object
  val x : int
  val x : float
end;;
[%%expect {|
class type c = object val x : float end
|}];;

class type c = object
  val x : int
  val mutable x : int
end;;
[%%expect {|
class type c = object val mutable x : int end
|}];;

class type c = object
  val mutable x : int
  val x : int
end;;
[%%expect {|
class type c = object val x : int end
|}];;

class type virtual c = object
  val virtual x : int
  val x : int
end;;
[%%expect {|
class type c = object val x : int end
|}];;

class type virtual c = object
  val x : int
  val virtual x : int
end;;
[%%expect {|
class type c = object val x : int end
|}];;

class type virtual c = object
  val x : int
  val virtual x : float
end;;
[%%expect {|
class type c = object val x : float end
|}];;

class c = object
  method virtual private test : unit
  method private test = ()
end

let () = (new c)#test
[%%expect {|
class c : object method private test : unit end
Line 6, characters 9-16:
6 | let () = (new c)#test
             ^^^^^^^
Error: This expression has type c
       It has no method test
|}];;

class c = object
  method virtual private test : unit
  method test = ()
end

let () = (new c)#test
[%%expect {|
class c : object method test : unit end
|}];;

class virtual d = object
  method virtual private test : unit
end

class c = object
  inherit d
  method private test = ()
end

let () = (new c)#test
[%%expect {|
class virtual d : object method private virtual test : unit end
class c : object method private test : unit end
Line 10, characters 9-16:
10 | let () = (new c)#test
              ^^^^^^^
Error: This expression has type c
       It has no method test
|}];;

class c = object
  inherit d
  method test = ()
end

let () = (new c)#test
[%%expect {|
class c : object method test : unit end
|}];;

class foo =
  object
    method private f (b : bool) = b
    inherit object
      method f (b : bool) = b
    end
  end
let _ = (new foo)#f true
[%%expect {|
class foo : object method f : bool -> bool end
- : bool = true
|}];;


class c : object
    method virtual m : int
end = object
    method m = 9
  end
[%%expect {|
Lines 1-3, characters 10-3:
1 | ..........object
2 |     method virtual m : int
3 | end.........
Error: This non-virtual class type has virtual methods.
       The following methods are virtual : m
|}];;

class virtual c : object
    method virtual m : int
end = object
    method m = 42
  end
[%%expect {|
class virtual c : object method virtual m : int end
|}];;

class virtual cv = object
    method virtual m : int
  end

class c : cv = object
    method m = 42
  end
[%%expect {|
class virtual cv : object method virtual m : int end
Line 5, characters 10-12:
5 | class c : cv = object
              ^^
Error: This non-virtual class type has virtual methods.
       The following methods are virtual : m
|}];;

class virtual c : cv = object
    method m = 41
  end
[%%expect {|
class virtual c : cv
|}];;

class c = cv
[%%expect {|
Line 1, characters 10-12:
1 | class c = cv
              ^^
Error: This non-virtual class has virtual methods.
       The following methods are virtual : m
|}];;

class virtual c = cv
[%%expect {|
class virtual c : cv
|}];;
