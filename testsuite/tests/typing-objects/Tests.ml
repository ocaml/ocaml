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
and ['a] d : unit -> object constraint 'a = int method f : int c end
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
Line _, characters 4-45:
  ....and d () = object
    inherit ['a] c ()
  end..
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
Line _, characters 0-48:
  class x () = object
    method virtual f : int
  end..
Error: This class should be virtual. The following methods are undefined : f
|}];;
(* The class x should be virtual:  its methods f is undefined *)

(* Supplementary method g *)
class virtual c ((x : 'a): < f : int >) = object (_ : 'a) end
and virtual d x = object (_ : 'a)
  inherit c x
  method g = true
end;;
[%%expect{|
Line _, characters 49-57:
  class virtual c ((x : 'a): < f : int >) = object (_ : 'a) end
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
Line _, characters 0-78:
  class ['a] c () = object
    constraint 'a = int
    method f x = (x : bool c)
  end..
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
    constraint 'a = int -> 'c
    constraint 'b = 'a * < x : 'b > * 'c * 'd
    method f : 'a -> 'b -> unit
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
Line _, characters 0-50:
  class ['a] c () = object
    method f = (x : 'a)
  end..
Error: The type of this class,
       class ['a] c :
         unit -> object constraint 'a = '_weak1 list ref method f : 'a end,
       contains type variables that cannot be generalized
|}];;

(* Abbreviations *)
type 'a c = <f : 'a c; g : 'a d>
and 'a d = <f : int c>;;
[%%expect{|
Line _, characters 0-32:
  type 'a c = <f : 'a c; g : 'a d>
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the definition of d, type int c should be 'a c
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
Line _, characters 0-17:
  and 'a t = 'a t u;;
  ^^^^^^^^^^^^^^^^^
Error: The definition of t contains a cycle:
       'a t u
|}];; (* fails since 4.04 *)
type 'a u = 'a
and 'a t = 'a t u;;
[%%expect{|
Line _, characters 0-17:
  and 'a t = 'a t u;;
  ^^^^^^^^^^^^^^^^^
Error: The type abbreviation t is cyclic
|}];;
type 'a u = 'a;;
[%%expect{|
type 'a u = 'a
|}];;
type t = t u * t u;;
[%%expect{|
Line _, characters 0-18:
  type t = t u * t u;;
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
    method f : 'a -> unit
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
    method f : 'a -> unit
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
Line _, characters 30-34:
  class c () = object val x = - true val y = -. () end;;
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
Line _, characters 10-13:
    inherit c 5
            ^^^
Warning 13: the following instance variables are overridden by the class c :
  x
The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)
Line _, characters 6-7:
    val y = 3
        ^
Warning 13: the instance variable y is overridden.
The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)
Line _, characters 10-13:
    inherit d 7
            ^^^
Warning 13: the following instance variables are overridden by the class d :
  t z
The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)
Line _, characters 6-7:
    val u = 3
        ^
Warning 13: the instance variable u is overridden.
The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)
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
Line _, characters 0-153:
  class virtual ['a] matrix (sz, init : int * 'a) = object
    val m = Array.make_matrix sz sz init
    method add (mtx : 'a matrix) = (mtx#m.(0).(0) : 'a)
  end..
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
Line _, characters 12-43:
  ............struct
    let f (x : #c) = x
  end......
Error: Signature mismatch:
       Modules do not match:
         sig val f : (#c as 'a) -> 'a end
       is not included in
         sig val f : #c -> #c end
       Values do not match:
         val f : (#c as 'a) -> 'a
       is not included in
         val f : #c -> #c
|}];;

module M = struct type t = int class t () = object end end;;
[%%expect{|
Line _, characters 37-38:
  module M = struct type t = int class t () = object end end;;
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
Line _, characters 9-38:
  fun x -> (x : int -> bool :> 'a -> 'a);;
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type int -> bool is not a subtype of int -> int
       Type bool is not a subtype of int
|}];;
fun x -> (x : int -> bool :> int -> int);;
[%%expect{|
Line _, characters 9-40:
  fun x -> (x : int -> bool :> int -> int);;
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
module F : functor (X : sig  end) -> sig type t = int end
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
Line _, characters 9-19:
  fun (x : 'a t as 'a) -> ();;
           ^^^^^^^^^^
Error: This alias is bound to type 'a t but is used as an instance of type 'a
       The type variable 'a occurs inside 'a t
|}];;
fun (x : 'a t) -> (x : 'a); ();;
[%%expect{|
Line _, characters 19-20:
  fun (x : 'a t) -> (x : 'a); ();;
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
Line _, characters 18-26:
  fun (x : 'a t) -> (x : 'a); ();;
                    ^^^^^^^^
Warning 10: this expression should have type unit.
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

(* Marshaling (cf. PR#5436) *)

let r = ref 0;;
[%%expect{|
val r : int ref = {contents = 0}
|}];;
let id o = Oo.id o - !r;;
[%%expect{|
val id : < .. > -> int = <fun>
|}];;
r := Oo.id (object end);;
[%%expect{|
- : unit = ()
|}];;
id (object end);;
[%%expect{|
- : int = 1
|}];;
id (object end);;
[%%expect{|
- : int = 2
|}];;
let o = object end in
  let s = Marshal.to_string o [] in
  let o' : < > = Marshal.from_string s 0 in
  let o'' : < > = Marshal.from_string s 0 in
  (id o, id o', id o'');;
[%%expect{|
- : int * int * int = (3, 4, 5)
|}];;

let o = object val x = 33 method m = x end in
  let s = Marshal.to_string o [Marshal.Closures] in
  let o' : <m:int> = Marshal.from_string s 0 in
  let o'' : <m:int> = Marshal.from_string s 0 in
  (id o, id o', id o'', o#m, o'#m);;
[%%expect{|
- : int * int * int * int * int = (6, 7, 8, 33, 33)
|}];;

let o = object val x = 33 val y = 44 method m = x end in
  let s = Marshal.to_string (o,o) [Marshal.Closures] in
  let (o1, o2) : (<m:int> * <m:int>) = Marshal.from_string s 0 in
  let (o3, o4) : (<m:int> * <m:int>) = Marshal.from_string s 0 in
  (id o, id o1, id o2, id o3, id o4, o#m, o1#m);;
[%%expect{|
- : int * int * int * int * int * int * int = (9, 10, 10, 11, 11, 33, 33)
|}];;

(* Recursion (cf. PR#5291) *)

class a = let _ = new b in object end
and b = let _ = new a in object end;;
[%%expect{|
Line _, characters 10-37:
  class a = let _ = new b in object end
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of recursive class expression is not allowed
|}];;

class a = let _ = new a in object end;;
[%%expect{|
Line _, characters 10-37:
  class a = let _ = new a in object end;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of recursive class expression is not allowed
|}];;
