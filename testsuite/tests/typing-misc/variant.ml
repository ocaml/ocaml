(* TEST
   * expect
*)

(* PR#6394 *)

module rec X : sig
 type t = int * bool
end = struct
 type t = A | B
 let f = function A | B -> 0
end;;
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |  type t = A | B
5 |  let f = function A | B -> 0
6 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t = A | B val f : t -> int end
       is not included in
         sig type t = int * bool end
       Type declarations do not match:
         type t = X.t = A | B
       is not included in
         type t = int * bool
       The type X.t is not equal to the type int * bool
|}];;


(* PR#7838 *)

module Make (X : sig val f : [ `A ] -> unit end) = struct
 let make f1 f2 arg = match arg with `A -> f1 arg; f2 arg
 let f = make X.f (fun _ -> ())
end;;
[%%expect{|
module Make :
  functor (X : sig val f : [ `A ] -> unit end) ->
    sig
      val make : (([< `A ] as 'a) -> 'b) -> ('a -> 'c) -> 'a -> 'c
      val f : [ `A ] -> unit
    end
|}]


(* reexport *)
type ('a,'b) def = X of int constraint 'b = [> `A]

type arity = (int, [`A]) def = X of int;;
[%%expect{|
type ('a, 'b) def = X of int constraint 'b = [> `A ]
type arity = (int, [ `A ]) def = X of int
|}]

type ('a,'b) ct = (int,'b) def = X of int;;
[%%expect{|
type ('a, 'b) ct = (int, 'b) def = X of int constraint 'b = [ `A ]
|}]

type ('a,'b) kind = ('a, 'b) def = {a:int} constraint 'b = [> `A];;
[%%expect{|
Line 1, characters 0-65:
1 | type ('a,'b) kind = ('a, 'b) def = {a:int} constraint 'b = [> `A];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         ('a, [ `A ]) def
       Their kinds differ.
|}]

type d = X of int | Y of int

type missing = d = X of int
[%%expect{|
type d = X of int | Y of int
Line 3, characters 0-27:
3 | type missing = d = X of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       An extra constructor, Y, is provided in the original definition.
|}]

type wrong_type = d = X of float
[%%expect{|
Line 1, characters 0-32:
1 | type wrong_type = d = X of float
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       1. Constructors do not match:
         X of int
       is not the same as:
         X of float
       The type int is not equal to the type float
       2. An extra constructor, Y, is provided in the original definition.
|}]

type mono = Foo of float
type unboxed = mono = Foo of float [@@unboxed]
[%%expect{|
type mono = Foo of float
Line 2, characters 0-46:
2 | type unboxed = mono = Foo of float [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type mono
       Their internal representations differ:
       this definition uses unboxed representation.
|}]

type perm = d = Y of int | X of int
[%%expect{|
Line 1, characters 0-35:
1 | type perm = d = Y of int | X of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       Constructors X and Y have been swapped.
|}]

module M : sig
  type t = Foo of int
end = struct
  type t = Foo : int -> t
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Foo : int -> t
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo : int -> t end
       is not included in
         sig type t = Foo of int end
       Type declarations do not match:
         type t = Foo : int -> t
       is not included in
         type t = Foo of int
       Constructors do not match:
         Foo : int -> t
       is not the same as:
         Foo of int
       The first has explicit return type and the second doesn't.
|}]

(* Adding type variables *)
module M = struct
  type t = Foo of int
end
module N = struct
  type 'a t = M.t = Foo of int
end;;
let m = M.Foo 15;;
let n = N.Foo 15;;
let f = function | M.Foo x -> x;;
let g = function | N.Foo x -> x;;
let f_m = f m;;
let f_n = f n;;
let g_m = g m;;
let g_n = g n;;
[%%expect{|
module M : sig type t = Foo of int end
module N : sig type 'a t = M.t = Foo of int end
val m : M.t = M.Foo 15
val n : 'a N.t = N.Foo 15
val f : M.t -> int = <fun>
val g : 'a N.t -> int = <fun>
val f_m : int = 15
val f_n : int = 15
val g_m : int = 15
val g_n : int = 15
|}]

(* Adding type variables with invalid constraint *)
module M = struct
  type t = Foo of int
end
module N = struct
  type 'a t = M.t = Foo of 'a
end;;
[%%expect{|
module M : sig type t = Foo of int end
Line 5, characters 2-29:
5 |   type 'a t = M.t = Foo of 'a
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type M.t
       Constructors do not match:
         Foo of int
       is not the same as:
         Foo of 'a
       The type int is not equal to the type 'a
|}]

(* Adding constrained type variables *)
module M = struct
  type t = Foo of int
end
module N = struct
  type 'a t = M.t = Foo of 'a constraint 'a = int
end;;
let m = M.Foo 15;;
let n = N.Foo 15;;
let f = function | M.Foo x -> x;;
let g = function | N.Foo x -> x;;
let f_m = f m;;
let f_n = f n;;
let g_m = g m;;
let g_n = g n;;
[%%expect{|
module M : sig type t = Foo of int end
module N : sig type 'a t = M.t = Foo of 'a constraint 'a = int end
val m : M.t = M.Foo 15
val n : int N.t = N.Foo 15
val f : M.t -> int = <fun>
val g : int N.t -> int = <fun>
val f_m : int = 15
val f_n : int = 15
val g_m : int = 15
val g_n : int = 15
|}]
let n_bad = N.Foo true;;
[%%expect{|
Line 1, characters 18-22:
1 | let n_bad = N.Foo true;;
                      ^^^^
Error: This expression has type bool but an expression was expected of type
         int
|}]

(* Mixing type variables *)
module M = struct
  type ('a, 'b) t = A of 'a | B of 'b
end
module N = struct
  type ('a, 'b) t = ('a -> 'b, int) M.t = A of ('a -> 'b) | B of int
end;;
let m_a = M.A (fun x -> (x, x));;
let m_b = M.B 15;;
let n_a = N.A (fun x -> (x, x));;
let n_b = N.B 15;;
let f = function | M.A f -> f 1 | M.B x -> (x, x);;
let g = function | N.A f -> f 1 | N.B x -> (x, x);;
let f_m_a = f m_a;;
let f_m_b = f m_b;;
let f_n_a = f n_a;;
let f_n_b = f n_b;;
let g_m_a = g m_a;;
let g_m_b = g m_b;;
let g_n_a = g n_a;;
let g_n_b = g n_b;;
[%%expect{|
module M : sig type ('a, 'b) t = A of 'a | B of 'b end
module N :
  sig type ('a, 'b) t = ('a -> 'b, int) M.t = A of ('a -> 'b) | B of int end
val m_a : ('a -> 'a * 'a, 'b) M.t = M.A <fun>
val m_b : ('a, int) M.t = M.B 15
val n_a : ('a, 'a * 'a) N.t = N.A <fun>
val n_b : ('a, 'b) N.t = N.B 15
val f : (int -> 'a * 'a, 'a) M.t -> 'a * 'a = <fun>
val g : (int, int * int) N.t -> int * int = <fun>
val f_m_a : int * int = (1, 1)
val f_m_b : int * int = (15, 15)
val f_n_a : int * int = (1, 1)
val f_n_b : int * int = (15, 15)
val g_m_a : int * int = (1, 1)
val g_m_b : int * int = (15, 15)
val g_n_a : int * int = (1, 1)
val g_n_b : int * int = (15, 15)
|}]
let m_a_bad = M.A 15;;
let g_m_a_bad = g m_a_bad;;
[%%expect{|
val m_a_bad : (int, 'a) M.t = M.A 15
Line 2, characters 18-25:
2 | let g_m_a_bad = g m_a_bad;;
                      ^^^^^^^
Error: This expression has type (int, 'a) M.t
       but an expression was expected of type
         (int, int * int) N.t = (int -> int * int, int) M.t
       Type int is not compatible with type int -> int * int
|}]
let m_b_bad = M.B true;;
let g_m_b_bad = g m_b_bad;;
[%%expect{|
val m_b_bad : ('a, bool) M.t = M.B true
Line 2, characters 18-25:
2 | let g_m_b_bad = g m_b_bad;;
                      ^^^^^^^
Error: This expression has type (int -> int * int, bool) M.t
       but an expression was expected of type
         (int, int * int) N.t = (int -> int * int, int) M.t
       Type bool is not compatible with type int
|}]

(* Invalid mixing of type variables *)
module M = struct
  type ('a, 'b) t = A of 'a | B of 'b
end
module N = struct
  type ('a, 'b) t = ('a -> 'b, int) M.t = A of 'a | B of bool
end;;
[%%expect{|
module M : sig type ('a, 'b) t = A of 'a | B of 'b end
Line 5, characters 2-61:
5 |   type ('a, 'b) t = ('a -> 'b, int) M.t = A of 'a | B of bool
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         ('a -> 'b, int) M.t
       1. Constructors do not match:
         A of ('a -> 'b)
       is not the same as:
         A of 'a
       The type 'a -> 'b is not equal to the type 'a
       2. Constructors do not match:
         B of int
       is not the same as:
         B of bool
       The type int is not equal to the type bool
|}]

(* Mixing between aliases *)
module M = struct
  type ('a, 'b) t = A of 'a | B of 'b
end
module N = struct
  type ('a, 'b) t = ('a * 'b, bool) M.t = A of ('a * 'b) | B of bool
end
module O = struct
  type 'c t = (int * unit, 'c) M.t = A of (int * unit) | B of 'c
end
let m_a = M.A ((1, ()));;
let m_b = M.B true;;
let n_a = N.A ((1, ()));;
let n_b = N.B true;;
let o_a = O.A ((1, ()));;
let o_b = O.B true;;
let f = function | M.A x -> Result.Ok x | M.B y -> Result.Error y;;
let g = function | N.A x -> Result.Ok x | N.B y -> Result.Error y;;
let h = function | O.A x -> Result.Ok x | O.B y -> Result.Error y;;
let f_m_a = f m_a;;
let f_m_b = f m_b;;
let f_n_a = f n_a;;
let f_n_b = f n_b;;
let f_o_a = f o_a;;
let f_o_b = f o_b;;
let g_m_a = g m_a;;
let g_m_b = g m_b;;
let g_n_a = g n_a;;
let g_n_b = g n_b;;
let g_o_a = g o_a;;
let g_o_b = g o_b;;
let h_m_a = h m_a;;
let h_m_b = h m_b;;
let h_n_a = h n_a;;
let h_n_b = h n_b;;
let h_o_a = h o_a;;
let h_o_b = h o_b;;
[%%expect{|
module M : sig type ('a, 'b) t = A of 'a | B of 'b end
module N :
  sig type ('a, 'b) t = ('a * 'b, bool) M.t = A of ('a * 'b) | B of bool end
module O :
  sig type 'c t = (int * unit, 'c) M.t = A of (int * unit) | B of 'c end
val m_a : (int * unit, 'a) M.t = M.A (1, ())
val m_b : ('a, bool) M.t = M.B true
val n_a : (int, unit) N.t = N.A (1, ())
val n_b : ('a, 'b) N.t = N.B true
val o_a : 'a O.t = O.A (1, ())
val o_b : bool O.t = O.B true
val f : ('a, 'b) M.t -> ('a, 'b) Result.t = <fun>
val g : ('a, 'b) N.t -> ('a * 'b, bool) Result.t = <fun>
val h : 'a O.t -> (int * unit, 'a) Result.t = <fun>
val f_m_a : (int * unit, 'a) Result.t = Result.Ok (1, ())
val f_m_b : ('a, bool) Result.t = Result.Error true
val f_n_a : (int * unit, bool) Result.t = Result.Ok (1, ())
val f_n_b : ('a * 'b, bool) Result.t = Result.Error true
val f_o_a : (int * unit, 'a) Result.t = Result.Ok (1, ())
val f_o_b : (int * unit, bool) Result.t = Result.Error true
val g_m_a : (int * unit, bool) Result.t = Result.Ok (1, ())
val g_m_b : ('a * 'b, bool) Result.t = Result.Error true
val g_n_a : (int * unit, bool) Result.t = Result.Ok (1, ())
val g_n_b : ('a * 'b, bool) Result.t = Result.Error true
val g_o_a : (int * unit, bool) Result.t = Result.Ok (1, ())
val g_o_b : (int * unit, bool) Result.t = Result.Error true
val h_m_a : (int * unit, 'a) Result.t = Result.Ok (1, ())
val h_m_b : (int * unit, bool) Result.t = Result.Error true
val h_n_a : (int * unit, bool) Result.t = Result.Ok (1, ())
val h_n_b : (int * unit, bool) Result.t = Result.Error true
val h_o_a : (int * unit, 'a) Result.t = Result.Ok (1, ())
val h_o_b : (int * unit, bool) Result.t = Result.Error true
|}]

(* Mixing between patterns *)
let f_m_n = function | M.A x -> Result.Ok x | N.B y -> Result.Error y;;
let f_m_o = function | M.A x -> Result.Ok x | O.B y -> Result.Error y;;
let f_n_m = function | N.A x -> Result.Ok x | M.B y -> Result.Error y;;
let f_n_o = function | N.A x -> Result.Ok x | O.B y -> Result.Error y;;
let f_o_m = function | N.A x -> Result.Ok x | M.B y -> Result.Error y;;
let f_o_n = function | O.A x -> Result.Ok x | N.B y -> Result.Error y;;
[%%expect {|
val f_m_n : ('a, 'b) N.t -> ('a * 'b, bool) Result.t = <fun>
val f_m_o : 'a O.t -> (int * unit, 'a) Result.t = <fun>
val f_n_m : ('a, 'b) N.t -> ('a * 'b, bool) Result.t = <fun>
val f_n_o : (int, unit) N.t -> (int * unit, bool) Result.t = <fun>
val f_o_m : ('a, 'b) N.t -> ('a * 'b, bool) Result.t = <fun>
val f_o_n : bool O.t -> (int * unit, bool) Result.t = <fun>
|}]

(* Restricting GADTs *)
module M = struct
  type 'a t = Foo : 'a -> 'a t
end
module N = struct
  type t = int M.t = Foo : int -> t
end;;
let m = M.Foo 15;;
let n = N.Foo 15;;
let f = function M.Foo x -> x;;
let g = function N.Foo x -> x;;
let f_m = f m;;
let f_n = f n;;
let g_m = g m;;
let g_n = g n;;
[%%expect {|
module M : sig type 'a t = Foo : 'a -> 'a t end
module N : sig type t = int M.t = Foo : int -> t end
val m : int M.t = M.Foo 15
val n : N.t = N.Foo 15
val f : int M.t -> int = <fun>
val g : N.t -> int = <fun>
val f_m : int = 15
val f_n : int = 15
val g_m : int = 15
val g_n : int = 15
|}]
let m_bad = M.Foo true;;
let g_m_bad = g m_bad;;
[%%expect {|
Line 1, characters 18-22:
1 | let m_bad = M.Foo true;;
                      ^^^^
Error: This expression has type bool but an expression was expected of type
         int
|}]

(* Restricted GADTs *)
let f (x : _ M.t) =
  match x with
  | N.Foo y -> y;;
let g (x : N.t) =
  match x with
  | N.Foo y -> y;;
let h (type a) (x : a M.t) =
  match x with
  | N.Foo y -> y;;
[%%expect {|
val f : int M.t -> int = <fun>
val g : N.t -> int = <fun>
Line 9, characters 4-11:
9 |   | N.Foo y -> y;;
        ^^^^^^^
Error: This pattern matches values of type N.t = int M.t
       but a pattern was expected which matches values of type a M.t
       Type int is not compatible with type a
|}]

(* Irreconcilable GADTs *)
module M = struct
  type _ t = Int : int t | Bool : bool t
end
module N = struct
  type t = int M.t = Int : t | Bool : t
end
[%%expect {|
module M : sig type _ t = Int : int t | Bool : bool t end
Line 5, characters 2-39:
5 |   type t = int M.t = Int : t | Bool : t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type int M.t
       Constructors do not match:
         Bool : bool M.t
       is not the same as:
         Bool : int M.t
       The type bool M.t is not equal to the type int M.t
       Type bool is not equal to type int
|}]

(* Conditionally reconcilable GADTs *)
module M = struct
  type _ t = Any : 'a t | Bool : bool t
end
module N = struct
  type t = bool M.t = Any : t | Bool : t
end
module O = struct
  type t = int M.t = Any : t | Bool : t
end
[%%expect {|
module M : sig type _ t = Any : 'a t | Bool : bool t end
module N : sig type t = bool M.t = Any : t | Bool : t end
Line 8, characters 2-39:
8 |   type t = int M.t = Any : t | Bool : t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type int M.t
       1. Constructors do not match:
         Any : bool M.t
       is not the same as:
         Any : int M.t
       The type bool M.t is not equal to the type int M.t
       Type bool is not equal to type int
       2. Constructors do not match:
         Bool : bool M.t
       is not the same as:
         Bool : int M.t
       The type bool M.t is not equal to the type int M.t
       Type bool is not equal to type int
|}]
