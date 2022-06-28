(* TEST
   * expect
*)

(* undefined labels *)
type t = {x:int;y:int};;
{x=3;z=2};;
[%%expect{|
type t = { x : int; y : int; }
Line 2, characters 5-6:
2 | {x=3;z=2};;
         ^
Error: Unbound record field z
|}];;
fun {x=3;z=2} -> ();;
[%%expect{|
Line 1, characters 9-10:
1 | fun {x=3;z=2} -> ();;
             ^
Error: Unbound record field z
|}];;

(* mixed labels *)
{x=3; contents=2};;
[%%expect{|
Line 1, characters 6-14:
1 | {x=3; contents=2};;
          ^^^^^^^^
Error: The record field contents belongs to the type 'a ref
       but is mixed here with fields of type t
|}];;

(* private types *)
type u = private {mutable u:int};;
{u=3};;
[%%expect{|
type u = private { mutable u : int; }
Line 2, characters 0-5:
2 | {u=3};;
    ^^^^^
Error: Cannot create values of the private type u
|}];;
fun x -> x.u <- 3;;
[%%expect{|
Line 1, characters 11-12:
1 | fun x -> x.u <- 3;;
               ^
Error: Cannot assign field u of the private type u
|}];;

(* Punning and abbreviations *)
module M = struct
  type t = {x: int; y: int}
end;;
[%%expect{|
module M : sig type t = { x : int; y : int; } end
|}];;

let f {M.x; y} = x+y;;
let r = {M.x=1; y=2};;
let z = f r;;
[%%expect{|
val f : M.t -> int = <fun>
val r : M.t = {M.x = 1; y = 2}
val z : int = 3
|}];;

(* messages *)
type foo = { mutable y:int };;
let f (r: int) = r.y <- 3;;
[%%expect{|
type foo = { mutable y : int; }
Line 2, characters 17-18:
2 | let f (r: int) = r.y <- 3;;
                     ^
Error: This expression has type int but an expression was expected of type
         foo
|}];;

let f (r: int) =
  match r with
  | { contents = 3 } -> ()
[%%expect{|
Line 3, characters 4-20:
3 |   | { contents = 3 } -> ()
        ^^^^^^^^^^^^^^^^
Error: This pattern matches values of type int ref
       but a pattern was expected which matches values of type int
|}];;



(* bugs *)
type foo = { y: int; z: int };;
type bar = { x: int };;
let f (r: bar) = ({ r with z = 3 } : foo)
[%%expect{|
type foo = { y : int; z : int; }
type bar = { x : int; }
Line 3, characters 20-21:
3 | let f (r: bar) = ({ r with z = 3 } : foo)
                        ^
Error: This expression has type bar but an expression was expected of type
         foo
|}];;

type foo = { x: int };;
let r : foo = { ZZZ.x = 2 };;
[%%expect{|
type foo = { x : int; }
Line 2, characters 16-21:
2 | let r : foo = { ZZZ.x = 2 };;
                    ^^^^^
Error: Unbound module ZZZ
|}];;

(ZZZ.X : int option);;
[%%expect{|
Line 1, characters 1-6:
1 | (ZZZ.X : int option);;
     ^^^^^
Error: Unbound module ZZZ
|}];;

(* PR#5865 *)
let f (x : Complex.t) = x.Complex.z;;
[%%expect{|
Line 1, characters 26-35:
1 | let f (x : Complex.t) = x.Complex.z;;
                              ^^^^^^^^^
Error: Unbound record field Complex.z
|}];;

(* PR#6608 *)
{ true with contents = 0 };;
[%%expect{|
Line 1, characters 2-6:
1 | { true with contents = 0 };;
      ^^^^
Error: This expression has type bool which is not a record type.
|}];;

type ('a, 'b) t = { fst : 'a; snd : 'b };;
let with_fst r fst = { r with fst };;
with_fst { fst=""; snd="" } 2;;
[%%expect{|
type ('a, 'b) t = { fst : 'a; snd : 'b; }
val with_fst : ('a, 'b) t -> 'c -> ('c, 'b) t = <fun>
- : (int, string) t = {fst = 2; snd = ""}
|}];;

(* PR#7695 *)
type 'a t = { f : 'a; g : 'a };;
let x = { f = 12; g = 43 };;
{x with f = "hola"};;
[%%expect{|
type 'a t = { f : 'a; g : 'a; }
val x : int t = {f = 12; g = 43}
Line 3, characters 0-19:
3 | {x with f = "hola"};;
    ^^^^^^^^^^^^^^^^^^^
Error: This expression has type string t
       but an expression was expected of type int t
       Type string is not compatible with type int
|}]

(* PR#7696 *)
let r = { (assert false) with contents = 1 } ;;
[%%expect{|
Line 1, characters 8-44:
1 | let r = { (assert false) with contents = 1 } ;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 23 [useless-record-with]: all the fields are explicitly listed in this record:
the 'with' clause is useless.
Exception: Assert_failure ("", 1, 10).
|}]

(* reexport *)

type ('a,'b) def = { x:int } constraint 'b = [> `A]

type arity = (int, [`A]) def = {x:int};;
[%%expect{|
type ('a, 'b) def = { x : int; } constraint 'b = [> `A ]
type arity = (int, [ `A ]) def = { x : int; }
|}]

type ('a,'b) ct = (int,'b) def = {x:int};;
[%%expect{|
type ('a, 'b) ct = (int, 'b) def = { x : int; } constraint 'b = [ `A ]
|}]

type ('a,'b) kind = ('a, 'b) def = A constraint 'b = [> `A];;
[%%expect{|
Line 1, characters 0-59:
1 | type ('a,'b) kind = ('a, 'b) def = A constraint 'b = [> `A];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         ('a, [ `A ]) def
       Their kinds differ.
|}]

type d = { x:int; y : int }
type mut = d = {x:int; mutable y:int}
[%%expect{|
type d = { x : int; y : int; }
Line 2, characters 0-37:
2 | type mut = d = {x:int; mutable y:int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       Fields do not match:
         y : int;
       is not the same as:
         mutable y : int;
       This is mutable and the original is not.
|}]

type missing = d = { x:int }
[%%expect{|
Line 1, characters 0-28:
1 | type missing = d = { x:int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       An extra field, y, is provided in the original definition.
|}]

type wrong_type = d = {x:float}
[%%expect{|
Line 1, characters 0-31:
1 | type wrong_type = d = {x:float}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       1. Fields do not match:
         x : int;
       is not the same as:
         x : float;
       The type int is not equal to the type float
       2. An extra field, y, is provided in the original definition.
|}]

type mono = {foo:int}
type unboxed = mono = {foo:int} [@@unboxed]
[%%expect{|
type mono = { foo : int; }
Line 2, characters 0-43:
2 | type unboxed = mono = {foo:int} [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type mono
       Their internal representations differ:
       this definition uses unboxed representation.
|}]

type perm = d = {y:int; x:int}
[%%expect{|
Line 1, characters 0-30:
1 | type perm = d = {y:int; x:int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       Fields x and y have been swapped.
|}]

(* Adding type variables *)
module M = struct
  type t = {foo: int}
end
module N = struct
  type 'a t = M.t = {foo: int}
end;;
let m = {M.foo= 15};;
let n = {N.foo= 15};;
let f = function | {M.foo= x} -> x;;
let g = function | {N.foo= x} -> x;;
let f_m = f m;;
let f_n = f n;;
let g_m = g m;;
let g_n = g n;;
[%%expect{|
module M : sig type t = { foo : int; } end
module N : sig type 'a t = M.t = { foo : int; } end
val m : M.t = {M.foo = 15}
val n : 'a N.t = {N.foo = 15}
val f : M.t -> int = <fun>
val g : 'a N.t -> int = <fun>
val f_m : int = 15
val f_n : int = 15
val g_m : int = 15
val g_n : int = 15
|}]

(* Adding type variables with invalid constraint *)
module M = struct
  type t = {foo: int}
end
module N = struct
  type 'a t = M.t = {foo: 'a}
end;;
[%%expect{|
module M : sig type t = { foo : int; } end
Line 5, characters 2-29:
5 |   type 'a t = M.t = {foo: 'a}
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type M.t
       Fields do not match:
         foo : int;
       is not the same as:
         foo : 'a;
       The type int is not equal to the type 'a
|}]

(* Adding constrained type variables *)
module M = struct
  type t = {foo: int}
end
module N = struct
  type 'a t = M.t = {foo: 'a} constraint 'a = int
end;;
let m = {M.foo= 15};;
let n = {N.foo= 15};;
let f = function | {M.foo= x} -> x;;
let g = function | {N.foo= x} -> x;;
let f_m = f m;;
let f_n = f n;;
let g_m = g m;;
let g_n = g n;;
[%%expect{|
module M : sig type t = { foo : int; } end
module N : sig type 'a t = M.t = { foo : 'a; } constraint 'a = int end
val m : M.t = {M.foo = 15}
val n : int N.t = {N.foo = 15}
val f : M.t -> int = <fun>
val g : int N.t -> int = <fun>
val f_m : int = 15
val f_n : int = 15
val g_m : int = 15
val g_n : int = 15
|}]
let n_bad = {N.foo= true};;
[%%expect{|
Line 1, characters 20-24:
1 | let n_bad = {N.foo= true};;
                        ^^^^
Error: This expression has type bool but an expression was expected of type
         int
|}]

(* Mixing type variables *)
module M = struct
  type ('a, 'b) t = {a: 'a; b: 'b}
end
module N = struct
  type ('a, 'b) t = ('a -> 'b, int) M.t = {a: ('a -> 'b); b: int}
end;;
let m = {M.a= (fun x -> (x, x)); b= 15};;
let n = {N.a= (fun x -> (x, x)); b= 15};;
let f = function {M.a; b} -> (a, b);;
let g = function {N.a; b} -> (a, b);;
let f_m = f m;;
let f_n = f n;;
let g_m = g m;;
let g_n = g n;;
[%%expect{|
module M : sig type ('a, 'b) t = { a : 'a; b : 'b; } end
module N :
  sig type ('a, 'b) t = ('a -> 'b, int) M.t = { a : 'a -> 'b; b : int; } end
val m : ('a -> 'a * 'a, int) M.t = {M.a = <fun>; b = 15}
val n : ('a, 'a * 'a) N.t = {N.a = <fun>; b = 15}
val f : ('a, 'b) M.t -> 'a * 'b = <fun>
val g : ('a, 'b) N.t -> ('a -> 'b) * int = <fun>
val f_m : ('_weak1 -> '_weak1 * '_weak1) * int = (<fun>, 15)
val f_n : ('_weak2 -> '_weak2 * '_weak2) * int = (<fun>, 15)
val g_m : ('_weak3 -> '_weak3 * '_weak3) * int = (<fun>, 15)
val g_n : ('_weak4 -> '_weak4 * '_weak4) * int = (<fun>, 15)
|}]
let m_bad = {M.a= 15; b= true};;
let g_m_bad = g m_bad;;
[%%expect{|
val m_bad : (int, bool) M.t = {M.a = 15; b = true}
Line 2, characters 16-21:
2 | let g_m_bad = g m_bad;;
                    ^^^^^
Error: This expression has type (int, bool) M.t
       but an expression was expected of type
         ('a, 'b) N.t = ('a -> 'b, int) M.t
       Type int is not compatible with type 'a -> 'b
|}]

(* Invalid mixing of type variables *)
module M = struct
  type ('a, 'b) t = {a: 'a; b: 'b}
end
module N = struct
  type ('a, 'b) t = ('a -> 'b, int) M.t = {a: 'a; b: bool}
end;;
[%%expect{|
module M : sig type ('a, 'b) t = { a : 'a; b : 'b; } end
Line 5, characters 2-58:
5 |   type ('a, 'b) t = ('a -> 'b, int) M.t = {a: 'a; b: bool}
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         ('a -> 'b, int) M.t
       1. Fields do not match:
         a : 'a -> 'b;
       is not the same as:
         a : 'a;
       The type 'a -> 'b is not equal to the type 'a
       2. Fields do not match:
         b : int;
       is not the same as:
         b : bool;
       The type int is not equal to the type bool
|}]

(* Mixing between aliases *)
module M = struct
  type ('a, 'b) t = {a: 'a; b: 'b}
end
module N = struct
  type ('a, 'b) t = ('a * 'b, bool) M.t = {a: ('a * 'b); b: bool}
end
module O = struct
  type 'c t = (int * unit, 'c) M.t = {a: (int * unit); b: 'c}
end
let m = {M.a= (1, ()); b= true};;
let n = {N.a= (1, ()); b= true};;
let o = {O.a= (1, ()); b= true};;
let f = function {M.a; b} -> (a, b);;
let g = function {N.a; b} -> (a, b);;
let h = function {O.a; b} -> (a, b);;
let f_m = f m;;
let f_n = f n;;
let f_o = f o;;
let g_m = g m;;
let g_n = g n;;
let g_o = g o;;
let h_m = h m;;
let h_n = h n;;
let h_o = h o;;
[%%expect{|
module M : sig type ('a, 'b) t = { a : 'a; b : 'b; } end
module N :
  sig type ('a, 'b) t = ('a * 'b, bool) M.t = { a : 'a * 'b; b : bool; } end
module O :
  sig type 'c t = (int * unit, 'c) M.t = { a : int * unit; b : 'c; } end
val m : (int * unit, bool) M.t = {M.a = (1, ()); b = true}
val n : (int, unit) N.t = {N.a = (1, ()); b = true}
val o : bool O.t = {O.a = (1, ()); b = true}
val f : ('a, 'b) M.t -> 'a * 'b = <fun>
val g : ('a, 'b) N.t -> ('a * 'b) * bool = <fun>
val h : 'a O.t -> (int * unit) * 'a = <fun>
val f_m : (int * unit) * bool = ((1, ()), true)
val f_n : (int * unit) * bool = ((1, ()), true)
val f_o : (int * unit) * bool = ((1, ()), true)
val g_m : (int * unit) * bool = ((1, ()), true)
val g_n : (int * unit) * bool = ((1, ()), true)
val g_o : (int * unit) * bool = ((1, ()), true)
val h_m : (int * unit) * bool = ((1, ()), true)
val h_n : (int * unit) * bool = ((1, ()), true)
val h_o : (int * unit) * bool = ((1, ()), true)
|}]

(* Mixing between patterns *)
let f_m_n = function {M.a; N.b} -> (a, b);;
let f_m_o = function {M.a; O.b} -> (a, b);;
let f_n_m = function {N.a; M.b} -> (a, b);;
let f_n_o = function {N.a; O.b} -> (a, b);;
let f_o_m = function {O.a; M.b} -> (a, b);;
let f_o_n = function {O.a; N.b} -> (a, b);;
[%%expect {|
val f_m_n : ('a, 'b) N.t -> ('a * 'b) * bool = <fun>
val f_m_o : 'a O.t -> (int * unit) * 'a = <fun>
val f_n_m : ('a, 'b) N.t -> ('a * 'b) * bool = <fun>
val f_n_o : (int, unit) N.t -> (int * unit) * bool = <fun>
val f_o_m : 'a O.t -> (int * unit) * 'a = <fun>
val f_o_n : bool O.t -> (int * unit) * bool = <fun>
|}]
