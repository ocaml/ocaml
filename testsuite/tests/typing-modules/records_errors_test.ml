(* TEST
 * expect
*)

module M1 : sig
  type t = {f0 : unit * unit * unit * int * unit * unit * unit;
            f1 : unit * unit * unit * int * unit * unit * unit}
end = struct
  type t = {f0 : unit * unit * unit * float* unit * unit * unit;
            f1 : unit * unit * unit * string * unit * unit * unit}
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = {f0 : unit * unit * unit * float* unit * unit * unit;
6 |             f1 : unit * unit * unit * string * unit * unit * unit}
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = {
             f0 : unit * unit * unit * float * unit * unit * unit;
             f1 : unit * unit * unit * string * unit * unit * unit;
           }
         end
       is not included in
         sig
           type t = {
             f0 : unit * unit * unit * int * unit * unit * unit;
             f1 : unit * unit * unit * int * unit * unit * unit;
           }
         end
       Type declarations do not match:
         type t = {
           f0 : unit * unit * unit * float * unit * unit * unit;
           f1 : unit * unit * unit * string * unit * unit * unit;
         }
       is not included in
         type t = {
           f0 : unit * unit * unit * int * unit * unit * unit;
           f1 : unit * unit * unit * int * unit * unit * unit;
         }
       1. Fields do not match:
         f0 : unit * unit * unit * float * unit * unit * unit;
       is not the same as:
         f0 : unit * unit * unit * int * unit * unit * unit;
       The type unit * unit * unit * float * unit * unit * unit
       is not equal to the type unit * unit * unit * int * unit * unit * unit
       Type float is not equal to type int
       2. Fields do not match:
         f1 : unit * unit * unit * string * unit * unit * unit;
       is not the same as:
         f1 : unit * unit * unit * int * unit * unit * unit;
       The type unit * unit * unit * string * unit * unit * unit
       is not equal to the type unit * unit * unit * int * unit * unit * unit
       Type string is not equal to type int
|}];;


module M2 : sig
  type t = {mutable f0 : unit * unit * unit * int * unit * unit * unit;
            f1 : unit * unit * unit * int * unit * unit * unit}
end = struct
  type t = {f0 : unit * unit * unit * float* unit * unit * unit;
            f1 : unit * unit * unit * string * unit * unit * unit}
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = {f0 : unit * unit * unit * float* unit * unit * unit;
6 |             f1 : unit * unit * unit * string * unit * unit * unit}
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = {
             f0 : unit * unit * unit * float * unit * unit * unit;
             f1 : unit * unit * unit * string * unit * unit * unit;
           }
         end
       is not included in
         sig
           type t = {
             mutable f0 : unit * unit * unit * int * unit * unit * unit;
             f1 : unit * unit * unit * int * unit * unit * unit;
           }
         end
       Type declarations do not match:
         type t = {
           f0 : unit * unit * unit * float * unit * unit * unit;
           f1 : unit * unit * unit * string * unit * unit * unit;
         }
       is not included in
         type t = {
           mutable f0 : unit * unit * unit * int * unit * unit * unit;
           f1 : unit * unit * unit * int * unit * unit * unit;
         }
       1. Fields do not match:
         f0 : unit * unit * unit * float * unit * unit * unit;
       is not the same as:
         mutable f0 : unit * unit * unit * int * unit * unit * unit;
       The second is mutable and the first is not.
       2. Fields do not match:
         f1 : unit * unit * unit * string * unit * unit * unit;
       is not the same as:
         f1 : unit * unit * unit * int * unit * unit * unit;
       The type unit * unit * unit * string * unit * unit * unit
       is not equal to the type unit * unit * unit * int * unit * unit * unit
       Type string is not equal to type int
|}];;

module M3 : sig
  type t = {f0 : unit}
end = struct
  type t = {f1 : unit}
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = {f1 : unit}
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f1 : unit; } end
       is not included in
         sig type t = { f0 : unit; } end
       Type declarations do not match:
         type t = { f1 : unit; }
       is not included in
         type t = { f0 : unit; }
       Fields have different names, f1 and f0.
|}];;

module M4 : sig
  type t = {f0 : unit; f1 : unit}
end = struct
  type t = {f0 : unit}
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = {f0 : unit}
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f0 : unit; } end
       is not included in
         sig type t = { f0 : unit; f1 : unit; } end
       Type declarations do not match:
         type t = { f0 : unit; }
       is not included in
         type t = { f0 : unit; f1 : unit; }
       A field, f1, is missing in the first declaration.
|}];;


(** Random additions and deletions of fields *)

module Addition : sig
  type t = {a : unit; b : unit; c : unit; d : unit}
end = struct
  type t = {a : unit; b : unit; beta : unit; c : unit; d: unit}
end
[%%expect {|
Lines 5-7, characters 6-3:
5 | ......struct
6 |   type t = {a : unit; b : unit; beta : unit; c : unit; d: unit}
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = { a : unit; b : unit; beta : unit; c : unit; d : unit; }
         end
       is not included in
         sig type t = { a : unit; b : unit; c : unit; d : unit; } end
       Type declarations do not match:
         type t = { a : unit; b : unit; beta : unit; c : unit; d : unit; }
       is not included in
         type t = { a : unit; b : unit; c : unit; d : unit; }
       An extra field, beta, is provided in the first declaration.
|}]


module Deletion : sig
  type t = {a : unit; b : unit; c : unit; d : unit}
end = struct
  type t = {a : unit; c : unit; d : unit}
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = {a : unit; c : unit; d : unit}
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { a : unit; c : unit; d : unit; } end
       is not included in
         sig type t = { a : unit; b : unit; c : unit; d : unit; } end
       Type declarations do not match:
         type t = { a : unit; c : unit; d : unit; }
       is not included in
         type t = { a : unit; b : unit; c : unit; d : unit; }
       A field, b, is missing in the first declaration.
|}]


module Multi: sig
  type t = {
    a : unit;
    b : unit;
    c : unit;
    d : unit;
    e : unit;
    f : unit;
    g : unit
  }
end = struct
  type t = {
    a : unit;
    b : unit;
    beta: int;
    c : unit;
    d : unit;
    f : unit;
    g : unit;
    phi : unit;
  }
end

[%%expect {|
Lines 11-22, characters 6-3:
11 | ......struct
12 |   type t = {
13 |     a : unit;
14 |     b : unit;
15 |     beta: int;
...
19 |     g : unit;
20 |     phi : unit;
21 |   }
22 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = {
             a : unit;
             b : unit;
             beta : int;
             c : unit;
             d : unit;
             f : unit;
             g : unit;
             phi : unit;
           }
         end
       is not included in
         sig
           type t = {
             a : unit;
             b : unit;
             c : unit;
             d : unit;
             e : unit;
             f : unit;
             g : unit;
           }
         end
       Type declarations do not match:
         type t = {
           a : unit;
           b : unit;
           beta : int;
           c : unit;
           d : unit;
           f : unit;
           g : unit;
           phi : unit;
         }
       is not included in
         type t = {
           a : unit;
           b : unit;
           c : unit;
           d : unit;
           e : unit;
           f : unit;
           g : unit;
         }
       3. An extra field, beta, is provided in the first declaration.
       5. A field, e, is missing in the first declaration.
       8. An extra field, phi, is provided in the first declaration.
|}]


(** Multiple errors *)

module M : sig
  type t = { a:int; e:int; c:int; d:int; b:int }
end = struct
  type t = { alpha:int; b:int; c:int; d:int; e:int }
end
[%%expect {|
Lines 5-7, characters 6-3:
5 | ......struct
6 |   type t = { alpha:int; b:int; c:int; d:int; e:int }
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = { alpha : int; b : int; c : int; d : int; e : int; }
         end
       is not included in
         sig type t = { a : int; e : int; c : int; d : int; b : int; } end
       Type declarations do not match:
         type t = { alpha : int; b : int; c : int; d : int; e : int; }
       is not included in
         type t = { a : int; e : int; c : int; d : int; b : int; }
       1. Fields have different names, alpha and a.
       2<->5. Fields b and e have been swapped.
|}]


module M: sig
  type t = { a:int; b:int; c:int; d:int; e:int; f:float }
end =
struct
  type t = { b:int; c:int; d:int; e:int; a:int; f:int }
end
[%%expect {|
Lines 4-6, characters 0-3:
4 | struct
5 |   type t = { b:int; c:int; d:int; e:int; a:int; f:int }
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = { b : int; c : int; d : int; e : int; a : int; f : int; }
         end
       is not included in
         sig
           type t = {
             a : int;
             b : int;
             c : int;
             d : int;
             e : int;
             f : float;
           }
         end
       Type declarations do not match:
         type t = { b : int; c : int; d : int; e : int; a : int; f : int; }
       is not included in
         type t = { a : int; b : int; c : int; d : int; e : int; f : float; }
       1->5. Field a has been moved from position 1 to 5.
       6. Fields do not match:
         f : int;
       is not the same as:
         f : float;
       The type int is not equal to the type float
|}]

(** Existential types introduce equations that must be taken in account
    when diffing
*)


module Eq : sig
  type t = A: { a:'a; b:'b; x:'a } -> t
end = struct
  type t = A: { a:'a; b:'b; x:'x } -> t
end
[%%expect {|
Lines 8-10, characters 6-3:
 8 | ......struct
 9 |   type t = A: { a:'a; b:'b; x:'x } -> t
10 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A : { a : 'a; b : 'b; x : 'x; } -> t end
       is not included in
         sig type t = A : { a : 'a; b : 'b; x : 'a; } -> t end
       Type declarations do not match:
         type t = A : { a : 'a; b : 'b; x : 'x; } -> t
       is not included in
         type t = A : { a : 'a; b : 'b; x : 'a; } -> t
       Constructors do not match:
         A : { a : 'a; b : 'b; x : 'x; } -> t
       is not the same as:
         A : { a : 'a; b : 'b; x : 'a; } -> t
       Fields do not match:
         x : 'x;
       is not the same as:
         x : 'a;
       The type 'x is not equal to the type 'a
|}]


module Not_a_swap: sig
  type t = A: { x:'a; a:'a; b:'b; y:'b} -> t
end = struct
  type t = A: { y:'a; a:'a; b:'b; x:'b} -> t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A: { y:'a; a:'a; b:'b; x:'b} -> t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A : { y : 'a; a : 'a; b : 'b; x : 'b; } -> t end
       is not included in
         sig type t = A : { x : 'a; a : 'a; b : 'b; y : 'b; } -> t end
       Type declarations do not match:
         type t = A : { y : 'a; a : 'a; b : 'b; x : 'b; } -> t
       is not included in
         type t = A : { x : 'a; a : 'a; b : 'b; y : 'b; } -> t
       Constructors do not match:
         A : { y : 'a; a : 'a; b : 'b; x : 'b; } -> t
       is not the same as:
         A : { x : 'a; a : 'a; b : 'b; y : 'b; } -> t
       1. Fields have different names, y and x.
       4. Fields have different names, x and y.
|}]

module Swap: sig
  type t = A: { x:'a; a:'a; b:'b; y:'b} -> t
end = struct
  type t = A: { y:'b; a:'a; b:'b; x:'a} -> t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A: { y:'b; a:'a; b:'b; x:'a} -> t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A : { y : 'b; a : 'a; b : 'b; x : 'a; } -> t end
       is not included in
         sig type t = A : { x : 'a; a : 'a; b : 'b; y : 'b; } -> t end
       Type declarations do not match:
         type t = A : { y : 'b; a : 'a; b : 'b; x : 'a; } -> t
       is not included in
         type t = A : { x : 'a; a : 'a; b : 'b; y : 'b; } -> t
       Constructors do not match:
         A : { y : 'b; a : 'a; b : 'b; x : 'a; } -> t
       is not the same as:
         A : { x : 'a; a : 'a; b : 'b; y : 'b; } -> t
       Fields x and y have been swapped.
|}]


module Not_a_move: sig
  type t = A: { a:'a; b:'b; x:'b} -> t
end = struct
  type t = A: { x:'a; a:'a; b:'b} -> t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A: { x:'a; a:'a; b:'b} -> t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A : { x : 'a; a : 'a; b : 'b; } -> t end
       is not included in
         sig type t = A : { a : 'a; b : 'b; x : 'b; } -> t end
       Type declarations do not match:
         type t = A : { x : 'a; a : 'a; b : 'b; } -> t
       is not included in
         type t = A : { a : 'a; b : 'b; x : 'b; } -> t
       Constructors do not match:
         A : { x : 'a; a : 'a; b : 'b; } -> t
       is not the same as:
         A : { a : 'a; b : 'b; x : 'b; } -> t
       1. An extra field, x, is provided in the first declaration.
       3. A field, x, is missing in the first declaration.
|}]


module Move: sig
  type t = A: { a:'a; b:'b; x:'b} -> t
end = struct
  type t = A: { x:'b; a:'a; b:'b} -> t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A: { x:'b; a:'a; b:'b} -> t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A : { x : 'b; a : 'a; b : 'b; } -> t end
       is not included in
         sig type t = A : { a : 'a; b : 'b; x : 'b; } -> t end
       Type declarations do not match:
         type t = A : { x : 'b; a : 'a; b : 'b; } -> t
       is not included in
         type t = A : { a : 'a; b : 'b; x : 'b; } -> t
       Constructors do not match:
         A : { x : 'b; a : 'a; b : 'b; } -> t
       is not the same as:
         A : { a : 'a; b : 'b; x : 'b; } -> t
       Field x has been moved from position 3 to 1.
|}]
