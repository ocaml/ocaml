(* TEST
  * expect
 *)

module M1 : sig
  type t =
    | Foo of int * int
end = struct
  type t =
    | Foo of float * int
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of float * int
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of float * int end
       is not included in
         sig type t = Foo of int * int end
       Type declarations do not match:
         type t = Foo of float * int
       is not included in
         type t = Foo of int * int
       Constructors do not match:
         Foo of float * int
       is not the same as:
         Foo of int * int
       The type float is not equal to the type int
|}];;

module M2 : sig
  type t =
    | Foo of int * int
end = struct
  type t =
    | Foo of float
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of float
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of float end
       is not included in
         sig type t = Foo of int * int end
       Type declarations do not match:
         type t = Foo of float
       is not included in
         type t = Foo of int * int
       Constructors do not match:
         Foo of float
       is not the same as:
         Foo of int * int
       They have different arities.
|}];;

module M3 : sig
  type t =
    | Foo of {x : int; y : int}
end = struct
  type t =
    | Foo of {x : float; y : int}
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of {x : float; y : int}
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of { x : float; y : int; } end
       is not included in
         sig type t = Foo of { x : int; y : int; } end
       Type declarations do not match:
         type t = Foo of { x : float; y : int; }
       is not included in
         type t = Foo of { x : int; y : int; }
       Constructors do not match:
         Foo of { x : float; y : int; }
       is not the same as:
         Foo of { x : int; y : int; }
       Fields do not match:
         x : float;
       is not the same as:
         x : int;
       The type float is not equal to the type int
|}];;

module M4 : sig
  type t =
    | Foo of {x : int; y : int}
end = struct
  type t =
    | Foo of float
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of float
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of float end
       is not included in
         sig type t = Foo of { x : int; y : int; } end
       Type declarations do not match:
         type t = Foo of float
       is not included in
         type t = Foo of { x : int; y : int; }
       Constructors do not match:
         Foo of float
       is not the same as:
         Foo of { x : int; y : int; }
       The second uses inline records and the first doesn't.
|}];;

module M5 : sig
  type 'a t =
    | Foo : int -> int t
end = struct
  type 'a t =
    | Foo of 'a
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type 'a t =
6 |     | Foo of 'a
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = Foo of 'a end
       is not included in
         sig type 'a t = Foo : int -> int t end
       Type declarations do not match:
         type 'a t = Foo of 'a
       is not included in
         type 'a t = Foo : int -> int t
       Constructors do not match:
         Foo of 'a
       is not the same as:
         Foo : int -> int t
       The second has explicit return type and the first doesn't.
|}];;

module M : sig
  type ('a, 'b) t = A of 'a
end = struct
  type ('a, 'b) t = A of 'b
end;;
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t = A of 'b
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t = A of 'b end
       is not included in
         sig type ('a, 'b) t = A of 'a end
       Type declarations do not match:
         type ('a, 'b) t = A of 'b
       is not included in
         type ('a, 'b) t = A of 'a
       Constructors do not match:
         A of 'b
       is not the same as:
         A of 'a
       The type 'b is not equal to the type 'a
|}];;

module M : sig
  type ('a, 'b) t = A of 'a
end = struct
  type ('b, 'a) t = A of 'a
end;;
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('b, 'a) t = A of 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type ('b, 'a) t = A of 'a end
       is not included in
         sig type ('a, 'b) t = A of 'a end
       Type declarations do not match:
         type ('b, 'a) t = A of 'a
       is not included in
         type ('a, 'b) t = A of 'a
       Constructors do not match:
         A of 'a
       is not the same as:
         A of 'a
       The type 'a is not equal to the type 'b
|}];;



(** Random additions and deletions of constructors *)

module Addition : sig
  type t =
    | A
    | B
    | C
    | D
end = struct
  type t =
    | A
    | B
    | Beta
    | C
    | D
end
[%%expect {|
Lines 9-16, characters 6-3:
 9 | ......struct
10 |   type t =
11 |     | A
12 |     | B
13 |     | Beta
14 |     | C
15 |     | D
16 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A | B | Beta | C | D end
       is not included in
         sig type t = A | B | C | D end
       Type declarations do not match:
         type t = A | B | Beta | C | D
       is not included in
         type t = A | B | C | D
       An extra constructor, Beta, is provided in the first declaration.
|}]


module Addition : sig
  type t =
    | A
    | B
    | C
    | D
end = struct
  type t =
    | A
    | B
    | D
end
[%%expect {|
Lines 7-12, characters 6-3:
 7 | ......struct
 8 |   type t =
 9 |     | A
10 |     | B
11 |     | D
12 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A | B | D end
       is not included in
         sig type t = A | B | C | D end
       Type declarations do not match:
         type t = A | B | D
       is not included in
         type t = A | B | C | D
       A constructor, C, is missing in the first declaration.
|}]


module Multi: sig
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
    | G
end = struct
  type t =
    | A
    | B
    | Beta
    | C
    | D
    | F
    | G
    | Phi
end

[%%expect {|
Lines 10-20, characters 6-3:
10 | ......struct
11 |   type t =
12 |     | A
13 |     | B
14 |     | Beta
...
17 |     | F
18 |     | G
19 |     | Phi
20 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A | B | Beta | C | D | F | G | Phi end
       is not included in
         sig type t = A | B | C | D | E | F | G end
       Type declarations do not match:
         type t = A | B | Beta | C | D | F | G | Phi
       is not included in
         type t = A | B | C | D | E | F | G
       3. An extra constructor, Beta, is provided in the first declaration.
       5. A constructor, E, is missing in the first declaration.
       8. An extra constructor, Phi, is provided in the first declaration.
|}]


(** Swaps and moves *)

module Swap : sig
  type t =
    | A
    | E
    | C
    | D
    | B
end = struct
  type t =
    | Alpha
    | B
    | C
    | D
    | E
end
[%%expect {|
Lines 10-17, characters 6-3:
10 | ......struct
11 |   type t =
12 |     | Alpha
13 |     | B
14 |     | C
15 |     | D
16 |     | E
17 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Alpha | B | C | D | E end
       is not included in
         sig type t = A | E | C | D | B end
       Type declarations do not match:
         type t = Alpha | B | C | D | E
       is not included in
         type t = A | E | C | D | B
       1. Constructors have different names, Alpha and A.
       2<->5. Constructors B and E have been swapped.
|}]


module Move: sig
  type t =
    | A of int
    | B
    | C
    | D
    | E
    | F
end = struct
  type t =
    | A of float
    | B
    | D
    | E
    | F
    | C
end
[%%expect {|
Lines 9-17, characters 6-3:
 9 | ......struct
10 |   type t =
11 |     | A of float
12 |     | B
13 |     | D
14 |     | E
15 |     | F
16 |     | C
17 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of float | B | D | E | F | C end
       is not included in
         sig type t = A of int | B | C | D | E | F end
       Type declarations do not match:
         type t = A of float | B | D | E | F | C
       is not included in
         type t = A of int | B | C | D | E | F
       1. Constructors do not match:
         A of float
       is not the same as:
         A of int
       The type float is not equal to the type int
       3->6. Constructor C has been moved from position 3 to 6.
|}]
