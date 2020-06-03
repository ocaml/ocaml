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
       is not compatible with:
         Foo of int * int
       The types are not equal.
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
       is not compatible with:
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
       is not compatible with:
         Foo of { x : int; y : int; }
       Fields do not match:
         x : float;
       is not compatible with:
         x : int;
       The types are not equal.
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
       is not compatible with:
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
       is not compatible with:
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
       is not compatible with:
         A of 'a
       The types are not equal.
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
       is not compatible with:
         A of 'a
       The types are not equal.
|}];;
