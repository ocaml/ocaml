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
       The types for field Foo are not equal.
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
       The arities for field Foo differ.
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
       The types for field x are not equal.
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
       The types for field Foo are not equal.
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
       The types for field Foo are not equal.
|}];;
