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
       6. A field, e, is missing in the first declaration.
       9. An extra field, phi, is provided in the first declaration.
|}]
