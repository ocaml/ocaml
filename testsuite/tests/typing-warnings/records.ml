(* TEST
 flags = " -w +A -strict-sequence ";
 expect;
*)

(* Use type information *)
module M1 = struct
  type t = {x: int; y: int}
  type u = {x: bool; y: bool}
end;;
[%%expect{|
module M1 :
  sig type t = { x : int; y : int; } type u = { x : bool; y : bool; } end
|}]

module OK = struct
  open M1
  let f1 (r:t) = r.x (* ok *)
  let f2 r = ignore (r:t); r.x (* non principal *)

  let f3 (r: t) =
    match r with {x; y} -> y + y (* ok *)
end;;
[%%expect{|
Line 3, characters 19-20:
3 |   let f1 (r:t) = r.x (* ok *)
                       ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 4, characters 29-30:
4 |   let f2 r = ignore (r:t); r.x (* non principal *)
                                 ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 7, characters 18-19:
7 |     match r with {x; y} -> y + y (* ok *)
                      ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 7, characters 21-22:
7 |     match r with {x; y} -> y + y (* ok *)
                         ^
Warning 42 [disambiguated-name]: this use of "y" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 7, characters 18-19:
7 |     match r with {x; y} -> y + y (* ok *)
                      ^
Warning 27 [unused-var-strict]: unused variable "x".

module OK :
  sig val f1 : M1.t -> int val f2 : M1.t -> int val f3 : M1.t -> int end
|}, Principal{|
Line 3, characters 19-20:
3 |   let f1 (r:t) = r.x (* ok *)
                       ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 4, characters 29-30:
4 |   let f2 r = ignore (r:t); r.x (* non principal *)
                                 ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

Line 4, characters 29-30:
4 |   let f2 r = ignore (r:t); r.x (* non principal *)
                                 ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 7, characters 18-19:
7 |     match r with {x; y} -> y + y (* ok *)
                      ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 7, characters 21-22:
7 |     match r with {x; y} -> y + y (* ok *)
                         ^
Warning 42 [disambiguated-name]: this use of "y" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 7, characters 18-19:
7 |     match r with {x; y} -> y + y (* ok *)
                      ^
Warning 27 [unused-var-strict]: unused variable "x".

module OK :
  sig val f1 : M1.t -> int val f2 : M1.t -> int val f3 : M1.t -> int end
|}]

module F1 = struct
  open M1
  let f r = match r with {x; y} -> y + y
end;; (* fails *)
[%%expect{|
Line 3, characters 25-31:
3 |   let f r = match r with {x; y} -> y + y
                             ^^^^^^
Warning 41 [ambiguous-name]: these field labels belong to several types:
    "M1.u" "M1.t".
  The first one was selected. Please disambiguate if this is wrong.

Line 3, characters 35-36:
3 |   let f r = match r with {x; y} -> y + y
                                       ^
Error: The value "y" has type "bool" but an expression was expected of type "int"
|}]

module F2 = struct
  open M1
  let f r =
    ignore (r: t);
    match r with
       {x; y} -> y + y
end;; (* fails for -principal *)
[%%expect{|
Line 6, characters 8-9:
6 |        {x; y} -> y + y
            ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 6, characters 11-12:
6 |        {x; y} -> y + y
               ^
Warning 42 [disambiguated-name]: this use of "y" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 6, characters 8-9:
6 |        {x; y} -> y + y
            ^
Warning 27 [unused-var-strict]: unused variable "x".

module F2 : sig val f : M1.t -> int end
|}, Principal{|
Line 6, characters 8-9:
6 |        {x; y} -> y + y
            ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 6, characters 11-12:
6 |        {x; y} -> y + y
               ^
Warning 42 [disambiguated-name]: this use of "y" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 6, characters 7-13:
6 |        {x; y} -> y + y
           ^^^^^^
Warning 18 [not-principal]: this type-based record disambiguation is not
  principal.

Line 6, characters 8-9:
6 |        {x; y} -> y + y
            ^
Warning 27 [unused-var-strict]: unused variable "x".

module F2 : sig val f : M1.t -> int end
|}]

(* Use type information with modules*)
module M = struct
  type t = {x:int}
  type u = {x:bool}
end;;
[%%expect{|
module M : sig type t = { x : int; } type u = { x : bool; } end
|}]
let f (r:M.t) = r.M.x;; (* ok *)
[%%expect{|
Line 1, characters 18-21:
1 | let f (r:M.t) = r.M.x;; (* ok *)
                      ^^^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

val f : M.t -> int = <fun>
|}]
let f (r:M.t) = r.x;; (* warning *)
[%%expect{|
Line 1, characters 18-19:
1 | let f (r:M.t) = r.x;; (* warning *)
                      ^
Warning 40 [name-out-of-scope]: "x" was selected from type "M.t".
  It is not visible in the current scope, and will not be selected
  if the type becomes unknown.

Line 1, characters 18-19:
1 | let f (r:M.t) = r.x;; (* warning *)
                      ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

val f : M.t -> int = <fun>
|}]
let f ({x}:M.t) = x;; (* warning *)
[%%expect{|
Line 1, characters 8-9:
1 | let f ({x}:M.t) = x;; (* warning *)
            ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 1, characters 7-10:
1 | let f ({x}:M.t) = x;; (* warning *)
           ^^^
Warning 40 [name-out-of-scope]: this record of type "M.t" contains fields that
  are not visible in the current scope: "x".
  They will not be selected if the type becomes unknown.

val f : M.t -> int = <fun>
|}]

module M = struct
  type t = {x: int; y: int}
end;;
[%%expect{|
module M : sig type t = { x : int; y : int; } end
|}]
module N = struct
  type u = {x: bool; y: bool}
end;;
[%%expect{|
module N : sig type u = { x : bool; y : bool; } end
|}]
module OK = struct
  open M
  open N
  let f (r:M.t) = r.x
end;;
[%%expect{|
Line 4, characters 20-21:
4 |   let f (r:M.t) = r.x
                        ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 3, characters 2-8:
3 |   open N
      ^^^^^^
Warning 33 [unused-open]: unused open "N".

module OK : sig val f : M.t -> int end
|}]

module M = struct
  type t = {x:int}
  module N = struct type s = t = {x:int} end
  type u = {x:bool}
end;;
[%%expect{|
module M :
  sig
    type t = { x : int; }
    module N : sig type s = t = { x : int; } end
    type u = { x : bool; }
  end
|}]
module OK = struct
  open M.N
  let f (r:M.t) = r.x
end;;
[%%expect{|
module OK : sig val f : M.t -> int end
|}]

(* Use field information *)
module M = struct
  type u = {x:bool;y:int;z:char}
  type t = {x:int;y:bool}
end;;
[%%expect{|
module M :
  sig
    type u = { x : bool; y : int; z : char; }
    type t = { x : int; y : bool; }
  end
|}]
module OK = struct
  open M
  let f {x;z} = x,z
end;; (* ok *)
[%%expect{|
Line 3, characters 9-10:
3 |   let f {x;z} = x,z
             ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 3, characters 8-13:
3 |   let f {x;z} = x,z
            ^^^^^
Warning 9 [missing-record-field-pattern]: the following labels are not bound
  in this record pattern: "y".
  Either bind these labels explicitly or add "; _" to the pattern.

module OK : sig val f : M.u -> bool * char end
|}]
module F3 = struct
  open M
  let r = {x=true;z='z'}
end;; (* fail for missing label *)
[%%expect{|
Line 3, characters 11-12:
3 |   let r = {x=true;z='z'}
               ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 3, characters 10-24:
3 |   let r = {x=true;z='z'}
              ^^^^^^^^^^^^^^
Error: Some record fields are undefined: "y"
|}]

module OK = struct
  type u = {x:int;y:bool}
  type t = {x:bool;y:int;z:char}
  let r = {x=3; y=true}
end;; (* ok *)
[%%expect{|
Line 4, characters 11-12:
4 |   let r = {x=3; y=true}
               ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 4, characters 16-17:
4 |   let r = {x=3; y=true}
                    ^
Warning 42 [disambiguated-name]: this use of "y" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

module OK :
  sig
    type u = { x : int; y : bool; }
    type t = { x : bool; y : int; z : char; }
    val r : u
  end
|}]

(* Corner cases *)

module F4 = struct
  type foo = {x:int; y:int}
  type bar = {x:int}
  let b : bar = {x=3; y=4}
end;; (* fail but don't warn *)
[%%expect{|
Line 4, characters 22-23:
4 |   let b : bar = {x=3; y=4}
                          ^
Error: This record expression is expected to have type "bar"
       There is no field "y" within type "bar"
|}]

module M = struct type foo = {x:int;y:int} end;;
[%%expect{|
module M : sig type foo = { x : int; y : int; } end
|}]
module N = struct type bar = {x:int;y:int} end;;
[%%expect{|
module N : sig type bar = { x : int; y : int; } end
|}]
let r = { M.x = 3; N.y = 4; };; (* error: different definitions *)
[%%expect{|
Line 1, characters 19-22:
1 | let r = { M.x = 3; N.y = 4; };; (* error: different definitions *)
                       ^^^
Error: The record field "N.y" belongs to the type "N.bar"
       but is mixed here with fields of type "M.foo"
|}]

module MN = struct include M include N end
module NM = struct include N include M end;;
[%%expect{|
module MN :
  sig
    type foo = M.foo = { x : int; y : int; }
    type bar = N.bar = { x : int; y : int; }
  end
module NM :
  sig
    type bar = N.bar = { x : int; y : int; }
    type foo = M.foo = { x : int; y : int; }
  end
|}]
let r = {MN.x = 3; NM.y = 4};; (* error: type would change with order *)
[%%expect{|
Line 1, characters 8-28:
1 | let r = {MN.x = 3; NM.y = 4};; (* error: type would change with order *)
            ^^^^^^^^^^^^^^^^^^^^
Warning 41 [ambiguous-name]: "x" belongs to several types: "MN.bar" "MN.foo".
  The first one was selected. Please disambiguate if this is wrong.

Line 1, characters 8-28:
1 | let r = {MN.x = 3; NM.y = 4};; (* error: type would change with order *)
            ^^^^^^^^^^^^^^^^^^^^
Warning 41 [ambiguous-name]: "y" belongs to several types: "NM.foo" "NM.bar".
  The first one was selected. Please disambiguate if this is wrong.

Line 1, characters 19-23:
1 | let r = {MN.x = 3; NM.y = 4};; (* error: type would change with order *)
                       ^^^^
Error: The record field "NM.y" belongs to the type "NM.foo" = "M.foo"
       but is mixed here with fields of type "MN.bar" = "N.bar"
|}]

(* Lpw25 *)

module M = struct
  type foo = { x: int; y: int }
  type bar = { x:int; y: int; z: int}
end;;
[%%expect{|
module M :
  sig
    type foo = { x : int; y : int; }
    type bar = { x : int; y : int; z : int; }
  end
|}]
module F5 = struct
  open M
  let f r = ignore (r: foo); {r with x = 2; z = 3}
end;;
[%%expect{|
Line 3, characters 37-38:
3 |   let f r = ignore (r: foo); {r with x = 2; z = 3}
                                         ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 3, characters 44-45:
3 |   let f r = ignore (r: foo); {r with x = 2; z = 3}
                                                ^
Error: This record expression is expected to have type "M.foo"
       There is no field "z" within type "M.foo"
|}]
module M = struct
  include M
  type other = { a: int; b: int }
end;;
[%%expect{|
module M :
  sig
    type foo = M.foo = { x : int; y : int; }
    type bar = M.bar = { x : int; y : int; z : int; }
    type other = { a : int; b : int; }
  end
|}]
module F6 = struct
  open M
  let f r = ignore (r: foo); { r with x = 3; a = 4 }
end;;
[%%expect{|
Line 3, characters 38-39:
3 |   let f r = ignore (r: foo); { r with x = 3; a = 4 }
                                          ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 3, characters 45-46:
3 |   let f r = ignore (r: foo); { r with x = 3; a = 4 }
                                                 ^
Error: This record expression is expected to have type "M.foo"
       There is no field "a" within type "M.foo"
|}]
module F7 = struct
  open M
  let r = {x=1; y=2}
  let r: other = {x=1; y=2}
end;;
[%%expect{|
Line 3, characters 11-12:
3 |   let r = {x=1; y=2}
               ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 3, characters 16-17:
3 |   let r = {x=1; y=2}
                    ^
Warning 42 [disambiguated-name]: this use of "y" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 4, characters 18-19:
4 |   let r: other = {x=1; y=2}
                      ^
Error: This record expression is expected to have type "M.other"
       There is no field "x" within type "M.other"
|}]

module A = struct type t = {x: int} end
module B = struct type t = {x: int} end;;
[%%expect{|
module A : sig type t = { x : int; } end
module B : sig type t = { x : int; } end
|}]
let f (r : B.t) = r.A.x;; (* fail *)
[%%expect{|
Line 1, characters 20-23:
1 | let f (r : B.t) = r.A.x;; (* fail *)
                        ^^^
Error: The field "A.x" belongs to the record type "A.t"
       but a field was expected belonging to the record type "B.t"
|}]

(* Spellchecking *)

module F8 = struct
  type t = {x:int; yyy:int}
  let a : t = {x=1;yyz=2}
end;;
[%%expect{|
Line 3, characters 19-22:
3 |   let a : t = {x=1;yyz=2}
                       ^^^
Error: This record expression is expected to have type "t"
       There is no field "yyz" within type "t"
Hint: Did you mean "yyy"?
|}]

(* PR#6004 *)

type t = A
type s = A

class f (_ : t) = object end;;
[%%expect{|
type t = A
type s = A
class f : t -> object  end
|}]
class g = f A;; (* ok *)

class f (_ : 'a) (_ : 'a) = object end;;
[%%expect{|
Line 1, characters 12-13:
1 | class g = f A;; (* ok *)
                ^
Warning 42 [disambiguated-name]: this use of "A" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

class g : f
class f : 'a -> 'a -> object  end
|}]
class g = f (A : t) A;; (* warn with -principal *)
[%%expect{|
Line 1, characters 13-14:
1 | class g = f (A : t) A;; (* warn with -principal *)
                 ^
Warning 42 [disambiguated-name]: this use of "A" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 1, characters 20-21:
1 | class g = f (A : t) A;; (* warn with -principal *)
                        ^
Warning 42 [disambiguated-name]: this use of "A" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

class g : f
|}, Principal{|
Line 1, characters 13-14:
1 | class g = f (A : t) A;; (* warn with -principal *)
                 ^
Warning 42 [disambiguated-name]: this use of "A" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 1, characters 20-21:
1 | class g = f (A : t) A;; (* warn with -principal *)
                        ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

Line 1, characters 20-21:
1 | class g = f (A : t) A;; (* warn with -principal *)
                        ^
Warning 42 [disambiguated-name]: this use of "A" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

class g : f
|}]


(* PR#5980 *)

module Shadow1 = struct
  type t = {x: int}
  module M = struct
    type s = {x: string}
  end
  open M  (* this open is unused, it isn't reported as shadowing 'x' *)
  let y : t = {x = 0}
end;;
[%%expect{|
Line 7, characters 15-16:
7 |   let y : t = {x = 0}
                   ^
Warning 42 [disambiguated-name]: this use of "x" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 6, characters 2-8:
6 |   open M  (* this open is unused, it isn't reported as shadowing 'x' *)
      ^^^^^^
Warning 33 [unused-open]: unused open "M".

module Shadow1 :
  sig
    type t = { x : int; }
    module M : sig type s = { x : string; } end
    val y : t
  end
|}]
module Shadow2 = struct
  type t = {x: int}
  module M = struct
    type s = {x: string}
  end
  open M  (* this open shadows label 'x' *)
  let y = {x = ""}
end;;
[%%expect{|
Line 6, characters 2-8:
6 |   open M  (* this open shadows label 'x' *)
      ^^^^^^
Warning 45 [open-shadow-label-constructor]: this open statement shadows the
  label "x" (which is later used)

Line 7, characters 10-18:
7 |   let y = {x = ""}
              ^^^^^^^^
Warning 41 [ambiguous-name]: these field labels belong to several types:
    "M.s" "t".
  The first one was selected. Please disambiguate if this is wrong.

module Shadow2 :
  sig
    type t = { x : int; }
    module M : sig type s = { x : string; } end
    val y : M.s
  end
|}]

(* PR#6235 *)

module P6235 = struct
  type t = { loc : string; }
  type v = { loc : string; x : int; }
  type u = [ `Key of t ]
  let f (u : u) = match u with `Key {loc} -> loc
end;;
[%%expect{|
Line 5, characters 37-40:
5 |   let f (u : u) = match u with `Key {loc} -> loc
                                         ^^^
Warning 42 [disambiguated-name]: this use of "loc" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

module P6235 :
  sig
    type t = { loc : string; }
    type v = { loc : string; x : int; }
    type u = [ `Key of t ]
    val f : u -> string
  end
|}]

(* Remove interaction between branches *)

module P6235' = struct
  type t = { loc : string; }
  type v = { loc : string; x : int; }
  type u = [ `Key of t ]
  let f = function
    | (_ : u) when false -> ""
    |`Key {loc} -> loc
end;;
[%%expect{|
Line 7, characters 11-14:
7 |     |`Key {loc} -> loc
               ^^^
Warning 42 [disambiguated-name]: this use of "loc" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

module P6235' :
  sig
    type t = { loc : string; }
    type v = { loc : string; x : int; }
    type u = [ `Key of t ]
    val f : u -> string
  end
|}, Principal{|
Line 7, characters 11-14:
7 |     |`Key {loc} -> loc
               ^^^
Warning 42 [disambiguated-name]: this use of "loc" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 7, characters 10-15:
7 |     |`Key {loc} -> loc
              ^^^^^
Warning 18 [not-principal]: this type-based record disambiguation is not
  principal.

module P6235' :
  sig
    type t = { loc : string; }
    type v = { loc : string; x : int; }
    type u = [ `Key of t ]
    val f : u -> string
  end
|}]

(** no candidates after filtering;
    This caused a temporary trunk regression identified by Florian Angeletti
    while reviewing #9196
 *)
module M = struct
  type t = { x:int; y:int}
end
type u = { a:int }
let _ = ( { M.x=0 } : u );;
[%%expect{|
module M : sig type t = { x : int; y : int; } end
type u = { a : int; }
Line 5, characters 12-15:
5 | let _ = ( { M.x=0 } : u );;
                ^^^
Error: The field "M.x" belongs to the record type "M.t"
       but a field was expected belonging to the record type "u"
|}]

(* PR#8747 *)
module M = struct type t = { x : int; y: char } end
let f (x : M.t) = { x with y = 'a' }
let g (x : M.t) = { x with y = 'a' } :: []
let h (x : M.t) = { x with y = 'a' } :: { x with y = 'b' } :: [];;
[%%expect{|
module M : sig type t = { x : int; y : char; } end
Line 2, characters 27-28:
2 | let f (x : M.t) = { x with y = 'a' }
                               ^
Warning 42 [disambiguated-name]: this use of "y" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 2, characters 18-36:
2 | let f (x : M.t) = { x with y = 'a' }
                      ^^^^^^^^^^^^^^^^^^
Warning 40 [name-out-of-scope]: this record of type "M.t" contains fields that
  are not visible in the current scope: "y".
  They will not be selected if the type becomes unknown.

val f : M.t -> M.t = <fun>
Line 3, characters 27-28:
3 | let g (x : M.t) = { x with y = 'a' } :: []
                               ^
Warning 42 [disambiguated-name]: this use of "y" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 3, characters 18-36:
3 | let g (x : M.t) = { x with y = 'a' } :: []
                      ^^^^^^^^^^^^^^^^^^
Warning 40 [name-out-of-scope]: this record of type "M.t" contains fields that
  are not visible in the current scope: "y".
  They will not be selected if the type becomes unknown.

val g : M.t -> M.t list = <fun>
Line 4, characters 27-28:
4 | let h (x : M.t) = { x with y = 'a' } :: { x with y = 'b' } :: [];;
                               ^
Warning 42 [disambiguated-name]: this use of "y" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 4, characters 18-36:
4 | let h (x : M.t) = { x with y = 'a' } :: { x with y = 'b' } :: [];;
                      ^^^^^^^^^^^^^^^^^^
Warning 40 [name-out-of-scope]: this record of type "M.t" contains fields that
  are not visible in the current scope: "y".
  They will not be selected if the type becomes unknown.

Line 4, characters 49-50:
4 | let h (x : M.t) = { x with y = 'a' } :: { x with y = 'b' } :: [];;
                                                     ^
Warning 42 [disambiguated-name]: this use of "y" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 4, characters 40-58:
4 | let h (x : M.t) = { x with y = 'a' } :: { x with y = 'b' } :: [];;
                                            ^^^^^^^^^^^^^^^^^^
Warning 40 [name-out-of-scope]: this record of type "M.t" contains fields that
  are not visible in the current scope: "y".
  They will not be selected if the type becomes unknown.

val h : M.t -> M.t list = <fun>
|}]
