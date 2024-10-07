(* TEST
   flags="-dheadshape";
   expect;
*)

type t = int
[%%expect{|
shape of t: {imm = Any; blocks = [];}
type t = int
|}]

type u =
  | Block of bool
  | Constant
  | Int of t [@unboxed]
[%%expect{|
Lines 1-4, characters 0-23:
1 | type u =
2 |   | Block of bool
3 |   | Constant
4 |   | Int of t [@unboxed]
Error: Constructors Constant and Int have overlapping representations.
         shape of Constant: {imm = [0]; blocks = [];}
         shape of Int: {imm = Any; blocks = [];}
|}]


type v =
  | Bool of bool
  | Maybe_int of int option [@unboxed]
[%%expect{|
Lines 1-3, characters 0-38:
1 | type v =
2 |   | Bool of bool
3 |   | Maybe_int of int option [@unboxed]
Error: Constructors Bool and Maybe_int have overlapping representations.
         shape of Bool: {imm = []; blocks = [0];}
         shape of Maybe_int: {imm = [0]; blocks = [0];}
|}]


(* There is a conflict between Single and Double,
   because we are not tracking size/arity. *)
type 'a solution =
  | Zero
  | Single of 'a
  | Double of 'a pair [@unboxed]
and 'a pair = 'a * 'a
[%%expect{|
Lines 1-4, characters 0-32:
1 | type 'a solution =
2 |   | Zero
3 |   | Single of 'a
4 |   | Double of 'a pair [@unboxed]
Error: Constructors Single and Double have overlapping representations.
         shape of Single: {imm = []; blocks = [0];}
         shape of Double: {imm = []; blocks = [0];}
|}]
