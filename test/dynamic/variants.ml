let failures = ref 0;;

let subtyping = false;;
let instantiation = true;;

type a = A;;
type b = B | C;;
type a1 = A1 of int;;
type a2 = A2 of int * int;;
type a3 = A3 of (int * int);;

DYNTEST A as a to a for true in failures;;
DYNTEST A as a to b for false in failures;;
DYNTEST B as b to b for true in failures;;
DYNTEST C as b to b for true in failures;;
DYNTEST B as b to a for false in failures;;
DYNTEST C as b to a for false in failures;;

DYNTEST A as a to a1 for false in failures;;
DYNTEST A1 42 as a1 to a for false in failures;;
DYNTEST A1 42 as a1 to a1 for true in failures;;
DYNTEST A1 42 as a1 to a2 for false in failures;;
DYNTEST A1 42 as a1 to a3 for false in failures;;
DYNTEST A2 (42, 43) as a2 to a1 for false in failures;;
DYNTEST A2 (42, 43) as a2 to a2 for true in failures;;
DYNTEST A2 (42, 43) as a2 to a3 for false in failures;;
DYNTEST A3 ((42, 43)) as a3 to a1 for false in failures;;
DYNTEST A3 ((42, 43)) as a3 to a2 for false in failures;;
DYNTEST A3 ((42, 43)) as a3 to a3 for true in failures;;

module M = struct type s = S end;;
type s = S;;
DYNTEST M.S as M.s to s for true in failures;;
DYNTEST S as s to M.s for true in failures;;

module BOOL = struct type t = True | False end;;
module BOOL' = struct type t = False | True end;;
DYNTEST BOOL.True as BOOL.t to BOOL'.t for false in failures;;
DYNTEST BOOL'.True as BOOL'.t to BOOL.t for false in failures;;
DYNTEST BOOL.False as BOOL.t to BOOL'.t for false in failures;;
DYNTEST BOOL'.False as BOOL'.t to BOOL.t for false in failures;;

module R = struct type t = A1 | A2 of t end;;
module R' = struct type t = A1 | A2 of R.t end;;
DYNTEST R.A1 as R.t to R.t for true in failures;;
DYNTEST R.A1 as R.t to R'.t for true in failures;;
DYNTEST R'.A1 as R'.t to R'.t for true in failures;;
DYNTEST R'.A1 as R'.t to R.t for true in failures;;

module S = struct type t1 = A1 of t2 and t2 = A3 | A2 of t1 end;;
module S' = struct type t2 = A3 | A2 of t1 and t1 = A1 of t2 end;;
DYNTEST S.A1 S.A3 as S.t1 to S'.t1 for true in failures;;
DYNTEST S'.A1 S'.A3 as S'.t1 to S.t1 for true in failures;;

module L = struct type 'a t = N | C of 'a * 'a t;;
                  type 'a tr = A of ('a tr) t;; end;;

module L' = struct type 'a t = N | C of 'a * 'a t;;
                   type 'a tr = A of ('a tr) t;; end;;


DYNTEST L.A L.N as int L.t L.tr to int L'.t L'.tr for true in failures;;

DYNTEST L.A L.N as int L.t L.tr to int L'.tr for true in failures;;
DYNTEST L.A L.N as int L.tr to int L'.t L'.tr for true in failures;;

module L2 = struct type 'a t = N | C of 'a * 'a t;;
                   type 'a tr = A of ('a tr) t | B of 'a;; end;;

DYNTEST L2.B (L2.C (3, L2.N)) as int L2.t L2.tr to int L2.t L2.tr for true in failures;;
DYNTEST L2.B 3 as int L2.tr to int L2.t L2.tr for false in failures;;


SUMMARY in failures;;
