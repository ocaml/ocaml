(* Check the unboxing of record fields *)
type point = { x : int; y : int };;

(* As a first, middle, and last field to check that
   unboxed record position was correctly computed. *)
type t1 = { a : point [@unboxed]; b : int; c : int; };;

let r = { a = { x = 0; y = 0; }; b = 0; c = 0; } in
let match_test ({ a = { x; y; }; b; c; }) =
  x = 0 && y = 0 && b = 0 && c = 0
in
assert (match_test r);
assert (Obj.size (Obj.repr r) = 4);
;;

type t2 = { d : int; e : point [@unboxed]; f : int; };;

let r = { d = 0; e = { x = 0; y = 0; }; f = 0; } in
let match_test ({ d; e = { x; y; }; f; }) =
  d = 0 && x = 0 && y = 0 && f = 0
in
assert (match_test r);
assert (Obj.size (Obj.repr r) = 4)
;;

type t3 = { g : int; h : int; i : point [@unboxed]; };;

let r = { g = 0; h = 0; i = { x = 0; y = 0; }; } in
let match_test ({ g; h; i = { x; y; }; }) =
  g = 0 && h = 0 && x = 0 && y = 0
in
assert (match_test r);
assert (Obj.size (Obj.repr r) = 4)
;;

(* Check that non-records cannot be unboxed *)
type var = T of int * int;;
type t4 = { j : var [@unboxed]; k : int };;

(* Check that recursive unboxes are prevented *)
type t5 = { l : t5 [@unboxed] };;
type t6 = { m : t7 } (* TODO: This would actually be useful *)
and  t7 = { n : t6 [@unboxed] };;

(* Check that mutable records cannot be unboxed *)
type t8 = { mutable o : int; p : int };;
type t9 = { q : t8 [@unboxed]; };;

(* Unboxing interactions are checked *)
type t10 = { r : int } [@@unboxed];;
type t11 = { s : t10 [@unboxed] };;
type t11a = { a : point [@unboxed] } [@@unboxed];;
type t11b = T of { a : point [@unboxed] } [@@unboxed];;

(* Nested unboxing is supported *)
type t12 = { t : point [@unboxed]; u : int };;
type t13 = { v : int; w : t12 [@unboxed]; };;

let r = { v = 0; w = { t = { x = 0; y = 0; }; u = 0; }; } in
let match_test { v; w = { t = { x; y; }; u}; } =
  v = 0 && x = 0 && y = 0 && u = 0
in
assert (match_test r);
assert (Obj.size (Obj.repr r) = 4);
;;

(* Multiple unboxing is supported *)
type t14 = { z : point [@unboxed]; a : point [@unboxed]; b : point [@unboxed] };;

let q = { x = 0; y = 0; } in
let r = { z = q; a = q; b = q } in
assert (Obj.size (Obj.repr r) = (3 * 2))
;;

(* Mutable unboxes. Check that set-field calculated position is correct *)
type t15 = { mutable c : point [@unboxed]; d : int };;
type t16 = { e : int; mutable f : point [@unboxed]; };;

let r = { c = { x = 0; y = 0; }; d = 0; } in
r.c <- { x = 1; y = 1 };
assert (r.c.x = 1 && r.c.y = 1)
;;

let r = { e = 0; f = { x = 0; y = 0; }; } in
r.f <- { x = 1; y = 1 };
assert (r.f.x = 1 && r.f.y = 1)
;;

(* Float inline not supported *)
type t17 = { g : float; h : float };; (* Optimises into an array *)
type t18 = { i : float; j : t17 [@unboxed]; k : float };;

(* Record initialisation from an existing record *)
type t19 = { l : point [@unboxed]; m : int };;

let r1 = { l = { x = 0; y = 0; }; m = 0 } in
(* Allocates a new record and projects fields out of r1 *)
let r2 = { r1 with m = 1 } in
assert (r2.l.x = 0 && r2.l.y = 0 && r2.m = 1)
;;

type pad = { n : int;  o : int;  p : int;  q : int;
             r : int;  s : int;  t : int;  u : int;
             v : int;  w : int;  x : int;  y : int;
             z : int;  a : int;  b : int;  c : int;
             d : int;  e : int;  f : int;  g : int;
             h : int;  i : int;  j : int;  k : int;
             l : int;  m : int;  n2 : int; o2 : int;
             p2 : int; q2 : int; r2 : int; s2 : int;
           };;
let padrec = { n = 0; o = 0; p = 0; q = 0;
               r = 0; s = 0; t = 0; u = 0;
               v = 0; w = 0; x = 0; y = 0;
               z = 0; a = 0; b = 0; c = 0;
               d = 0; e = 0; f = 0; g = 0;
               h = 0; i = 0; j = 0; k = 0;
               l = 0; m = 0; n2 = 0; o2 = 0;
               p2 = 0; q2 = 0; r2 = 0; s2 = 0;
             };;

(* Record size > 256 *)
type t20 = { t : pad [@unboxed]; u : pad [@unboxed];
             v : pad [@unboxed]; w : pad [@unboxed];
             x : pad [@unboxed]; y : pad [@unboxed];
             z : pad [@unboxed]; a : pad [@unboxed];
             b : point [@unboxed];
             c : int;
             d : point [@unboxed]; };;

let p = { x = 0; y = 0; } in
let r1 = { t = padrec; u = padrec; v = padrec; w = padrec;
           x = padrec; y = padrec; z = padrec; a = padrec;
           b = p; c = 0; d = p;
         } in
let q = { x = 1; y = 1 } in
(* Takes a shallow copy of r1 and mutates b, c, d *)
let r2 = { r1 with b = q; c = 1; d = q; } in
assert (r2.b.x = 1 && r2.c = 1 && r2.d.x = 1)
;;

(* Module signature and its definition have to have consistent annotations *)
module M1 : sig (* Should be OK *)
  type t = { e : point; f : point [@unboxed] }
end = struct
  type t = { e : point; f : point [@unboxed] }
end
;;

module M2 : sig (* Should fail *)
  type t = { e : point; f : point }
end = struct
  type t = { e : point; f : point [@unboxed] }
end
;;

module M3 : sig (* Should fail *)
  type t = { e : point; f : point [@unboxed] }
end = struct
  type t = { e : point; f : point }
end
;;

(* Tuple unboxing *)
type t21 = { g : int;
             h : int * int [@unboxed];
             i : int; }
;;

let r = { g = 0; h = (0, 0); i = 0; } in
let match_test { g; h = (x, y); i } =
  g = 0 && x = 0 && y = 0 && i = 0
in
assert (match_test r);
assert (r.h = (0, 0))
;;

type t22 = { g : int;
             mutable h : int * int [@unboxed];
             i : int; }
;;

let r = { g = 0; h = (0, 0); i = 0; } in
r.h <- (1, 1);
assert (r.h = (1, 1))
;;

(* 'Inlined' record unboxing *)
type t23 = T of { i : int; j : point [@unboxed] }
;;

let r = T { i = 0; j = { x = 0; y = 0; }; } in
let match_test = function
  | T { i; j = { x; y; }; } -> x = 0 && y = 0 && i = 0
in
assert (match_test r);
assert (Obj.size (Obj.repr r) = 3);
;;

type t24 = U of { k : point [@unboxed]; l : int }
         | V of { m : int; n : point [@unboxed] }
;;

let r1 = U { k = { x = 0; y = 0; }; l = 0; } in
let r2 = V { m = 1; n = { x = 1; y = 1; }; } in
let match_test = function
  | U { k = { x; y; }; l; } -> x = 0 && y = 0 && l = 0
  | V { m; n = { x; y; }; } -> x = 1 && y = 1 && m = 1
in
assert (match_test r1);
assert (match_test r2);
assert (Obj.size (Obj.repr r1) = 3);
assert (Obj.size (Obj.repr r2) = 3);
;;

module M4 : sig (* Should be OK *)
  type t = T of { e : point; f : point [@unboxed] }
end = struct
  type t = T of { e : point; f : point [@unboxed] }
end
;;

module M5 : sig (* Should fail *)
  type t = T of { e : point; f : point }
end = struct
  type t = T of { e : point; f : point [@unboxed] }
end
;;

module M6 : sig (* Should fail *)
  type t = T of { e : point; f : point [@unboxed] }
end = struct
  type t = T of { e : point; f : point }
end
;;

(* Extension records *)
type t25 = ..;;
type t25 += U of { o : point [@unboxed]; p : int; };;
type t25 += V of { q : int; r : point [@unboxed] };;

let r1 = U { o = { x = 0; y = 0; }; p = 0; } in
let r2 = V { q = 1; r = { x = 1; y = 1; }; } in
let match_test = function
  | U { o = { x; y; }; p; } -> x = 0 && y = 0 && p = 0
  | V { q; r = { x; y; }; } -> x = 1 && y = 1 && q = 1
in
assert (match_test r1);
assert (match_test r2);
assert (Obj.size (Obj.repr r1) = 4);
assert (Obj.size (Obj.repr r2) = 4);
;;
