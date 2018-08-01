(* TEST
   * expect
*)

(* See MPR#7822, by octachron *)

(** Skip to the end of the file for the problematic refutation clauses *)
(** {1 Chessboard and wazir's moves encoding }*)

(** Visited case or border *)
type x = X

(** Free case *)
type o = O

(** 4x4 chessboards are the smallest size that trigers the issue on
    hamiltonian path refutation *)
type 'a r = 'a * 'a * 'a
type 'a c = 'a * 'a * 'a
type 'a rect = 'a r c


(** Chessboard-representation
Current position is at ce:
  ul ul ul up ur ur ur
  ul ul ul up ur ur ur
  le le le ce ri ri ri
  dl dl dl dw dr dr dr
  dl dl dl sw dr dr dr
*)
type start =
  < ul:x rect ; up:x c;      ur:x rect ;
    le:x r ; ce:x;      ri: o r;
    dl:x rect; dw: o c; dr: o rect
  >

(** pick first or last case of a line *)
type 't lfirst = 'a * 'b * 'c
  constraint 't = 'a * ('b * 'c)
type 't llast = 'a * 'b * 'c
  constraint 't = ('a * 'b) * 'c

(** same for columns *)
type 't cfirst = 't lfirst
type 't clast = 't llast

(** pick the firt or last columns in a rectangle *)
type 't fcol =
    ('l1 * 'r1) lfirst
  * ('l2 * 'r2) lfirst
  * ('l3 * 'r3) lfirst
  constraint 't =
    ( 'l1 * 'l2 * 'l3) * ( 'r1 * 'r2 * 'r3)
type 't lcol =
  ('l1 * 'r1) llast
  * ('l2 * 'r2) llast
  * ('l3 * 'r3) llast
  constraint 't =
    ( 'l1 * 'l2 *'l3) * ( 'r1 * 'r2 *'r3)

type (_,_) move =
  | Right:(
      < ul:('ul * _) lcol ; up:'up;   ur:('nur * 'ur ) fcol;
        le:('le *_) llast; ce:x;   ri:(o * 'ri) lfirst;
        dl:('dl * _) lcol ; dw:'dw; dr: ('ndr * 'dr) fcol;
      >,
      < ul:('up * 'ul) fcol; up:'nur;  ur:('ur * x c) lcol;
        le:(x * 'le) lfirst; ce:x;  ri:('ri * x) llast;
        dl:('dw * 'dl) fcol; dw:'ndr; dr: ('dr * x c) lcol
      >
    ) move
  | Left:(
      < ul:('nul * 'ul) fcol ; up:'up;   ur:('ur * _ ) lcol;
        le:(o * 'le) lfirst; ce:x;   ri:('ri * _) llast;
        dl:('ndl * 'dl) fcol ; dw:'dw; dr: ('dr * _) lcol;
      >,
      < ul:('ul * x c) lcol; up:'nul; ur:('up * 'ur) fcol;
        le:('le * x) llast;   ce:x;   ri:(x * 'ri) lfirst;
        dl:('dl * x c) lcol; dw:'ndl; dr: ('dw * 'dr) fcol
      >
    ) move
  | Up:(
      < ul:('nul * 'ul) cfirst; up:(o * 'up ) cfirst;  ur:('nur * 'ur) cfirst;
        le:'le;                ce:x;                   ri:'ri;
        dl:('dl * _) clast;     dw: ('dw * _) clast ;     dr:('dr * _) clast;
      >,
      < ul: ('ul * x r) clast; up:('up * x) clast;  ur: ('ur * x r) clast;
        le:'nul;               ce:x;               ri:'nur;
        dl:('le * 'dl) cfirst;  dw:(x * 'dw) cfirst; dr:('ri * 'dr) cfirst
      >
    ) move
  | Down:(
      < ul:('ul * _ ) clast;     up:('up * _ ) clast;  ur:('ur * _) clast;
        le:'le;                  ce:x;               ri:'ri;
        dl:('ndl * 'dl) cfirst;  dw: (o * 'dw) cfirst ; dr:('ndr * 'dr) cfirst;
      >,
      < ul: ('le * 'ul) cfirst ; up: (x * 'up) cfirst; ur: ('ri *'ur) cfirst;
        le:'ndl;                ce:x;                ri:'ndr;
        dl:('dl * x r) clast;   dw:('dw * x) clast;   dr:('dr * x r) clast
      >
    ) move


(** {1 Path encoding } *)
type _ path =
  | []: start path
  | (::) : ('a,'b) move * 'a path -> 'b path

type hamiltonian = <ul: x rect; up: x c; ur: x rect;
                    le:x r;  ce: x  ; ri: x r;
                    dl: x rect; dw: x c; dr: x rect
                   >;;
[%%expect{|
type x = X
type o = O
type 'a r = 'a * 'a * 'a
type 'a c = 'a * 'a * 'a
type 'a rect = 'a r c
type start =
    < ce : x; dl : x rect; dr : o rect; dw : o c; le : x r; ri : o r;
      ul : x rect; up : x c; ur : x rect >
type 'd lfirst = 'a * 'b * 'c constraint 'd = 'a * ('b * 'c)
type 'd llast = 'a * 'b * 'c constraint 'd = ('a * 'b) * 'c
type 'a cfirst = 'a lfirst constraint 'a = 'b * ('c * 'd)
type 'a clast = 'a llast constraint 'a = ('b * 'c) * 'd
type 'a fcol =
    ('l1 * ('b * 'c)) lfirst * ('l2 * ('d * 'e)) lfirst *
    ('l3 * ('f * 'g)) lfirst
  constraint 'a = ('l1 * 'l2 * 'l3) * (('b * 'c) * ('d * 'e) * ('f * 'g))
type 'a lcol =
    (('b * 'c) * 'r1) llast * (('d * 'e) * 'r2) llast *
    (('f * 'g) * 'r3) llast
  constraint 'a = (('b * 'c) * ('d * 'e) * ('f * 'g)) * ('r1 * 'r2 * 'r3)
type (_, _) move =
    Right :
      (< ce : x;
         dl : ((('a * 'b) * ('c * 'd) * ('e * 'f)) * ('g * 'h * 'i)) lcol;
         dr : (('j * 'k * 'l) * (('m * 'n) * ('o * 'p) * ('q * 'r))) fcol;
         dw : 's * 't * 'u; le : (('v * 'w) * 'x) llast;
         ri : (o * ('y * 'z)) lfirst;
         ul : ((('a1 * 'b1) * ('c1 * 'd1) * ('e1 * 'f1)) * ('g1 * 'h1 * 'i1))
              lcol;
         up : 'j1 * 'k1 * 'l1;
         ur : (('m1 * 'n1 * 'o1) * (('p1 * 'q1) * ('r1 * 's1) * ('t1 * 'u1)))
              fcol >,
       < ce : x;
         dl : (('s * 't * 'u) * (('a * 'b) * ('c * 'd) * ('e * 'f))) fcol;
         dr : ((('m * 'n) * ('o * 'p) * ('q * 'r)) * x c) lcol;
         dw : 'j * 'k * 'l; le : (x * ('v * 'w)) lfirst;
         ri : (('y * 'z) * x) llast;
         ul : (('j1 * 'k1 * 'l1) * (('a1 * 'b1) * ('c1 * 'd1) * ('e1 * 'f1)))
              fcol;
         up : 'm1 * 'n1 * 'o1;
         ur : ((('p1 * 'q1) * ('r1 * 's1) * ('t1 * 'u1)) * x c) lcol >)
      move
  | Left :
      (< ce : x;
         dl : (('v1 * 'w1 * 'x1) * (('y1 * 'z1) * ('a2 * 'b2) * ('c2 * 'd2)))
              fcol;
         dr : ((('e2 * 'f2) * ('g2 * 'h2) * ('i2 * 'j2)) * ('k2 * 'l2 * 'm2))
              lcol;
         dw : 'n2 * 'o2 * 'p2; le : (o * ('q2 * 'r2)) lfirst;
         ri : (('s2 * 't2) * 'u2) llast;
         ul : (('v2 * 'w2 * 'x2) * (('y2 * 'z2) * ('a3 * 'b3) * ('c3 * 'd3)))
              fcol;
         up : 'e3 * 'f3 * 'g3;
         ur : ((('h3 * 'i3) * ('j3 * 'k3) * ('l3 * 'm3)) * ('n3 * 'o3 * 'p3))
              lcol >,
       < ce : x; dl : ((('y1 * 'z1) * ('a2 * 'b2) * ('c2 * 'd2)) * x c) lcol;
         dr : (('n2 * 'o2 * 'p2) * (('e2 * 'f2) * ('g2 * 'h2) * ('i2 * 'j2)))
              fcol;
         dw : 'v1 * 'w1 * 'x1; le : (('q2 * 'r2) * x) llast;
         ri : (x * ('s2 * 't2)) lfirst;
         ul : ((('y2 * 'z2) * ('a3 * 'b3) * ('c3 * 'd3)) * x c) lcol;
         up : 'v2 * 'w2 * 'x2;
         ur : (('e3 * 'f3 * 'g3) * (('h3 * 'i3) * ('j3 * 'k3) * ('l3 * 'm3)))
              fcol >)
      move
  | Up :
      (< ce : x; dl : (('q3 * 'r3) * 's3) clast;
         dr : (('t3 * 'u3) * 'v3) clast; dw : (('w3 * 'x3) * 'y3) clast;
         le : 'le; ri : 'ri; ul : ('nul * ('z3 * 'a4)) cfirst;
         up : (o * ('b4 * 'c4)) cfirst; ur : ('nur * ('d4 * 'e4)) cfirst >,
       < ce : x; dl : ('le * ('q3 * 'r3)) cfirst;
         dr : ('ri * ('t3 * 'u3)) cfirst; dw : (x * ('w3 * 'x3)) cfirst;
         le : 'nul; ri : 'nur; ul : (('z3 * 'a4) * x r) clast;
         up : (('b4 * 'c4) * x) clast; ur : (('d4 * 'e4) * x r) clast >)
      move
  | Down :
      (< ce : x; dl : ('ndl * ('f4 * 'g4)) cfirst;
         dr : ('ndr * ('h4 * 'i4)) cfirst; dw : (o * ('j4 * 'k4)) cfirst;
         le : 'le; ri : 'ri; ul : (('l4 * 'm4) * 'n4) clast;
         up : (('o4 * 'p4) * 'q4) clast; ur : (('r4 * 's4) * 't4) clast >,
       < ce : x; dl : (('f4 * 'g4) * x r) clast;
         dr : (('h4 * 'i4) * x r) clast; dw : (('j4 * 'k4) * x) clast;
         le : 'ndl; ri : 'ndr; ul : ('le * ('l4 * 'm4)) cfirst;
         up : (x * ('o4 * 'p4)) cfirst; ur : ('ri * ('r4 * 's4)) cfirst >)
      move
type _ path = [] : start path | (::) : ('a, 'b) move * 'a path -> 'b path
type hamiltonian =
    < ce : x; dl : x rect; dr : x rect; dw : x c; le : x r; ri : x r;
      ul : x rect; up : x c; ur : x rect >
|}]

(** {1 Refutation clause and existential limits } *)
(** How to convice the typechecker that wazir's tours don't exist: *)
let there_are_no_wazir_tours: 'a. hamiltonian path -> 'a = function
  | [   _;_;_
     ;_;_;_;_
     ;_;_;_;_
     ;_;_;_;_
     ]-> .
;;
[%%expect{|
Line 1, characters 59-134:
  ...........................................................function
    | [   _;_;_
       ;_;_;_;_
       ;_;_;_;_
       ;_;_;_;_
       ]-> .
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Right::Right::Up::Left::Up::Left::Down::_::_::_::_::_::_::_::_::_::_
Line 2, characters 4-62:
  ....[   _;_;_
       ;_;_;_;_
       ;_;_;_;_
       ;_;_;_;_
       ]....
Error: This match case could not be refuted.
       Here is an example of a value that would reach it:
       Right::Right::Up::Left::Left::Up::Up::Right::Down::Right::Up::Right::
       Down::Down::Down::[]
|}]

(** v Simultaneously,  this does not compile v *)
let find_hamiltonian:hamiltonian path -> _ = function
  | [   _;_;_
     ;_;_;_;_
     ;_;_;_;_
     ;Down;Right;Right;Right
     ]-> .
;;
[%%expect{|
Line 1, characters 45-135:
  .............................................function
    | [   _;_;_
       ;_;_;_;_
       ;_;_;_;_
       ;Down;Right;Right;Right
       ]-> .
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Right::Up::Right::Down::Down::Left::Down::Left::Up::_::_::Down::Right::
Right::Right::_::_
Line 2, characters 4-77:
  ....[   _;_;_
       ;_;_;_;_
       ;_;_;_;_
       ;Down;Right;Right;Right
       ]....
Error: This match case could not be refuted.
       Here is an example of a value that would reach it:
       Right::Right::Up::Up::Left::Down::Left::Up::Left::Down::Down::Down::
       Right::Right::Right::[]
|}]
