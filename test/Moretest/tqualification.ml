type t1 =
   | A | B | C

and t2 =
   | A | C | D
;;

t1.^A;;
t2.^A;;
A;;

let f1_0 = function
  | t1.^A -> 1
  | B -> 2
  | C -> 3
;;

let f1_1 = function
  | A -> 1
  | B -> 2
  | C -> 3
;;

let g2_0 = function
  | A -> 1
  | D -> 2
  | t2.^C -> 3
;;

let g2_1 = function
  | A -> 1
  | (D | t2.^C) -> 3
;;

g2_1 t2.^A;;
g2_1 D;;
g2_1 t2.^C;;

let g2_2 = function
  | A -> 1
  | D -> 2
  | (t2.^C : t2) -> 3
;;

let g2_3 = function
  | A -> 1
  | D -> 2
  | (t2.^C as v) -> if v = D then 3 else 4
;;

let g2_4 = function
  | (A : t2) | D -> 1
  | (t2.^C : t2) -> 3
;;

let g2_5 = function
  | t2.^A, t2.^C -> 1
  | D, t2.^A -> 2
  | (t2.^C, v) -> if v = D then 3 else 4
  | _ -> 5
;;

