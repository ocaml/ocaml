(* DEFINE F(x, y, z) = x + y * z;; *)
(* F(F(1, 2, 3), 4, 5);; *)

(* !+ (1, 2, 3, 4);; *)

(* foldl(( + ), 1, 2, 3, 4);; *)
(* foldr(cons, 1, 2, 3, []);; *)

let cons x xs = x :: xs;;

def_foldl ( !+ ) ( + );;
def_foldr ( !:: ) cons;;

!+ (1, 2, 3, 4);;
!:: (1, 2, 3, []);;
