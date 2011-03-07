(* x and y are free *)
close_expr(x y);;

(* bind x *)
let x a = a + 42;;

(* y is free *)
close_expr(x y);;

(* bind y locally so the expr is closed *)
close_expr(let y = x 2 in x y);;

(* bind y locally but outside, z is free *)
let y = x 2 in close_expr(x (z y));;
