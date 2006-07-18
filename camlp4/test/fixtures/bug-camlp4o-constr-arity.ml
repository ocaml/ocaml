type t = A of t * t | B;;
type t2 = C of (t2 * t2) | D;;
type 'a t3 = S of 'a | T;;

fun B B B -> ();;

fun B (A (B, B)) B -> ();;

fun D (D, D) -> ();;

fun (C (D, D)) -> ();;

let A (b, B) = A (B, B);;

let f (A (B, B)) = ();;

let f B (A (B, B)) = ();;

let (D, d) = (D, D);;

let (C (D, d)) = (C (D, D));;

function S S T -> ();;

function Some (A (B, B)) -> ();;

function S (A (B, B)) -> ();;

function S (D, D) -> ();;

function (C (D, D)) -> ();;

function
| Some Some Some x -> x
(* | None None None x -> x *)
| _ -> assert false;;

fun None None None -> ();;

fun (Some None) None None -> ();;

let Some a = Some 42;;
let Some a :: y = [Some 42];;
let Some a, b = Some 42, 43;;
let (Some a), b = Some 42, 43;;
let Some a as b = let _ = b = 42 in Some 42;;
(* let Some (a as b) = let _ = b = None in Some 42;; *)
(* let Some (a as b) = let _ = b = 42 in Some 42;; *)
(* let (Some a) as b = let _ = b = 42 in Some 42;; *)
(* let (Some a) as b = let _ = b = None in Some 42;; *)
let Some a | Some a = Some 42;;
let x,y as r = 1,2 ;;
let ((x, y) as r) = (1, 2);;

type top = Top of (int * int);;

match Top (1,2) with Top min as t -> ();;

match Top (1,2) with Top (min,max) as t -> ();;

(* let Some 'a' .. 'b' = Some 'b';; *)

let rec f x y = ();;

fun x y -> ();;

fun (x, y) -> ();;

function x, y -> ();;

let rec next line pos0 = () in ();;

(* fun Some None None None -> ();; *)
(* fun x, y -> ();; |+ syntax error +| *)

