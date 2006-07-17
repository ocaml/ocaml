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
(* let Some 'a' .. 'b' = Some 'b';; *)

let rec f x y = ();;

fun x y -> ();;

fun x, y -> ()

function x, y -> ();;

let rec next line pos0 = () in ();;

(* fun Some None None None -> ();; *)
