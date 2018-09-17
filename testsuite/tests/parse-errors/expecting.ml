(* TEST
   * toplevel
*)

let f = function
  | 3 as 3 -> ()
;;

let f = function
  | 3 :: -> ()
;;

let f = function
  | 3 | -> ()
;;

let f = function
  | List.( -> ()
;;

let f = function
  | (3 : 3) -> ()
;;

let f = function
  | (3,) -> ()
;;

let f = function
  | ( -> ()
;;

let f = function
  | (module -> ()
;;
