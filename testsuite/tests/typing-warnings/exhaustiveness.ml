(* Warn about all relevant cases when possible *)
let f = function
    None, None -> 1
  | Some _, Some _ -> 2;;

(* Exhaustiveness check is very slow *)
type _ t =
  A : int t | B : bool t | C : char t | D : float t
type (_,_,_,_) u = U : (int, int, int, int) u
type v = E | F | G
;;
(*
let f : type a b c d e f g.
      a t * b t * c t * d t * e t * f t * g t * v
       * (a,b,c,d) u * (e,f,g,g) u -> int =
 function A, A, A, A, A, A, A, _, U, U -> 1
   | _, _, _, _, _, _, _, G, _, _ -> 1
;;
*)

(* Unused cases *)
let f (x : int t) = match x with A -> 1 | _ -> 2;; (* warn *)
let f (x : unit t option) = match x with None -> 1 | _ -> 2;; (* warn? *)
let f (x : unit t option) = match x with None -> 1 | Some _ -> 2;; (* warn *)
let f (x : int t option) = match x with None -> 1 | _ -> 2;;
let f (x : int t option) = match x with None -> 1;; (* warn *)

(* Example with record, type, single case *)

type 'a box = Box of 'a
type 'a pair = {left: 'a; right: 'a};;

let f : (int t box pair * bool) option -> unit = function None -> ();;
let f : (string t box pair * bool) option -> unit = function None -> ();;
