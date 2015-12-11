(* Regression testing for PR#7083 *)
let simple x =
  match x with
  | `A -> ()
  | exception Not_found -> ()
;;

let moderatly_less_simple x =
  match x with
  | `A | exception Exit -> ()
;;

let less_simple x =
  match x with
  | `A | exception Exit -> ()
  | exception Not_found -> ()
;;

type t = [ `A | `B ]

let plain_weird x =
  match x with
  | #t -> ()
  | exception Not_found -> ()
;;

let plain_weird' x =
  match x with
  | #t | exception Exit -> ()
  | exception Not_found -> ()
;;
