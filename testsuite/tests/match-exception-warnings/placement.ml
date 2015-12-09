(*****************************************************)
(* Restrict where "exception P" patterns can appear. *)
(*****************************************************)

(* should be accepted *)

let f x =
  match x () with
  | _ -> ()
  | exception _ -> ()
;;

let f x =
  match x () with
  | _ | exception _ -> ()
;;

(* should be rejected *)

let f x =
  try x (); ()
  with exception _ -> ()
;;

let f x =
  match x () with
  | (exception _) as _pat -> ()
  | _ -> ()
;;

let f x =
  match x () with
  | (_, exception _, _) -> ()
;;

let f x =
  match x () with
  | lazy (exception _) -> ()
  | _ -> ()
;;

let f x =
  match x () with
  | { contents = exception _ } -> ()
;;

let f x =
  match x () with
  | [| exception _ |] -> ()
;;

let f x =
  match x () with
  | Some (exception _) -> ()
;;

let f = function
  | exception _ -> ()
  | _ -> ()
;;
