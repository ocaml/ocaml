let f x =
  match x with
  | _ -> ()
  | exception _ -> .
;;

let f x =
  match x with
  | _ -> ()
  | None | exception _ -> .
;;

let f x =
  match x with
  | _ -> ()
  | exception Not_found | None -> .
;;

let f x =
  match x with
  | _ | exception _ -> ()
  | exception Not_found -> .
;;
