type _ t = T : int t;;

(* Should raise Not_found *)
let _ = match (raise Not_found : float t) with _ -> .;;
