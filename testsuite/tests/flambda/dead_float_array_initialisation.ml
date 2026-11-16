(* TEST *)

type _ t = Int : int t | Float : float t

let () =
  let f (type a) (t : a t) (x : a) =
    match t with
    | Float ->
        (* This array initialisation assumes that x is float, yet some
           analysis can propagate the call argument that is an int. *)
        ignore [| x |]
    | Int -> ()
  in
  f (Sys.opaque_identity Int) 0;
  print_endline "ok"
