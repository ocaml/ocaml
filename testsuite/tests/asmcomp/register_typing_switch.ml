(* TEST
 native;
*)

type 'a typ = Int : int typ | Ptr : int list typ | Int2 : int typ

let f (type a) (t : a typ) (p : int list) : a =
  match t with
  | Int -> 100
  | Ptr -> p
  | Int2 -> 200

let allocate_garbage () =
  for i = 0 to 100 do
    ignore (Array.make 200 0.0)
  done

let g (t : int list typ) x =
  Gc.minor ();
  let x = f t ([x; x; x; x; x]) in
  Gc.minor ();
  allocate_garbage ();
  ignore (String.length (String.concat " " (List.map Int.to_string x)))

let () = g Ptr 5
