(* TEST
 native;
*)

type r = { foo : float }

type 'a t = Left of 'a | Right of r

type 'a ty =
  | Float : float ty
  | Anything : 'a ty

let f (type a) (ty : a ty) (x : a t) =
  match ty, x with
  | Float, Right { foo = (((3.5 : a) as a) : float) }
  | _, Left a -> ignore (Sys.opaque_identity a)
  | _, _ -> ()

let f = Sys.opaque_identity f

let () = f Anything (Left 0)
