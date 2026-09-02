(* TEST
no-flambda; (* different lambda output *)
flags="-dlambda -dno-locations -dno-unique-ids";
native;
*)

[@@@warning "-8"]

type 'a rep = Float : float rep | Int : int rep

let f b x =
  let[@local] tuple : type a. a rep * a -> float = fun (Float,a) -> a +. 1. in
  if b then tuple (Float, (x +. x)) else tuple (Int, (Sys.opaque_identity 0))
