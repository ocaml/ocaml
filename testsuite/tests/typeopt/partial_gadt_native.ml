(* TEST
no-flambda; (* different lambda output *)
flags="-dlambda -dno-locations -dcanonical-ids";
native;
*)

[@@@warning "-8"]

type 'a rep = Float : float rep | Int : int rep

(** Expected kind: [ (int * any) -> float]
    However, this kind will only manifest when trying to avoid a tuple
    allocation.
    Typically in this test, this kind appears in the exit condition of
    the generated static catch, which should be
    {[with param/0[int] param/1]} and not {[with param/0[int] param/1[float]]}.
*)
let f b x =
  let[@local] tuple : type a. a rep * a -> float = fun (Float,a) -> a +. 1. in
  if b then tuple (Float, (x +. x)) else tuple (Int, (Sys.opaque_identity 0))
