type t = private float;;

val make : float -> t;;
val from : t -> float;;

type s = {f : t};;
