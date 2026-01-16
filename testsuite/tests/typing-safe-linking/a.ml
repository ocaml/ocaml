 type _ t =
     X of string
   | Y : string t

(* this definition used to be
type _ t =
     X of string
   | Y : bytes t
but string and bytes are incompatible since 5.0 *)
