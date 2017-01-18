 type _ t =
     X of string
   | Y : bytes t

(* It is important that the line below is the last line of the file (see Makefile) *)
let y : string t = Y
