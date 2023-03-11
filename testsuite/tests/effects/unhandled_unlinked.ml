(* TEST
     exit_status= "2"
*)

open Effect
type _ t += E : unit t
let _ = perform E
