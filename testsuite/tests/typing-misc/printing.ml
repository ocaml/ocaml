(* PR#7012 *)

type t = [ 'A_name | `Hi ];;

let f (x:'id_arg) = x;;

let f (x:'Id_arg) = x;;
