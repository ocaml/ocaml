let f x =
  (multimatch x with `A -> 1 | `B -> true),
  (multimatch x with `A -> 1. | `B -> "1");;

(* OK *)
module M : sig
  val f :
    [< `A & 'a = int & 'b = float | `B &   'b =string & 'a =  bool] -> 'a * 'b
end = struct let f = f end;;

(* Bad *)
module M : sig
  val f :
    [< `A & 'a = int & 'b = float | `B &   'b =string & 'a =   int] -> 'a * 'b
end = struct let f = f end;;


let f = multifun
    `A -> (multifun `C -> 1 | `D -> 1.)
  | `B -> (multifun `C -> true | `D -> "1");;

(* OK *)
module M : sig
  val f :
    [< `A & 'b = [< `C & 'a = int | `D & 'a = float & 'c = bool] -> 'a
     | `B & 'b = [< `C & 'c = bool | `D & 'c = string] -> 'c] -> 'b
end = struct let f = f end;;

(* Bad *)
module M : sig
  val f :
    [< `A & 'b = [< `C & 'a = int | `D & 'a = bool] -> 'a
     | `B & 'b = [< `C & 'c = bool | `D & 'c = string] -> 'c] -> 'b
end = struct let f = f end;;
