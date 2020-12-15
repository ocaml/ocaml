type _ term =
          | Int : int -> int term
          | Add : (int -> int -> int) term
          | App : ('b -> 'a) term * 'b term -> 'a term

let rec eval : type a. a term -> a = function
  | Int n    -> n                 (* a = int *)
  | Add      -> (fun x y -> x+y)  (* a = int -> int -> int *)
  | App(f,x) -> (eval f) (eval x)

(* Code above is currently not eligible to improved errors *)

let _ =
  print_string (eval (Int 3))     (* missing string_of_term *)

(* Nevertheless, we get improved error messages for the rest *)
