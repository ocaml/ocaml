(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)


type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn

type _ expr =
  | Int : int -> int expr
  | Add : (int -> int -> int) expr
  | App : ('a -> 'b) expr * 'a expr -> 'b expr


let a = function Dyn (type a b) (a, x : a ty * b) -> ignore (x : b)


let rec eval : type t. t expr -> t = function
    Int n -> n
  | Add -> (+)
  | App (type a) (f, x : _ * a expr) -> eval f (eval x : a)

let rec test : type a. a expr -> a = function
  | Int (type b) (n : a) -> n
  | Add -> (+)
  | App (type b) (f, x : (b -> a) expr * _) -> test f (test x : b)

type _ th =
  | Thunk : 'a * ('a -> 'b) -> 'b th

let f1 (type a) : a th -> a = function
  | Thunk (type b) (x, f : b * (b -> _)) -> f x

let f2 (type a) : a th -> a = function
  | Thunk (type b c) (x, f : b * (b -> c)) -> f x

let t : string = 10
