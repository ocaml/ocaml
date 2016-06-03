type s = [`A | `B] and sub = [`B];;
type +'a t = T : [< `Conj of 'a & sub | `Other of string] -> 'a t;; (* ok *)

let f (T (`Other msg) : s t) = print_string msg;;
let _ = f (T (`Conj `B) :> s t);; (* warn *)

module M : sig
  type s
  type t = T : [< `Conj of int & s | `Other of string] -> t
  val x : t
end = struct
  type s = int
  type t = T : [< `Conj of int | `Other of string] -> t
  let x = T (`Conj 42)
end;;
    
let () = M.(match x with T (`Other msg) -> print_string msg);; (* warn *)

module M : sig
  type s
  type elim =
      { ex : 'a . ([<`Conj of int & s | `Other of string] as 'a) -> unit }
  val e : elim -> unit
end = struct
  type s = int
  type elim =
      { ex : 'a . (([<`Conj of int | `Other of string] as 'a) -> unit) }
  let e { ex } = ex (`Conj 42 : [`Conj of int])
end;;
    
let () = M.(e { ex = fun (`Other msg) -> print_string msg });; (* warn *)
