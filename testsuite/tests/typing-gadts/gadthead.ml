(* TEST
 expect;
*)

module M : sig
  type t
  val x : t
  val print : t -> unit
end = struct
  type t = string
  let x = "hello"
  let print = print_endline
end

type _ g = I : int g
[%%expect{|
module M : sig type t val x : t val print : t -> unit end
type _ g = I : int g
|}]

module S = struct
  let g (x : M.t) =
    match x with I -> M.print I
  let () = g M.x
end
[%%expect{|
Line 3, characters 17-18:
3 |     match x with I -> M.print I
                     ^
Error: This pattern matches values of type "'a g"
       but a pattern was expected which matches values of type "M.t"
|}]
