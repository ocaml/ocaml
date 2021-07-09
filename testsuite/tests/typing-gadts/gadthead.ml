(* TEST
   * expect
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

let g (x : M.t) =
  match x with I -> M.print I
let () = g M.x
[%%expect{|
Line 2, characters 15-16:
2 |   match x with I -> M.print I
                   ^
Error: This pattern matches values of type 'a g
       but a pattern was expected which matches values of type M.t
|}]
