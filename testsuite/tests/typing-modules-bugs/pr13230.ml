(* TEST
 expect;
*)

module S (M : sig
  type t
end) =
struct
  type 'a u = M.t constraint 'a = M.t

  let id x : M.t u = x
end

module A = S (struct
  type t = unit
end)

module B = S (struct
  type t = unit A.u
end)

[%%expect{|
module S :
  (M : sig type t end) ->
    sig type 'a u = M.t constraint 'a = M.t val id : M.t u -> M.t u end
module A :
  sig type 'a u = 'a constraint 'a = unit val id : unit u -> unit u end
module B :
  sig
    type 'a u = 'a constraint 'a = unit A.u
    val id : unit A.u u -> unit A.u u
  end
|}, Principal{|
module S :
  (M : sig type t end) ->
    sig type 'a u = M.t constraint 'a = M.t val id : M.t u -> M.t u end
module A :
  sig type 'a u = unit constraint 'a = unit val id : unit u -> unit u end
module B :
  sig
    type 'a u = unit A.u constraint 'a = unit A.u
    val id : unit A.u u -> unit A.u u
  end
|}]
