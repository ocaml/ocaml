type (_, _) eq = Eq : ('a, 'a) eq | Neq : int -> ('a, 'b) eq;;
type 'a t;;
let f (type a) (Neq n : (a, a t) eq) = n;;   (* warn! *)

module F (T : sig type _ t end) = struct
 let f (type a) (Neq n : (a, a T.t) eq) = n  (* warn! *)
end;;
