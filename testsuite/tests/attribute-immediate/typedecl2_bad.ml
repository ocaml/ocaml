(* Not guaranteed that t is immediate, so this is an invalid declaration *)

type t
type s = t [@@immediate]
