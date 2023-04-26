type 'a t = T
module type T = sig type t end

type r = { x:unit }
let r = { x = () }

type s = S
let s = S

type _ is_int = Is_int : int is_int
