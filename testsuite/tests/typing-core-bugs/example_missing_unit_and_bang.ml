type 'a t = ('a option * int) ref

let show (x: 'a t) = 
  x
let create () = 
  ref (None, 3)

let _ = show (create)