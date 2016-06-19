
module M = struct
  type t = Nil
  let ( .[] ) Nil _x = Nil
  let ( .[]<- ) Nil _x Nil = ()
  let ( .{} ) Nil _x = Nil
  let ( .{}<- ) Nil _x Nil = ()
end

let x = M.Nil

let () = x.M.[0] <- x.M.{0}
let () = x.M.{0} <- x.M.[0]
