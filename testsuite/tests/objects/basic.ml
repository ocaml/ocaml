
class a (n:int) =
  object
    val x = n
    method x = x
  end

class b (n:int) (m:int) =
  object (self)
    inherit a n
    val y = m
    method v z =
      self#x + y + z
  end

let f x = (new b 1 2)#v x
let v = new b 1 2
let r1 = v#v 1
let r2 = f 2

let () = print_int r1; print_endline ""
let () = print_int r2; print_endline ""
