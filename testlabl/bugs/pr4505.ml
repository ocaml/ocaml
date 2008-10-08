class ['a1] a1 = object (self : 'a1)

  val a2 : _ #a2 option ref = ref None
  method set a2' = a2 := Some a2'
  method get = match ! a2 with
    | Some a2 -> a2
    | None -> raise Not_found

end

and ['a2] a2 = object (self : 'a2)

  val a1 : _ #a1 option ref = ref None
  method set a1' = a1 := Some a1'
  method get = match ! a1 with
    | Some a1 -> a1
    | None -> raise Not_found

end


let x = new a1
and y = new a2

let _ =
  x#set (y :> _ a2);


type 'a a = {a : 'a} constraint 'a = < m : int; .. >

class c = object (s)
  method m = 1
  method a = {a = s}
end

let f x = (x :> c);;
