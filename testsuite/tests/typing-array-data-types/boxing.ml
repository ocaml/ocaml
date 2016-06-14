
module Normal = struct

  type 'a t = [| 'a |]

end

module Dynamic = struct

  type 'a t = [| 'a [@dynamic_boxing] |]

end

module Float = struct

  type t = [| float |]

end

let normal_string = Normal.[| "hello" |]
let dynamic_string = Dynamic.[| "hello" |]

let normal_float = Normal.[| 4.5 |]
let dynamic_float = Dynamic.[| 4.5 |]
let float_float = Float.[| 4.5 |]

;;

let () = assert (Obj.tag (Obj.repr normal_string) = 0)
;;

let () = assert (Obj.tag (Obj.repr dynamic_string) = 0)
;;

let () = assert (Obj.tag (Obj.repr normal_float) = 0)
;;

let () = assert (Obj.tag (Obj.repr dynamic_float) = Obj.double_array_tag)
;;

let () = assert (Obj.tag (Obj.repr float_float) = Obj.double_array_tag)
;;
