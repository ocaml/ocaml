fun x when x <> 0 -> x / 42
;;
object val virtual mutable x : int val mutable virtual y : int end
;;
- !r
;;
! -r
;;
-32
;;
- - 32
;;
!(r.b)
;;
(!r).b = !r.b
;;
let l : (unit -> int) list = [(fun _ -> 42); (fun _ -> 42)]
