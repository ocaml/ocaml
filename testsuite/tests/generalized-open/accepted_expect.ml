(* TEST
   * expect
*)

open Set.Make(String);;
[%%expect{|
|}]

let e = empty;;
[%%expect{|
val e : t = <abstr>
|}]

open struct
  let x = singleton "hidden"
end;;
[%%expect{|
|}];;

elements (union x (of_list ["a"; "b"]));;
[%%expect{|
- : elt list = ["a"; "b"; "hidden"]
|}]

let f =
  let open Set.Make(Int32) in
  let e2 = empty in
  let open struct
    let y = 3
  end in
  (e, e2, y);;
[%%expect{|
val f : t * Set.Make(Int32).t * int = (<abstr>, <abstr>, 3)
|}]

module type S = sig
  open Set.Make(Bool)

  type nonrec t = t
end;;
[%%expect{|
module type S = sig type nonrec t = Set.Make(Bool).t end
|}]

let hd _ = ();;
[%%expect{|
val hd : 'a -> unit = <fun>
|}]

open (List : sig val map : ('a -> 'b) -> 'a list -> 'b list end);;
[%%expect{|
|}]

let l = map succ [0;1;2;3]
let () = hd l;;
[%%expect{|
val l : int list = [1; 2; 3; 4]
|}]

let y = map succ [];;
[%%expect{|
val y : int list = []
|}]
