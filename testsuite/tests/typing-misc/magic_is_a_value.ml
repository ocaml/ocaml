(* TEST
 expect;
*)

(* not really an application, since Obj.magic doesn't do anything *)
let f : 'a -> 'a = Obj.magic Fun.id
[%%expect {|
val f : 'a -> 'a = <fun>
|}]


(* not a value, since it's actually an application *)
let g : 'a -> 'a = Obj.magic Fun.id Fun.id
[%%expect {|
val g : '_a -> '_a = <fun>
|}]
