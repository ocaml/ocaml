(* TEST
 expect;
*)

(* #13688 *)
type 'e opt = 'e option constraint 'e = [> `A ]
let f: unit -> [> `A] opt = fun () -> None
let x = f ()
[%%expect{|
type 'a opt = 'a option constraint 'a = [> `A ]
val f : unit -> [> `A ] opt = <fun>
val x : [> `A ] opt = None
|}]
