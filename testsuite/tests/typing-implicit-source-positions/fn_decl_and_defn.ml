(* TEST
   expect;
*)

type t = ?call_pos:[%call_pos] -> unit -> unit

[%%expect {|
type t = ?call_pos:[%call_pos] -> unit -> unit
|}]

let f : t = fun ?(call_pos = [%call_pos]) () -> ()

[%%expect{|
val f : t = <fun>
|}]

let g ?(call_pos = [%call_pos]) () = ()

[%%expect{|
val g : ?call_pos:[%call_pos] -> unit -> unit = <fun>
|}]

let apply (f : t) = f ~call_pos:Textloc.dummy () ;;
[%%expect {|
val apply : t -> unit = <fun>
|}]

let _ = apply f ;;
[%%expect{|
- : unit = ()
|}]

let _ = apply g ;;
[%%expect{|
- : unit = ()
|}]

let _ = g ~call_pos:Textloc.dummy () ;;
[%%expect{|
- : unit = ()
|}]
