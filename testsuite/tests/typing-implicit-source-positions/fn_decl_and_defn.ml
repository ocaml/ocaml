(* TEST
   expect;
*)

type t = ?call_loc:[%call_loc] -> unit -> unit

[%%expect {|
type t = ?call_loc:[%call_loc] -> unit -> unit
|}]

let f : t = fun ?(call_loc = [%call_loc]) () -> ()

[%%expect{|
val f : t = <fun>
|}]

let g ?(call_loc = [%call_loc]) () = ()

[%%expect{|
val g : ?call_loc:[%call_loc] -> unit -> unit = <fun>
|}]

let apply (f : t) = f ~call_loc:Textloc.Compact.none () ;;
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

let _ = g ~call_loc:Textloc.Compact.none () ;;
[%%expect{|
- : unit = ()
|}]
