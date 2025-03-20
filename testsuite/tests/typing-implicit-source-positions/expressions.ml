(* TEST
   expect;
*)

let x = (fun ?(pos = [%call_pos]) () -> pos) ()
[%%expect{|
val x : lexing_location = <location: "", line 1, bytes 8-47>
|}]

let f = fun ?(call_pos = [%call_pos]) () -> call_pos
[%%expect{|
val f : ?call_pos:[%call_pos] -> unit -> lexing_location = <fun>
|}]

let _ = f ~call_pos:x () ;;
[%%expect{|
- : lexing_location = <location: "", line 1, bytes 8-47>
|}]

let _ = "Increment line count"
let _ = f ~call_pos:(f ()) () ;;
[%%expect{|
- : string = "Increment line count"
- : lexing_location = <location: "", line 2, bytes 20-26>
|}]
