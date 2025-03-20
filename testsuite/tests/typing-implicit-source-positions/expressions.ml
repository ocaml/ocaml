(* TEST
   expect;
*)

let x = (fun ?(pos = [%call_loc]) () -> pos) ()
[%%expect{|
val x : textloc = <location: "", line 1, bytes 8-47>
|}]

let f = fun ?(call_loc = [%call_loc]) () -> call_loc
[%%expect{|
val f : ?call_loc:[%call_loc] -> unit -> textloc = <fun>
|}]

let _ = f ~call_loc:x () ;;
[%%expect{|
- : textloc = <location: "", line 1, bytes 8-47>
|}]

let _ = "Increment line count"
let _ = f ~call_loc:(f ()) () ;;
[%%expect{|
- : string = "Increment line count"
- : textloc = <location: "", line 2, bytes 20-26>
|}]
