(* TEST
   expect;
*)

(* textloc and Textloc.t are synonyms *)
let x = Textloc.dummy;;
[%%expect {|
val x : Textloc.t = <location: "", line 0, bytes -1--1>
|}]

let y : textloc = x;;
[%%expect {|
val y : textloc = <location: "", line 0, bytes -1--1>
|}]

let predef_to_module ?(call_loc = [%call_loc]) () : Textloc.t = call_loc ;;
[%%expect{|
val predef_to_module : ?call_loc:[%call_loc] -> unit -> Textloc.t = <fun>
|}]

let module_to_predef (call_loc:Textloc.t) : textloc = call_loc ;;
[%%expect{|
val module_to_predef : Textloc.t -> textloc = <fun>
|}]

let x = predef_to_module ~call_loc:Textloc.dummy ();;
[%%expect{|
val x : Textloc.t = <location: "", line 0, bytes -1--1>
|}]

let y = module_to_predef Textloc.dummy;;
[%%expect{|
val y : textloc = <location: "", line 0, bytes -1--1>
|}]
