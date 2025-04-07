(* TEST
   expect;
*)

(* lexing_location and Textloc.t are synonyms *)
let x = Textloc.dummy;;
[%%expect {|
val x : Textloc.t = <location: "", line 0, bytes -1--1>
|}]

let y : lexing_location = x;;
[%%expect {|
val y : lexing_location = <location: "", line 0, bytes -1--1>
|}]

let predef_to_module ?(call_pos = [%call_pos]) () : Textloc.t = call_pos ;;
[%%expect{|
val predef_to_module : ?call_pos:[%call_pos] -> unit -> Textloc.t = <fun>
|}]

let module_to_predef (call_pos:Textloc.t) : lexing_location = call_pos ;;
[%%expect{|
val module_to_predef : Textloc.t -> lexing_location = <fun>
|}]

let x = predef_to_module ~call_pos:Textloc.dummy ();;
[%%expect{|
val x : Textloc.t = <location: "", line 0, bytes -1--1>
|}]

let y = module_to_predef Textloc.dummy;;
[%%expect{|
val y : lexing_location = <location: "", line 0, bytes -1--1>
|}]
