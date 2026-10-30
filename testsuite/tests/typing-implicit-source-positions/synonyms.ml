(* TEST
   expect;
*)

(* textloc and Textloc.Compact.t are synonyms *)
let x = Textloc.Compact.none;;
[%%expect {|
val x : Textloc.Compact.t = <location: File "-">
|}]

let y : textloc = x;;
[%%expect {|
val y : textloc = <location: File "-">
|}]

let predef_to_module ?(call_loc = [%call_loc]) () : Textloc.Compact.t =
  call_loc ;;
[%%expect{|
val predef_to_module : ?call_loc:[%call_loc] -> unit -> Textloc.Compact.t =
  <fun>
|}]

let module_to_predef (call_loc : Textloc.Compact.t) : textloc = call_loc ;;
[%%expect{|
val module_to_predef : Textloc.Compact.t -> textloc = <fun>
|}]

let x = predef_to_module ~call_loc:Textloc.(compact none) ();;
[%%expect{|
val x : Textloc.Compact.t = <location: File "-">
|}]

let y = module_to_predef Textloc.Compact.none;;
[%%expect{|
val y : textloc = <location: File "-">
|}]
