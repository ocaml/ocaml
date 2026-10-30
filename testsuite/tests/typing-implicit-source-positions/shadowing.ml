(* TEST
   expect;
*)

(* Shadowing *)

type textloc = int
[%%expect{|
type textloc = int
|}]

(* src_pos works *)
let f ?(call_loc = [%call_loc]) () = ();;
[%%expect{|
val f : ?call_loc:[%call_loc] -> unit -> unit = <fun>
|}]

let _ = f ~call_loc:Textloc.Compact.none () ;;
[%%expect{|
- : unit = ()
|}]

(* new type works *)
let h (x : textloc) = x ;;
[%%expect{|
val h : textloc -> textloc = <fun>
|}]

let _ = h 5;;
[%%expect {|
- : textloc = 5
|}]

(* Works with class parameters *)
class c ?(call_loc = [%call_loc]) () = object end

[%%expect {|
class c : ?call_loc:[%call_loc] -> unit -> object  end
|}]

let _ = new c ~call_loc:Textloc.Compact.none ();;

[%%expect{|
- : c = <obj>
|}]

(* Works with object method parameters *)
let o = object
   method m ?(call_loc = [%call_loc]) () = ()
end

[%%expect {|
val o : < m : ?call_loc:[%call_loc] -> unit -> unit > = <obj>
|}]

let _ = o#m ~call_loc:Textloc.Compact.none ()

[%%expect{|
- : unit = ()
|}]
