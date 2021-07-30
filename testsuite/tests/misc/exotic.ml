(* TEST
   flags = "-I ${ocamlsrcdir}/utils"
   * expect
*)

(* Strict-sequence can change the behavior of programs *)

(* The two examples below were proposed by Jeremy Yallop in
   https://github.com/ocaml/ocaml/pull/1971 .
   Note that those tests are here to record this behavior and not to enshrine it.
*)

[@@@warning "-non-unit-statement"];;
[@@@warning "-not-principal"];;
[@@@warning "-partial-match"];;
[@@@warning "-ignored-partial-application"];;

type t = A | () and b = B : _ -> b;;
[%%expect{|
type t = A | ()
and b = B : 'a -> b
|}];;

Clflags.strict_sequence := false ;;
let f (g : 'a) = g; Format.printf "%b@." (B (() : 'a) = B A) in f ();;
[%%expect {|
- : unit = ()
false
- : unit = ()
|}]
;;

Clflags.strict_sequence := true ;;
let f (g : 'a) = g; Format.printf "%b@." (B (() : 'a) = B A) in f ();;
[%%expect {|
- : unit = ()
true
- : unit = ()
|}]
;;

[@@@warning "-labels-omitted"];;
Clflags.strict_sequence := false;;
let f () = let g ~y = (raise Not_found : 'a) in
           if false then ((assert false : 'a); g ()) else g ()
let _ = Format.printf "%b@." (try f (); false with Not_found -> true)
[%%expect {|
- : unit = ()
val f : t -> y:'a -> 'b = <fun>
false
- : unit = ()
|}]
;;

Clflags.strict_sequence := true ;;
let f () = let g ~y = (raise Not_found : 'a) in
           if false then ((assert false : 'a); g ()) else g ()
let _ = Format.printf "%b@." (try f (); false with Not_found -> true)
[%%expect {|
- : unit = ()
val f : t -> unit = <fun>
true
- : unit = ()
|}]
