(* TEST
   * toplevel
*)

(* Horrible hack; there should be a way to do that... *)
#directory"../../../../../../../../utils";;
#directory"../../../../../../../../parsing";;
#directory"../../../../../../../../compilerlibs";;
#load"ocamlcommon.cma";;

Parse.expression (Lexing.from_string "1");;
