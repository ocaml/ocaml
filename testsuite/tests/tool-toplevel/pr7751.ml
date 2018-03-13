(* TEST
   * toplevel
*)

#directory"../../../../../../utils";;
#directory"../../../../../../parsing";;
#directory"../../../../../../compilerlibs";;
#load"ocamlcommon.cma";;

Parse.expression (Lexing.from_string "1");;
