(* TEST
 ocamlparam=",_,metaocaml-mode=0";
 toplevel;
*)

let specific_syntax_error = .~ x;;

print_endline "Valid syntax";;
let (>.): float -> float -> bool = Stdlib.(>);;
let _ = 1. >. 4.;;


print_endline "Syntax error on .<";;
let x  = .< y >. ;;


print_endline "Still valid syntax";;
let (>.): float -> float -> bool = Stdlib.(>)
let _ = 1. >. 4.;;
