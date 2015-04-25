module M = struct
  let x="mx"
  let x="x"
end;;

module N = struct
  let x="nx"
  let x="x"
end;;

(* value shadowing is authorized in sequential include *)
include M;;
include N;;
(* not in grouped include *)
include M and N;;

module F() = struct include M end;;
(* nested grouped include *)
include F() and F();; 
