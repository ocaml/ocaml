(* TEST
  flags="-log-format json";
  toplevel;
*)

let x = 1
let y = 2.;;

#shw List.hd;;

#show List.hd;;

let z = x + y;;

#trace List.map;;

let l = List.map succ [1;2;3];;
