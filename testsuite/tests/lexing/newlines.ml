(* TEST *)

let check ~kind ~input ~result =
  if input <> result then
    Printf.printf "FAIL: %s %S should normalize to %S
"
      kind input result
;;

check ~kind:"string literal" ~input:"
" ~result:"\n";
check ~kind:"quoted string literal" ~input:{|
|} ~result:"\n";

check ~kind:"string literal" ~input:"
" ~result:"\n";
check ~kind:"quoted string literal" ~input:{|
|} ~result:"\n";

check ~kind:"string literal" ~input:"
" ~result:"\r\n";
check ~kind:"quoted string literal" ~input:{|
|} ~result:"\r\n";
