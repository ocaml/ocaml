(* Tests for matchings on integers and characters *)

(* Dense integer switch *)

let f = function 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4 | 5 -> 5 | 6 -> 6 | _ -> 0

(* Sparse integer switch *)

let g = function 303 -> 1 | 401 -> 2 | _ -> 0

(* Very sparse integer switch *)

let iszero = function 0 -> true | _ -> false

(* Simple matching on characters *)

let h = function
    'a' -> "a"
  | 'e' -> "e"
  | 'i' -> "i"
  | 'o' -> "o"
  | 'u' -> "u"
  | _ -> "?"

(* Matching with orpats *)

let k = function
    ' ' | '\t' | '\n' | '\r' -> "blank"
  | 'A'..'Z' | 'a'..'z' | '\192'..'\255' -> "letter"
  | '0'..'9' -> "digit"
  | '!'|'%'|'&'|'$'|'#'|'+'|'/'|':'|'<'|'='|'>'|'?'|'@'|'\\'|
             '~'|'^'|'|'|'*' -> "operator"
  | _ -> "other"

(* The test *)

open Printf

let _ =
  for i = -5 to 10 do printf "f(%d) = %d\n" i (f i) done;
  List.iter (fun i -> printf "g(%d) = %d\n" i (g i))
            [0;300;303;305;400;401;402;999];
  for i = -2 to 2 do printf "iszero(%d) = %b\n" i (iszero i) done;
  for i = 97 to 126 do
    let c = Char.chr i in
    printf "h(%c) = %s\n" c (h c)
  done;
  for i = 0 to 255 do
    let c = Char.chr i in
    printf "k(%s) = %s\n" (Char.escaped c) (k c)
  done;
  exit 0



