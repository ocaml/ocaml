(* TEST
   readonly_files = "error_highlighting_use1.ml \
                     error_highlighting_use2.ml \
                     error_highlighting_use3.ml \
                     error_highlighting_use4.ml"
   * toplevel
*)

let x = (1 + 2) +. 3. in ();;

let x = (1 + 2 in ();;

let x = (1 + 2;;

let x = 1 in
let y = 1 +. 2. in
();;

let x = (1
  +
2 in
();;

let x = (1
  +
2) +.
3. in ();;

let x = 1 + "abc" in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in
let x = 1 in ();;

#use "error_highlighting_use1.ml";;
#use "error_highlighting_use2.ml";;
#use "error_highlighting_use3.ml";;
#use "error_highlighting_use4.ml";;
