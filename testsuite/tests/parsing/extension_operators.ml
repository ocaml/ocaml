(* TEST
   * expect
*)

let f o x = o##x;;
[%%expect {|
Line 1, characters 13-15:
1 | let f o x = o##x;;
                 ^^
Error: '##' is not a valid value identifier.
|}]

let f x = !#x
[%%expect {|
Line 1, characters 10-12:
1 | let f x = !#x
              ^^
Error: '!#' is not a valid value identifier.
|}]

let f x = ?#x
[%%expect {|
Line 1, characters 10-12:
1 | let f x = ?#x
              ^^
Error: '?#' is not a valid value identifier.
|}]

let f x = ~#x
[%%expect {|
Line 1, characters 10-12:
1 | let f x = ~#x
              ^^
Error: '~#' is not a valid value identifier.
|}]

let f o x = o#-#x
[%%expect {|
Line 1, characters 13-16:
1 | let f o x = o#-#x
                 ^^^
Error: '#-#' is not a valid value identifier.
|}]

let f x = !-#x
[%%expect {|
Line 1, characters 10-13:
1 | let f x = !-#x
              ^^^
Error: '!-#' is not a valid value identifier.
|}]

let f x = ?-#x
[%%expect {|
Line 1, characters 10-13:
1 | let f x = ?-#x
              ^^^
Error: '?-#' is not a valid value identifier.
|}]

let f x = ~-#x
[%%expect {|
Line 1, characters 10-13:
1 | let f x = ~-#x
              ^^^
Error: '~-#' is not a valid value identifier.
|}]
