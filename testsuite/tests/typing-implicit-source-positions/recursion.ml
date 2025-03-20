(* TEST
   expect;
*)

type lexing_location = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
[%%expect {|
type lexing_location = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
|}]

let y = { pos_fname = ""
        ; pos_lnum = 0
        ; pos_bol = 0
        ; pos_cnum = -1 }
[%%expect {|
val y : lexing_location =
  {pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
|}]

let rec g ?(call_pos = [%call_pos]) i =
  if i < 0 then 0
  else g ~call_pos:y (i - 1)
[%%expect {|
Line 3, characters 19-20:
3 |   else g ~call_pos:y (i - 1)
                       ^
Error: The value "y" has type "lexing_location"
       but an expression was expected of type "lexing_location/2"
       Lines 1-6, characters 0-1:
         Definition of type "lexing_location"
       File "_none_", line 1:
         Definition of type "lexing_location/2"
|}]
