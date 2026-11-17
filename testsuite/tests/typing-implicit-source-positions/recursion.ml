(* TEST
   expect;
*)

type textloc = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
[%%expect {|
type textloc = {
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
val y : textloc = {pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
|}]

let rec g ?(call_loc = [%call_loc]) i =
  if i < 0 then 0
  else g ~call_loc:y (i - 1)
[%%expect {|
Line 3, characters 19-20:
3 |   else g ~call_loc:y (i - 1)
                       ^
Error: The value "y" has type "textloc" but an expression was expected of type
         "textloc/2"
       Lines 1-6, characters 0-1:
         Definition of type "textloc"
       File "_none_", line 1:
         Definition of type "textloc/2"
|}]
