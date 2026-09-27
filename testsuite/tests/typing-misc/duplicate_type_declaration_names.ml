(* TEST
 expect;
*)

type constructor_duplicates =
  | Duplicate
  | Duplicate
  | Duplicate
;;
[%%expect {|
Lines 1-4, characters 0-13:
1 | type constructor_duplicates =
2 |   | Duplicate
3 |   | Duplicate
4 |   | Duplicate
Error: Duplicate constructor name "Duplicate"
|}]

type record_label_duplicates = {
  duplicate : int;
  duplicate : string;
  duplicate : bool;
};;
[%%expect {|
File "_none_", line 1:
Error: Duplicate label name "duplicate"
|}]
