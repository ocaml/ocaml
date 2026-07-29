(* TEST
{
  toplevel;
}{
  toplevel.opt;
}
*)

open Introspect.Print;;

print_any_endline 1;;
print_any_endline "Hello";;
print_any_endline [1;2;3];;
print_any_endline (let rec l = 1 :: l in l);;

module M = Map.Make(Int);;

print_any_endline (M.of_list [1, 1; 2, 2; 3, 3]);;
