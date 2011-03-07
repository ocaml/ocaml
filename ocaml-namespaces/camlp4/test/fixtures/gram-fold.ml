open Camlp4.PreCast;
module G = MakeGram Lexer;
type t = [ A of t and t | B of string | C ];
value main = G.Entry.mk "main";
value rec length x acc =
  match x with
  [ A x y -> length x (length y acc)
  | B _ -> succ acc
  | C -> acc ];
EXTEND G
  GLOBAL: main;
  main:
    [ [
      l = FOLD1 (fun a b -> A (B a) b) (C) ident -> l
  ] ];
  ident:
    [ [ `LIDENT s -> s ] ];
END;
let f = Sys.argv.(1) in
Format.printf "%d@."
  (length (G.parse main (Loc.mk f) (Stream.of_channel (open_in f))) 0);
