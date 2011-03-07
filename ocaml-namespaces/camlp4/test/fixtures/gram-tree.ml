open Camlp4.PreCast;
module G = MakeGram Lexer;
type t = [ A of t and t | B of string ];
value main = G.Entry.mk "main";
(* value rec length x acc =
  match x with
  [ A x y -> length x (length y acc)
  | B _ -> succ acc ];                  *)
value length _ _ = -1;
EXTEND G
  GLOBAL: main;
  main:
    [ [ x = SELF; y = SELF -> A x y
      | i = ident -> B i ] ];
  ident:
    [ [ `LIDENT s -> s ] ];
END;
try
  let f = Sys.argv.(1) in
  Format.printf "%d@."
    (length (G.parse main (Loc.mk f) (Stream.of_channel (open_in f))) 0)
with e -> Format.eprintf "error: %a@." Camlp4.ErrorHandler.print e;
