open Camlp4.PreCast;
module G = MakeGram Lexer;
(* type t = [ A of Loc.t and t and t | B of Loc.t and string ]; *)
value main = G.Entry.mk "main";
(* value rec length x acc =
  match x with
  [ A x y -> length x (length y acc)
  | B _ -> succ acc ];
value length _ _ = -1;                  *)
EXTEND G
  GLOBAL: main;
  main:
    [ RIGHTA
      [ x = SELF; y = SELF ->
        let l = Loc.merge x y in
        if l = _loc then _loc
        else do {
          Format.eprintf "bad loc: %a <> %a + %a@."
            Loc.dump _loc Loc.dump x Loc.dump y;
          _loc
        }
      | i = ident -> i ] ];
  ident:
    [ [ `LIDENT _ -> _loc ] ];
END;
try
  let f = Sys.argv.(1) in
  Format.printf "%a@."
    Loc.dump (G.parse main (Loc.mk f) (Stream.of_channel (open_in f)))
with e -> Format.eprintf "error: %a@." Camlp4.ErrorHandler.print e;
