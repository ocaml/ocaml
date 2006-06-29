open Camlp4.PreCast;
module G = MakeGram Lexer;
value main = G.Entry.mk "main";
EXTEND G
  GLOBAL: main;
  main:
    [ [ l = LIST1 ident -> l ] ];
  ident:
    [ [ `LIDENT s -> s ] ];
END;
let f = Sys.argv.(1) in
Format.printf "%d@."
  (List.length (G.parse main (Loc.mk f) (Stream.of_channel (open_in f))));
