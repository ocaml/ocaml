open Camlp4.PreCast.Syntax;

value mo _loc =
  fun
  [ None -> <:expr< None >>
  | Some e -> <:expr< Some $e$ >> ];

EXTEND Gram
GLOBAL: expr;
expr:
  [ [ "testbegin";
      lb = [ "("; l = LIST0 a_LIDENT SEP ","; ")" -> l | "()" -> [] ];
      b = bar;
      "testend" ->
        let e =
        List.fold_right (fun i acc -> <:expr< [ $lid:i$ :: $acc$ ] >>) lb <:expr< [] >>
        in <:expr< ($e$, $b$) >>
  ] ];
bar:
  [ [ x = OPT [ o = OPT [ x = "testb" ->
        <:expr< $str:Token.extract_string x$ >> ]; "testc"; b = baz ->
        <:expr< ($mo _loc o$, $b$) >> ] -> mo _loc x
  ] ];
(* bar:
  [ [ o = OPT [ o = OPT [ "bar" -> <:expr< bar >> ]; b = baz -> <:expr< ($mo _loc o$, $b$) >> ] ->
      mo _loc o
  ] ];                                                                                                *)
(* bar:
  [ [ o = OPT [ "bar" -> <:expr< bar >> ]; b = baz -> <:expr< ($mo _loc o$, $b$) >>
  ] ];                                                                                 *)
baz:
  [ [ "baz" -> <:expr< baz >> ] ];
END;
