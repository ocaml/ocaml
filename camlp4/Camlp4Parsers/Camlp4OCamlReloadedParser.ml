open Camlp4;                                             (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007  Institut  National  de  Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: Original version
 *)

module Id = struct
  value name = "Camlp4Reloaded";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  Gram.Entry.clear match_case;
  Gram.Entry.clear semi;

  value mkseq _loc =
    fun
    [ <:expr< $_$; $_$ >> as e -> <:expr< do { $e$ } >>
    | e -> e ]
  ;

  DELETE_RULE Gram match_case0: patt_as_patt_opt; opt_when_expr; "->"; expr END;

  value revised =
    try
      (DELETE_RULE Gram expr: "if"; SELF; "then"; SELF; "else"; SELF END; True)
    with [ Not_found -> begin
      DELETE_RULE Gram expr: "if"; SELF; "then"; expr LEVEL "top"; "else"; expr LEVEL "top" END;
      DELETE_RULE Gram expr: "if"; SELF; "then"; expr LEVEL "top" END; False
    end ];

  if revised then begin
    DELETE_RULE Gram expr: "fun"; "["; LIST0 match_case0 SEP "|"; "]" END;
    EXTEND Gram
      expr: LEVEL "top"
      [ [ "function"; a = match_case -> <:expr< fun [ $a$ ] >> ] ];
    END;
    DELETE_RULE Gram value_let: "value" END;
    DELETE_RULE Gram value_val: "value" END;
  end else begin
    DELETE_RULE Gram value_let: "let" END;
    DELETE_RULE Gram value_val: "val" END;
  end;

  EXTEND Gram
    GLOBAL: match_case match_case0 expr value_let value_val semi;

    match_case:
      [ [ OPT "|"; l = LIST1 match_case0 SEP "|"; "end" -> Ast.mcOr_of_list l
        | "end" -> <:match_case<>> ] ]
    ;

    match_case0:
      [ [ p = patt_as_patt_opt; w = opt_when_expr; "->"; e = sequence ->
            <:match_case< $p$ when $w$ -> $mkseq _loc e$ >> ] ]
    ;

    expr: LEVEL "top"
      [ [ "if"; e1 = sequence; "then"; e2 = sequence; "else"; e3 = sequence; "end" ->
            <:expr< if $mkseq _loc e1$ then $mkseq _loc e2$ else $mkseq _loc e3$ >>
        | "if"; e1 = sequence; "then"; e2 = sequence; "end" ->
            <:expr< if $mkseq _loc e1$ then $mkseq _loc e2$ else () >> ] ]
    ;

    value_let:
      [ [ "val" -> () ] ]
    ;
    value_val:
      [ [ "val" -> () ] ]
    ;
    semi:
      [ [ ";;" -> () | ";" -> () | -> () ] ]
    ;
  END;

end;

let module M = Register.OCamlSyntaxExtension Id Make in ();
