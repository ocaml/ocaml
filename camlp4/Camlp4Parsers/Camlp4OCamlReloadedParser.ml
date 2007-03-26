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
  value version = "$Id$";
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  Gram.Entry.clear match_case;

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
  end else ();

  EXTEND Gram
    GLOBAL: match_case match_case0 expr;

    match_case:
      [ [ OPT "|"; l = LIST1 match_case0 SEP "|"; "end" -> Ast.mcOr_of_list l
        | "end" -> <:match_case<>> ] ]
    ;

    expr: LEVEL "top"
      [ [ "if"; e1 = SELF; "then"; e2 = expr LEVEL "top";
          "else"; e3 = expr LEVEL "top"; "end" ->
            <:expr< if $e1$ then $e2$ else $e3$ >>
        | "if"; e1 = SELF; "then"; e2 = expr LEVEL "top"; "end" ->
            <:expr< if $e1$ then $e2$ else () >> ] ]
    ;
  END;

end;

let module M = Register.OCamlSyntaxExtension Id Make in ();
