open Camlp4;                                             (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

module Id = struct
  value name = "Camlp4DebugParser";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  module StringSet = Set.Make String;

  value debug_mode =
    try
      let str = Sys.getenv "STATIC_CAMLP4_DEBUG" in
      let rec loop acc i =
        try
          let pos = String.index_from str i ':' in
          loop (StringSet.add (String.sub str i (pos - i)) acc) (pos + 1)
        with
        [ Not_found ->
            StringSet.add (String.sub str i (String.length str - i)) acc ] in
      let sections = loop StringSet.empty 0 in
      if StringSet.mem "*" sections then fun _ -> True
      else fun x -> StringSet.mem x sections 
    with [ Not_found -> fun _ -> False ];

  value rec apply accu =
    fun
    [ [] -> accu
    | [x :: xs] ->
        let _loc = Ast.loc_of_expr x
        in apply <:expr< $accu$ $x$ >> xs ];

  value mk_debug_mode _loc = fun [ None -> <:expr< Debug.mode >>
                                 | Some m -> <:expr< $uid:m$.Debug.mode >> ];

  value mk_debug _loc m fmt section args =
    let call = apply <:expr< Debug.printf $str:section$ $str:fmt$ >> args in
      <:expr< if $mk_debug_mode _loc m$ $str:section$ then $call$ else () >>;

  EXTEND Gram
    GLOBAL: expr;
    expr:
    [ [ m = start_debug; section = LIDENT; fmt = STRING;
        args = LIST0 expr LEVEL "."; x = end_or_in ->
      match (x, debug_mode section) with
      [ (None,   False) -> <:expr< () >>
      | (Some e, False) -> e
      | (None, _) -> mk_debug _loc m fmt section args
      | (Some e, _) -> <:expr< let () = $mk_debug _loc m fmt section args$ in $e$ >> ]
    ] ];
    end_or_in:
    [ [ "end" -> None
      | "in"; e = expr -> Some e
    ] ];
    start_debug:
    [ [ LIDENT "debug" -> None
      | LIDENT "camlp4_debug" -> Some "Camlp4"
    ] ];
  END;

end;

let module M = Register.OCamlSyntaxExtension Id Make in ();
