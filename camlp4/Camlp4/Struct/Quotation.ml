(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)


(* $Id$ *)

module Make (Ast : Sig.Ast.S)
: Sig.Quotation.S with module Ast = Ast
= struct
  module Ast = Ast;
  module Loc = Ast.Loc;
  open Format;
  open Sig.Quotation;

  type expand_fun 'a = Loc.t -> option string -> string -> 'a;

  type expander =
    [ ExStr of bool -> expand_fun string
    | ExAst of (expand_fun Ast.expr) and (expand_fun Ast.patt) ];

  value expanders_table = ref [];

  value default = ref "";
  value translate = ref (fun x -> x);

  value expander_name name =
    match translate.val name with
    [ "" -> default.val
    | name -> name ];

  value find name = List.assoc (expander_name name) expanders_table.val;

  value add name f = expanders_table.val := [(name, f) :: expanders_table.val];

  value dump_file = ref None;

  module Error = struct
    type error =
      [ Finding
      | Expanding
      | ParsingResult of Loc.t and string
      | Locating ];
    type t = (string * error * exn);
    exception E of t;

    value print ppf (name, ctx, exn) =
      let name = if name = "" then default.val else name in
      let pp x = fprintf ppf "@?@[<2>While %s %S:" x name in
      let () =
        match ctx with
        [ Finding -> do {
            pp "finding quotation";
            fprintf ppf " available quotations are:\n@[<2>";
            List.iter (fun (s,_) -> fprintf ppf "%s@ " s) expanders_table.val;
            fprintf ppf "@]"
          } 
        | Expanding -> pp "expanding quotation"
        | Locating -> pp "parsing"
        | ParsingResult loc str ->
          let () = pp "parsing result of quotation" in
          match dump_file.val with
          [ Some dump_file ->
              let () = fprintf ppf " dumping result...\n" in
              try
                let oc = open_out_bin dump_file in
                do {
                  output_string oc str;
                  output_string oc "\n";
                  flush oc;
                  close_out oc;
                  fprintf ppf "%a:" Loc.print (Loc.set_file_name dump_file loc);
                }
              with _ ->
                fprintf ppf
                  "Error while dumping result in file %S; dump aborted"
                  dump_file
          | None ->
              fprintf ppf
                "\n(consider setting variable Quotation.dump_file, or using the -QD option)"
          ]
        ]
      in fprintf ppf "@\n%a@]@." ErrorHandler.print exn;

    value to_string x =
      let b = Buffer.create 50 in
      let () = bprintf b "%a" print x in Buffer.contents b;
  end;
  let module M = ErrorHandler.Register Error in ();
  open Error;

  value expand_quotation loc expander quot =
    debug quot "expand_quotation: name: %s, str: %S@." quot.q_name quot.q_contents in
    let loc_name_opt = if quot.q_loc = "" then None else Some quot.q_loc in
    try expander loc loc_name_opt quot.q_contents with
    [ Loc.Exc_located _ (Error.E _) as exc ->
        raise exc
    | Loc.Exc_located iloc exc ->
        let exc1 = Error.E (quot.q_name, Expanding, exc) in
        raise (Loc.Exc_located iloc exc1)
    | exc ->
        let exc1 = Error.E (quot.q_name, Expanding, exc) in
        raise (Loc.Exc_located loc exc1) ];

  value parse_quotation_result parse loc quot str =
    try parse loc str with
    [ Loc.Exc_located iloc (Error.E (n, Expanding, exc)) ->
        let ctx = ParsingResult iloc quot.q_contents in
        let exc1 = Error.E (n, ctx, exc) in
        raise (Loc.Exc_located iloc exc1)
    | Loc.Exc_located iloc (Error.E _ as exc) ->
        raise (Loc.Exc_located iloc exc)
    | Loc.Exc_located iloc exc ->
        let ctx = ParsingResult iloc quot.q_contents in
        let exc1 = Error.E (quot.q_name, ctx, exc) in
        raise (Loc.Exc_located iloc exc1) ];

  value handle_quotation loc proj in_expr parse quotation =
    let name = quotation.q_name in
    debug quot "handle_quotation: name: %s, str: %S@." name quotation.q_contents in
    let expander =
      try find name
      with
      [ Loc.Exc_located _ (Error.E _) as exc -> raise exc
      | Loc.Exc_located qloc exc ->
          raise (Loc.Exc_located qloc (Error.E (name, Finding, exc)))
      | exc ->
          raise (Loc.Exc_located loc (Error.E (name, Finding, exc))) ]
    in
    let loc = Loc.join (Loc.move `start quotation.q_shift loc) in
    match expander with
    [ ExStr f ->
        let new_str = expand_quotation loc (f in_expr) quotation in
        parse_quotation_result parse loc quotation new_str
    | ExAst fe fp ->
        expand_quotation loc (proj (fe, fp)) quotation ];

  value expand_expr parse loc x =
    handle_quotation loc fst True parse x;

  value expand_patt parse loc x =
    handle_quotation loc snd False parse x;

end;
