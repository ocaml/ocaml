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



module Make (Ast : Sig.Camlp4Ast)
: Sig.Quotation with module Ast = Ast
= struct
  module Ast = Ast;
  module DynAst = DynAst.Make Ast;
  module Loc = Ast.Loc;
  open Format;
  open Sig;

  type expand_fun 'a = Loc.t -> option string -> string -> 'a;

  module Exp_key = DynAst.Pack(struct
    type t 'a = unit;
  end);

  module Exp_fun = DynAst.Pack(struct
    type t 'a = expand_fun 'a;
  end);

  value expanders_table =
    (ref [] : ref (list ((string * Exp_key.pack) * Exp_fun.pack)));

  value default = ref "";
  value translate = ref (fun x -> x);

  value expander_name name =
    match translate.val name with
    [ "" -> default.val
    | name -> name ];

  value find name tag =
    let key = (expander_name name, Exp_key.pack tag ()) in
    Exp_fun.unpack tag (List.assoc key expanders_table.val);

  value add name tag f =
    let elt = ((name, Exp_key.pack tag ()), Exp_fun.pack tag f) in
    expanders_table.val := [elt :: expanders_table.val];

  value dump_file = ref None;

  module Error = struct
    type error =
      [ Finding
      | Expanding
      | ParsingResult of Loc.t and string
      | Locating ];
    type t = (string * string * error * exn);
    exception E of t;

    value print ppf (name, position, ctx, exn) =
      let name = if name = "" then default.val else name in
      let pp x = fprintf ppf "@?@[<2>While %s %S in a position of %S:" x name position in
      let () =
        match ctx with
        [ Finding -> begin
            pp "finding quotation";
            if expanders_table.val = [] then
              fprintf ppf "@ There is no quotation expander available."
            else
              begin
                fprintf ppf "@ @[<hv2>Available quotation expanders are:@\n";
                List.iter begin fun ((s,t),_) ->
                  fprintf ppf "@[<2>%s@ (in@ a@ position@ of %a)@]@ "
                    s Exp_key.print_tag t
                end expanders_table.val;
                fprintf ppf "@]"
              end
          end
        | Expanding -> pp "expanding quotation"
        | Locating -> pp "parsing"
        | ParsingResult loc str ->
          let () = pp "parsing result of quotation" in
          match dump_file.val with
          [ Some dump_file ->
              let () = fprintf ppf " dumping result...\n" in
              try
                let oc = open_out_bin dump_file in
                begin
                  output_string oc str;
                  output_string oc "\n";
                  flush oc;
                  close_out oc;
                  fprintf ppf "%a:" Loc.print (Loc.set_file_name dump_file loc);
                end
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

  value expand_quotation loc expander pos_tag quot =
    debug quot "expand_quotation: name: %s, str: %S@." quot.q_name quot.q_contents in
    let loc_name_opt = if quot.q_loc = "" then None else Some quot.q_loc in
    try expander loc loc_name_opt quot.q_contents with
    [ Loc.Exc_located _ (Error.E _) as exc ->
        raise exc
    | Loc.Exc_located iloc exc ->
        let exc1 = Error.E (quot.q_name, pos_tag, Expanding, exc) in
        raise (Loc.Exc_located iloc exc1)
    | exc ->
        let exc1 = Error.E (quot.q_name, pos_tag, Expanding, exc) in
        raise (Loc.Exc_located loc exc1) ];

  value parse_quotation_result parse loc quot pos_tag str =
    try parse loc str with
    [ Loc.Exc_located iloc (Error.E (n, pos_tag, Expanding, exc)) ->
        let ctx = ParsingResult iloc quot.q_contents in
        let exc1 = Error.E (n, pos_tag, ctx, exc) in
        raise (Loc.Exc_located iloc exc1)
    | Loc.Exc_located iloc (Error.E _ as exc) ->
        raise (Loc.Exc_located iloc exc)
    | Loc.Exc_located iloc exc ->
        let ctx = ParsingResult iloc quot.q_contents in
        let exc1 = Error.E (quot.q_name, pos_tag, ctx, exc) in
        raise (Loc.Exc_located iloc exc1) ];

  value expand loc quotation tag =
    let pos_tag = DynAst.string_of_tag tag in
    let name = quotation.q_name in
    debug quot "handle_quotation: name: %s, str: %S@." name quotation.q_contents in
    let expander =
      try find name tag
      with
      [ Loc.Exc_located _ (Error.E _) as exc -> raise exc
      | Loc.Exc_located qloc exc ->
          raise (Loc.Exc_located qloc (Error.E (name, pos_tag, Finding, exc)))
      | exc ->
          raise (Loc.Exc_located loc (Error.E (name, pos_tag, Finding, exc))) ]
    in
    let loc = Loc.join (Loc.move `start quotation.q_shift loc) in
    expand_quotation loc expander pos_tag quotation;

end;
