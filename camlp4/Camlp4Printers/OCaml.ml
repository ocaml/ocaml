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

open Camlp4;

module Id = struct
  value name = "Camlp4.Printers.OCaml";
  value version = "$Id$";
end;

module Make (Syntax : Sig.Camlp4Syntax.S)
: Sig.Printer.S with module Ast = Syntax.Ast
= struct

  include Printers.OCaml.Make Syntax;

  value semisep = ref False;
  value margin = ref 78;
  value comments = ref True;
  value locations = ref False;
  value curry_constr = ref False;

  value print output_file fct =
    let o = new printer ~comments:comments.val
                        ~curry_constr:curry_constr.val () in
    let o = if semisep.val then o#set_semisep ";;" else o#set_semisep "" in
    let o = if locations.val then o#set_loc_and_comments else o in
    with_outfile output_file
      (fun f ->
        let () = Format.pp_set_margin f margin.val in
        Format.fprintf f "@[<v0>%a@]@." (fct o));

  value print_interf ?input_file:(_) ?output_file sg =
    print output_file (fun o -> o#interf) sg;

  value print_implem ?input_file:(_) ?output_file st =
    print output_file (fun o -> o#implem) st;

  Options.add "-l" (Arg.Int (fun i -> margin.val := i))
    "<length> line length for pretty printing.";

  Options.add "-ss" (Arg.Set semisep) "Print double semicolons.";

  Options.add "-curry-constr" (Arg.Set curry_constr) "Use currified constructors.";

  Options.add "-no_ss" (Arg.Clear semisep)
    "Do not print double semicolons (default).";

  Options.add "-no_comments" (Arg.Clear comments) "Do not add comments.";

  Options.add "-add_locations" (Arg.Set locations) "Add locations as comment.";

end;

let module M = Register.OCamlPrinter Id Make in ();
