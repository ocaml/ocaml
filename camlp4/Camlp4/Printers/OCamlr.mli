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

module Id : Sig.Id.S;

module Make (Syntax : Sig.Camlp4Syntax.S) : sig
  open Format;
  include Sig.Camlp4Syntax.S
           with module Loc     = Syntax.Loc
            and module Warning = Syntax.Warning
            and module Token   = Syntax.Token
            and module Ast     = Syntax.Ast
            and module Gram    = Syntax.Gram;

  (**
    [new printer ~curry_constr:c ~comments:False]
    Default values: curry_constr = True
                    comments = True
   *)
  class printer :
    [?curry_constr: bool] -> [?comments: bool] -> [unit] ->
  object ('a)
    inherit (OCaml.Make Syntax).printer;
  end;

  value with_outfile :
    option string -> (formatter -> 'a -> unit) -> 'a -> unit;

  value print :
    option string -> (printer -> formatter -> 'a -> unit) -> 'a -> unit;

  value print_interf :
    ?input_file: string -> ?output_file: string -> Ast.sig_item -> unit;

  value print_implem :
    ?input_file: string -> ?output_file: string -> Ast.str_item -> unit;
end;

module MakeMore (Syntax : Sig.Camlp4Syntax.S)
: Sig.Printer.S with module Ast = Syntax.Ast;
