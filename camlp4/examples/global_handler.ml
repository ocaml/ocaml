(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006  Institut  National  de  Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

open Camlp4.PreCast;

value ghost = Loc.ghost;

value global_handler_ref = ref <:expr@ghost<>>;

value find_global_handler =
  Ast.map_str_item begin
    fun
    [ <:str_item@_loc< value global_handler = $f$ >> ->
        (global_handler_ref.val := f; <:str_item<>>)
    | st -> st ]
  end;

AstFilters.register_str_item_filter begin fun st ->
  let _ = find_global_handler#str_item st in
  <:str_item@ghost< try let module Main = struct $st$ end in ()
                    with e -> $global_handler_ref.val$ e >>
end;
