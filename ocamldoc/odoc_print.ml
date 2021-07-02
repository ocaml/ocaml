(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format
let () = Printtyp.Naming_context.enable false

let new_fmt () =
  let buf = Buffer.create 512 in
  let fmt = formatter_of_buffer buf in
  let flush () =
    pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.reset buf ;
    s
  in
  (fmt, flush)

let (type_fmt, flush_type_fmt) = new_fmt ()
let _ =
  let outfuns = pp_get_formatter_out_functions type_fmt () in
  pp_set_formatter_out_functions type_fmt
    {outfuns with out_newline = fun () -> outfuns.out_string "\n  " 0 3}

let (modtype_fmt, flush_modtype_fmt) = new_fmt ()




let string_of_type_expr t =
  Printtyp.shared_type_scheme type_fmt t;
  flush_type_fmt ()

exception Use_code of string

(** Return the given module type where methods and vals have been removed
   from the signatures. Used when we don't want to print a too long module type.
   @param code when the code is given, we raise the [Use_code] exception if we
   encounter a signature, so that the calling function can use the code rather
   than the "emptied" type.
*)
let simpl_module_type ?code t =
  let open Types in
  let rec iter t =
    match t with
      Mty_ident _
    | Mty_alias _ -> t
    | Mty_signature _ ->
        (
         match code with
           None -> Mty_signature []
         | Some s -> raise (Use_code s)
        )
    | Mty_functor (Unit, mt) -> Mty_functor (Unit, iter mt)
    | Mty_functor (Named (name, mt1), mt2) ->
      Mty_functor (Named (name, iter mt1), iter mt2)
  in
  iter t

let string_of_module_type ?code ?(complete=false) t =
  try
    let t2 = if complete then t else simpl_module_type ?code t in
    Printtyp.modtype modtype_fmt t2;
    flush_modtype_fmt ()
  with
    Use_code s -> s

(** Return the given class type where methods and vals have been removed
   from the signatures. Used when we don't want to print a too long class type.*)
let simpl_class_type t =
  let rec iter t =
    let open Types in
    match t with
      Cty_constr _ -> t
    | Cty_signature cs ->
        (* we delete vals and methods in order to not print them when
           displaying the type *)
      let self_row =
        Transient_expr.create Tnil
          ~level:0 ~scope:Btype.lowest_level ~id:0
      in
      let tself =
        let t = cs.csig_self in
        let desc = Tobject (Transient_expr.type_expr self_row, ref None) in
        Transient_expr.create desc
          ~level:(get_level t) ~scope:(get_scope t) ~id:(get_id t)
      in
        Types.Cty_signature { csig_self = Transient_expr.type_expr tself;
                              csig_self_row = Transient_expr.type_expr self_row;
                              csig_vars = Vars.empty ;
                              csig_meths = Meths.empty ; }
    | Types.Cty_arrow (l, texp, ct) ->
        let new_ct = iter ct in
        Cty_arrow (l, texp, new_ct)
  in
  iter t

let string_of_class_type ?(complete=false) t =
  let t2 = if complete then t else simpl_class_type t in
  (* FIXME : my own Printtyp.class_type variant to avoid reset_names *)
  Printtyp.class_type modtype_fmt t2;
  flush_modtype_fmt ()
