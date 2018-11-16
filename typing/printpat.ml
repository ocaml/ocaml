(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Values as patterns pretty printer *)

open Asttypes
open Typedtree
open Types
open Format

let is_cons = function
| {cstr_name = "::"} -> true
| _ -> false

let pretty_const c = match c with
| Const_int i -> Printf.sprintf "%d" i
| Const_char c -> Printf.sprintf "%C" c
| Const_string (s, _) -> Printf.sprintf "%S" s
| Const_float f -> Printf.sprintf "%s" f
| Const_int32 i -> Printf.sprintf "%ldl" i
| Const_int64 i -> Printf.sprintf "%LdL" i
| Const_nativeint i -> Printf.sprintf "%ndn" i

let rec pretty_val ppf v =
  match v.pat_extra with
      (cstr, _loc, _attrs) :: rem ->
        begin match cstr with
          | Tpat_unpack ->
            fprintf ppf "@[(module %a)@]" pretty_val { v with pat_extra = rem }
          | Tpat_constraint _ ->
            fprintf ppf "@[(%a : _)@]" pretty_val { v with pat_extra = rem }
          | Tpat_type _ ->
            fprintf ppf "@[(# %a)@]" pretty_val { v with pat_extra = rem }
          | Tpat_open _ ->
              fprintf ppf "@[(# %a)@]" pretty_val { v with pat_extra = rem }
        end
    | [] ->
  match v.pat_desc with
  | Tpat_any -> fprintf ppf "_"
  | Tpat_var (x,_) -> fprintf ppf "%s" (Ident.name x)
  | Tpat_constant c -> fprintf ppf "%s" (pretty_const c)
  | Tpat_tuple vs ->
      fprintf ppf "@[(%a)@]" (pretty_vals ",") vs
  | Tpat_construct (_, cstr, []) ->
      fprintf ppf "%s" cstr.cstr_name
  | Tpat_construct (_, cstr, [w]) ->
      fprintf ppf "@[<2>%s@ %a@]" cstr.cstr_name pretty_arg w
  | Tpat_construct (_, cstr, vs) ->
      let name = cstr.cstr_name in
      begin match (name, vs) with
        ("::", [v1;v2]) ->
          fprintf ppf "@[%a::@,%a@]" pretty_car v1 pretty_cdr v2
      |  _ ->
          fprintf ppf "@[<2>%s@ @[(%a)@]@]" name (pretty_vals ",") vs
      end
  | Tpat_variant (l, None, _) ->
      fprintf ppf "`%s" l
  | Tpat_variant (l, Some w, _) ->
      fprintf ppf "@[<2>`%s@ %a@]" l pretty_arg w
  | Tpat_record (lvs,_) ->
      let filtered_lvs = List.filter
          (function
            | (_,_,{pat_desc=Tpat_any}) -> false (* do not show lbl=_ *)
            | _ -> true) lvs in
      begin match filtered_lvs with
      | [] -> fprintf ppf "_"
      | (_, lbl, _) :: q ->
          let elision_mark ppf =
            (* we assume that there is no label repetitions here *)
             if Array.length lbl.lbl_all > 1 + List.length q then
               fprintf ppf ";@ _@ "
             else () in
          fprintf ppf "@[{%a%t}@]"
            pretty_lvals filtered_lvs elision_mark
      end
  | Tpat_array vs ->
      fprintf ppf "@[[| %a |]@]" (pretty_vals " ;") vs
  | Tpat_lazy v ->
      fprintf ppf "@[<2>lazy@ %a@]" pretty_arg v
  | Tpat_exception v ->
      fprintf ppf "@[<2>exception@ %a@]" pretty_arg v
  | Tpat_alias (v, x,_) ->
      fprintf ppf "@[(%a@ as %a)@]" pretty_val v Ident.print x
  | Tpat_or (v,w,_)    ->
      fprintf ppf "@[(%a|@,%a)@]" pretty_or v pretty_or w

and pretty_car ppf v = match v.pat_desc with
| Tpat_construct (_,cstr, [_ ; _])
    when is_cons cstr ->
      fprintf ppf "(%a)" pretty_val v
| _ -> pretty_val ppf v

and pretty_cdr ppf v = match v.pat_desc with
| Tpat_construct (_,cstr, [v1 ; v2])
    when is_cons cstr ->
      fprintf ppf "%a::@,%a" pretty_car v1 pretty_cdr v2
| _ -> pretty_val ppf v

and pretty_arg ppf v = match v.pat_desc with
| Tpat_construct (_,_,_::_)
| Tpat_variant (_, Some _, _) -> fprintf ppf "(%a)" pretty_val v
|  _ -> pretty_val ppf v

and pretty_or ppf v = match v.pat_desc with
| Tpat_or (v,w,_) ->
    fprintf ppf "%a|@,%a" pretty_or v pretty_or w
| _ -> pretty_val ppf v

and pretty_vals sep ppf = function
  | [] -> ()
  | [v] -> pretty_val ppf v
  | v::vs ->
      fprintf ppf "%a%s@ %a" pretty_val v sep (pretty_vals sep) vs

and pretty_lvals ppf = function
  | [] -> ()
  | [_,lbl,v] ->
      fprintf ppf "%s=%a" lbl.lbl_name pretty_val v
  | (_, lbl,v)::rest ->
      fprintf ppf "%s=%a;@ %a"
        lbl.lbl_name pretty_val v pretty_lvals rest

let top_pretty ppf v =
  fprintf ppf "@[%a@]@?" pretty_val v


let pretty_pat p =
  top_pretty Format.str_formatter p ;
  prerr_string (Format.flush_str_formatter ())

type matrix = pattern list list

let pretty_line fmt =
  List.iter (fun p ->
    Format.fprintf fmt " <";
    top_pretty fmt p;
    Format.fprintf fmt ">";
  )

let pretty_matrix fmt (pss : matrix) =
  Format.fprintf fmt "begin matrix\n" ;
  List.iter (fun ps ->
    pretty_line fmt ps ;
    Format.fprintf fmt "\n"
  ) pss;
  Format.fprintf fmt "end matrix\n%!"
