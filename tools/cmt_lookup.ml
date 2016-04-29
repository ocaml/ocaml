(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Nicolas Ojeda Bar, LexiFi                        *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree
open Location
open Cmt_format
open Lexing
open Asttypes

type pos =
  {
    line: int;
    column: int;
  }

type span =
  {
    start: pos;
    finish: pos;
  }

let after {line; column} pos =
  pos.pos_lnum < line || (pos.pos_lnum = line && pos.pos_cnum - pos.pos_bol <= column)

let before {line; column} pos =
  line < pos.pos_lnum || (line = pos.pos_lnum && column <= pos.pos_cnum - pos.pos_bol)

let inside {start; finish} loc =
  after start loc.loc_start && before finish loc.loc_end

let (<|>) o1 o2 span =
  match o1 span with
  | None -> o2 span
  | Some _ as r -> r

let none _ = None

let option f o span =
  match o with
  | None -> None
  | Some x -> f x span

let list f l =
  let rec loop = function
    | [] ->
        none
    | x :: l ->
        f x <|> loop l
  in
  loop l

let if_inside loc f (span : span) =
  if inside span loc then
    f span
  else
    None

let some loc x _ =
  Some (loc, x)

type definition =
  | External
  | Internal of Location.t
  | Global_variable of Location.t
  | Local_variable of Location.t

let let_scope s scope =
  if scope <> Location.none then
    match scope.loc_end with
    | {pos_lnum = 1; pos_bol = 0; pos_cnum = -1; _} ->
        Some (s.txt, Global_variable scope)
    | _ ->
        Some (s.txt, Local_variable scope)
    else
  else
    None

let rebuild_env = ref false

let print_type_scheme env typ =
  Printtyp.wrap_printing_env env (fun () -> Format.asprintf "%a" Printtyp.type_scheme typ)

let print_modtype env modtyp =
  Printtyp.wrap_printing_env env (fun () -> Format.asprintf "%a" Printtyp.modtype modtyp)

let print_class_type env cltype =
  Printtyp.wrap_printing_env env (fun () -> Format.asprintf "%a" Printtyp.class_type cltype)

let rec expression_desc loc env = function
  | Texp_ident (path, _, {Types.val_type; val_loc; _}) ->
      begin try
        let full_name = Path.name ~paren:Oprint.parenthesized_ident path in
        let env =
          if !rebuild_env then
            try
              Env.env_of_only_summary Envaux.env_from_summary env
            with Envaux.Error _ ->
              env
          else
            env
        in
        let annot =
          try
            if val_loc.loc_ghost then
              External
            else
              Internal val_loc
          with Not_found ->
            External
        in
        some loc (print_type_scheme env val_type, Some (full_name, annot))
      with _ ->
        none
      end
  | Texp_let (Recursive, vbs, e) ->
      list (value_binding loc) vbs <|> expression e
  | Texp_let (Nonrecursive, vbs, e) ->
      list (value_binding e.exp_loc) vbs <|> expression e
  | Texp_function (_, cases, _) ->
      list case cases
  | Texp_apply (e, args) ->
      expression e <|> list (fun (_, e) -> option expression e) args
  | Texp_match (e, cases, cases', _) ->
      expression e <|> list case cases <|> list case cases'
  | Texp_try (e, cases) ->
      expression e <|> list case cases
  | Texp_tuple el
  | Texp_construct (_, _, el) ->
      list expression el
  | Texp_variant (_, e) ->
      option expression e
  | Texp_record (fields, e) ->
      list (fun (_, _, e) -> expression e) fields <|> option expression e
  | Texp_field (e, _, _) ->
      expression e
  | Texp_setfield (e1, _, _, e2) ->
      expression e1 <|> expression e2
  | Texp_array el ->
      list expression el
  | Texp_ifthenelse (e1, e2, e3) ->
      expression e1 <|> expression e2 <|> option expression e3
  | Texp_sequence (e1, e2)
  | Texp_while (e1, e2) ->
      expression e1 <|> expression e2
  | Texp_for (_, _, e1, e2, _, e3) ->
      expression e1 <|> expression e2 <|> expression e3
  | Texp_send (e1, _, e2) ->
      expression e1 <|> option expression e2
  | Texp_letmodule (_, _, me, e) ->
      module_expr me <|> expression e
  | Texp_assert e
  | Texp_lazy e ->
      expression e
  | Texp_object (ce, _) ->
      class_structure ce
  | Texp_pack me ->
      module_expr me
  | _ ->
      none

and expression {exp_loc; exp_env; exp_type; exp_desc; _} =
  let default = some exp_loc (print_type_scheme exp_env exp_type, None) in
  if_inside exp_loc (expression_desc exp_loc exp_env exp_desc <|> default)

and value_binding scope {vb_pat; vb_expr; vb_loc; _} =
  if_inside vb_loc (pattern scope vb_pat <|> expression vb_expr)

and class_expr {cl_desc; cl_loc; cl_env; cl_type; _} =
  let default = some cl_loc (print_class_type cl_env cl_type, None) in
  if_inside cl_loc (class_expr_desc cl_desc <|> default)

and class_expr_desc = function
  | Tcl_ident _ ->
      none
  | Tcl_structure cs ->
      class_structure cs
  | Tcl_fun (_, p, args, ce, _) ->
      pattern ce.cl_loc p <|> list (fun (_, _, e) -> expression e) args <|> class_expr ce
  | Tcl_apply (ce, args) ->
      class_expr ce <|> list (fun (_, e) -> option expression e) args
  | Tcl_let (_, vbs, args, ce) ->
      list (value_binding ce.cl_loc) vbs <|> list (fun (_, _, e) -> expression e) args <|> class_expr ce
  | Tcl_constraint (ce, _, _, _, _) ->
      class_expr ce

and class_field {cf_desc; cf_loc; _} =
  if_inside cf_loc (class_field_desc cf_desc)

and class_field_desc = function
  | Tcf_inherit (_, ce, _, _, _) ->
      class_expr ce
  | Tcf_val (_, _, _, Tcfk_concrete (_, e), _)
  | Tcf_method (_, _, Tcfk_concrete (_, e))
  | Tcf_initializer e ->
      expression e
  | _ ->
      none

and class_structure {cstr_self; cstr_fields; _} =
  pattern Location.none cstr_self <|> list class_field cstr_fields (* Fix scope *)

and module_expr {mod_desc; mod_loc; mod_env; mod_type; _} =
  let default = some mod_loc (print_modtype mod_env mod_type, None) in
  if_inside mod_loc (module_expr_desc mod_loc mod_desc <|> default)

and module_expr_desc scope = function
  | Tmod_ident _ ->
      none
  | Tmod_structure str ->
      structure scope str
  | Tmod_functor (_, _, _, me) ->
      module_expr me
  | Tmod_apply (me1, me2, _) ->
      module_expr me1 <|> module_expr me2
  | Tmod_constraint (me, _, _, _) ->
      module_expr me
  | Tmod_unpack (e, _) ->
      expression e

and case {c_lhs; c_guard; c_rhs} =
  let scope =
    match c_guard with
    | None -> c_rhs.exp_loc
    | Some {exp_loc = {loc_start; _}; _} -> {c_rhs.exp_loc with loc_start}
  in
  pattern scope c_lhs <|> option expression c_guard <|> expression c_rhs

and pattern_desc scope loc env typ = function
  | Tpat_var (_, s) ->
      some loc (print_type_scheme env typ, let_scope s scope)
  | Tpat_alias (p, _, s) ->
      pattern scope p <|> if_inside s.loc (some loc (print_type_scheme env typ, let_scope s scope))
  | Tpat_tuple pl
  | Tpat_construct (_, _, pl) ->
      list (pattern scope) pl
  | Tpat_variant (_, Some p, _) ->
      pattern scope p
  | Tpat_record (fields, _) ->
      list (fun (_, _, p) -> pattern scope p) fields
  | Tpat_array pl ->
      list (pattern scope) pl
  | Tpat_or (p1, p2, _) ->
      pattern scope p1 <|> pattern scope p2
  | Tpat_lazy p ->
      pattern scope p
  | _ ->
      none

and pattern scope {pat_loc; pat_env; pat_type; pat_desc; _} =
  let default = some pat_loc (print_type_scheme pat_env pat_type, None) in
  if_inside pat_loc (pattern_desc scope pat_loc pat_env pat_type pat_desc <|> default)

and structure_item_desc scope loc = function
  | Tstr_eval (e, _) ->
      expression e
  | Tstr_value (Recursive, vbs) ->
      list (value_binding {scope with loc_start = loc.loc_start}) vbs
  | Tstr_value (Nonrecursive, vbs) ->
      list (value_binding scope) vbs
  | Tstr_module mb ->
      module_binding mb
  | Tstr_recmodule mbs ->
      list module_binding mbs
  | Tstr_class args ->
      list (fun ({ci_expr; _}, _) -> class_expr ci_expr) args
  | Tstr_include {incl_mod; _} ->
      module_expr incl_mod
  | _ ->
      none

and structure_item scope {str_desc; str_loc; _} =
  if_inside str_loc (structure_item_desc scope str_loc str_desc)

and module_binding {mb_expr; mb_loc; _} =
  if_inside mb_loc (module_expr mb_expr)

and structure scope {str_items; _} =
  let rec loop scope = function
    | si :: ({str_loc = {loc_start; _}; _} :: _ as rest) ->
        structure_item {scope with loc_start} si <|> loop scope rest
    | si :: [] ->
        structure_item scope si
    | [] ->
        none
  in
  loop scope str_items

let binary_part = function
  | Partial_structure str ->
      structure Location.none str
  | Partial_structure_item si ->
      structure_item Location.none si
  | Partial_expression e ->
      expression e
  | Partial_pattern p ->
      pattern Location.none p
  | Partial_class_expr ce ->
      class_expr ce
  | _ ->
      none

let binary_annots = function
  | Implementation typedtree ->
      structure Location.none typedtree
  | Partial_implementation binary_parts ->
      Array.fold_right (fun part rest -> binary_part part <|> rest) binary_parts none
  | _ ->
      none

let to_span loc =
  let line = loc.loc_start.pos_lnum in
  let column = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let start = {line; column} in
  let line = loc.loc_end.pos_lnum in
  let column = loc.loc_end.pos_cnum - loc.loc_end.pos_bol in
  let finish = {line; column} in
  {start; finish}

module Driver = struct
  type mode =
    | Get_type
    | Get_ident

  let mode = ref Get_type

  let set_mode t () = mode := t

  let spec =
    Arg.align
      [
        "-type", Arg.Unit (set_mode Get_type), " Query type";
        "-ident", Arg.Unit (set_mode Get_ident), " Query ident";
      ]

  let usage_msg =
    Printf.sprintf "Usage: %s [-type | -ident] <filename> <startline> <startcol> [<endline> <endcol>]"
      (Filename.basename Sys.argv.(0))

  let pp_loc oc loc =
    let {start = {line = l1; column = c1}; finish = {line = l2; column = c2}} = to_span loc in
    Printf.fprintf oc "%i %i %i %i" l1 c1 l2 c2

  let main () =
    let anon_args = ref [] in
    Arg.parse spec (fun s -> anon_args := s :: !anon_args) usage_msg;
    let filename, span =
      match List.rev !anon_args with
      | [fname; l1; c1] ->
          let start = {line = int_of_string l1; column = int_of_string c1} in
          let finish = start in
          fname, {start; finish}
      | [fname; l1; c1; l2; c2] ->
          let start = {line = int_of_string l1; column = int_of_string c1} in
          let finish = {line = int_of_string l2; column = int_of_string c2} in
          fname, {start; finish}
      | _ ->
          Printf.eprintf "Error: wrong number of arguments.\n%!";
          Arg.usage spec usage_msg;
          exit 2
    in
    let {cmt_loadpath; cmt_annots; cmt_use_summaries; _} = read_cmt filename in
    Envaux.reset_cache ();
    Config.load_path := cmt_loadpath;
    rebuild_env := cmt_use_summaries;
    let info = binary_annots cmt_annots span in
    match info, !mode with
    | None, _
    | Some (_, (_, None)), Get_ident ->
        Printf.printf "nil\n%!"
    | Some (loc, (s, _)), Get_type ->
        Printf.printf "(%a %S)\n%!" pp_loc loc s
    | Some (loc, (_, Some (full_name, kind))), Get_ident ->
        let pp_kind oc = function
          | External -> Printf.fprintf oc "external"
          | Internal loc -> Printf.fprintf oc "internal %a" pp_loc loc
          | Global_variable loc -> Printf.fprintf oc "global-variable %a" pp_loc loc
          | Local_variable loc -> Printf.fprintf oc "local-variable %a" pp_loc loc
        in
        Printf.printf "(%a %S %a)\n%!" pp_loc loc full_name pp_kind kind
end

let () =
  Driver.main ()
