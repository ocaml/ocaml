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

let debug = ref false

let with_timer name f =
  match !debug with
  | true ->
      let t0 = Sys.time () in
      let x = f () in
      let t1 = Sys.time () in
      let dt = t1 -. t0 in
      Printf.eprintf "%s: %f s\n%!" name dt;
      x
  | false ->
      f ()

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

type definition =
  | External
  | Internal of Location.t
  | Global_variable of Location.t
  | Local_variable of Location.t

type info = Location.t * (string * (string * definition) option)

exception Found of info

let after {line; column} pos =
  pos.pos_lnum < line || (pos.pos_lnum = line && pos.pos_cnum - pos.pos_bol <= column)

let before {line; column} pos =
  line < pos.pos_lnum || (line = pos.pos_lnum && column <= pos.pos_cnum - pos.pos_bol)

let inside {start; finish} loc =
  after start loc.loc_start && before finish loc.loc_end

let option f o =
  match o with
  | None -> ()
  | Some x -> f x

let found loc x =
  raise (Found (loc, x))

let let_scope s scope =
  if scope <> Location.none then
    match scope.loc_end with
    | {pos_lnum = 1; pos_bol = 0; pos_cnum = -1; _} ->
        Some (s.txt, Global_variable scope)
    | _ ->
        Some (s.txt, Local_variable scope)
  else
    None

let rebuild_env = ref false

let print_type_scheme env typ =
  Printtyp.wrap_printing_env env (fun () -> Format.asprintf "%a" Printtyp.type_scheme typ)

let print_modtype env modtyp =
  Printtyp.wrap_printing_env env (fun () -> Format.asprintf "%a" Printtyp.modtype modtyp)

let print_class_type env cltype =
  Printtyp.wrap_printing_env env (fun () -> Format.asprintf "%a" Printtyp.class_type cltype)

let rec expression_desc span loc env = function
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
        found loc (print_type_scheme env val_type, Some (full_name, annot))
      with _ ->
        ()
      end
  | Texp_let (Recursive, vbs, e) ->
      List.iter (value_binding span loc) vbs;
      expression span e
  | Texp_let (Nonrecursive, vbs, e) ->
      List.iter (value_binding span e.exp_loc) vbs;
      expression span e
  | Texp_function (_, cases, _) ->
      List.iter (case span) cases
  | Texp_apply (e, args) ->
      expression span e;
      List.iter (fun (_, e) -> option (expression span) e) args
  | Texp_match (e, cases, cases', _) ->
      expression span e;
      List.iter (case span) cases;
      List.iter (case span) cases'
  | Texp_try (e, cases) ->
      expression span e;
      List.iter (case span) cases
  | Texp_tuple el
  | Texp_construct (_, _, el) ->
      List.iter (expression span) el
  | Texp_variant (_, e) ->
      option (expression span) e
  | Texp_record (fields, e) ->
      List.iter (fun (_, _, e) -> expression span e) fields;
      option (expression span) e
  | Texp_field (e, _, _) ->
      expression span e
  | Texp_setfield (e1, _, _, e2) ->
      expression span e1;
      expression span e2
  | Texp_array el ->
      List.iter (expression span) el
  | Texp_ifthenelse (e1, e2, e3) ->
      expression span e1;
      expression span e2;
      option (expression span) e3
  | Texp_sequence (e1, e2)
  | Texp_while (e1, e2) ->
      expression span e1;
      expression span e2
  | Texp_for (_, _, e1, e2, _, e3) ->
      expression span e1;
      expression span e2;
      expression span e3
  | Texp_send (e1, _, e2) ->
      expression span e1;
      option (expression span) e2
  | Texp_letmodule (_, _, me, e) ->
      module_expr span me;
      expression span e
  | Texp_assert e
  | Texp_lazy e ->
      expression span e
  | Texp_object (ce, _) ->
      class_structure span ce
  | Texp_pack me ->
      module_expr span me
  | _ ->
      ()

and expression span {exp_loc; exp_env; exp_type; exp_desc; _} =
  if inside span exp_loc then begin
    expression_desc span exp_loc exp_env exp_desc;
    found exp_loc (print_type_scheme exp_env exp_type, None)
  end

and value_binding span scope {vb_pat; vb_expr; vb_loc; _} =
  if inside span vb_loc then begin
    pattern span scope vb_pat;
    expression span vb_expr
  end

and class_expr span {cl_desc; cl_loc; cl_env; cl_type; _} =
  if inside span cl_loc then begin
    class_expr_desc span cl_desc;
    found cl_loc (print_class_type cl_env cl_type, None)
  end

and class_expr_desc span = function
  | Tcl_ident _ ->
      ()
  | Tcl_structure cs ->
      class_structure span cs
  | Tcl_fun (_, p, args, ce, _) ->
      pattern span ce.cl_loc p;
      List.iter (fun (_, _, e) -> expression span e) args;
      class_expr span ce
  | Tcl_apply (ce, args) ->
      class_expr span ce;
      List.iter (fun (_, e) -> option (expression span) e) args
  | Tcl_let (_, vbs, args, ce) ->
      List.iter (value_binding span ce.cl_loc) vbs;
      List.iter (fun (_, _, e) -> expression span e) args;
      class_expr span ce
  | Tcl_constraint (ce, _, _, _, _) ->
      class_expr span ce

and class_field span {cf_desc; cf_loc; _} =
  if inside span cf_loc then
    class_field_desc span cf_desc

and class_field_desc span = function
  | Tcf_inherit (_, ce, _, _, _) ->
      class_expr span ce
  | Tcf_val (_, _, _, Tcfk_concrete (_, e), _)
  | Tcf_method (_, _, Tcfk_concrete (_, e))
  | Tcf_initializer e ->
      expression span e
  | _ ->
      ()

and class_structure span {cstr_self; cstr_fields; _} =
  pattern span Location.none cstr_self;
  List.iter (class_field span) cstr_fields (* Fix scope *)

and module_expr span {mod_desc; mod_loc; mod_env; mod_type; _} =
  if inside span mod_loc then begin
    module_expr_desc span mod_loc mod_desc;
    found mod_loc (print_modtype mod_env mod_type, None)
  end

and module_expr_desc span scope = function
  | Tmod_ident _ ->
      ()
  | Tmod_structure str ->
      structure span scope str
  | Tmod_functor (_, _, _, me) ->
      module_expr span me
  | Tmod_apply (me1, me2, _) ->
      module_expr span me1;
      module_expr span me2
  | Tmod_constraint (me, _, _, _) ->
      module_expr span me
  | Tmod_unpack (e, _) ->
      expression span e

and case span {c_lhs; c_guard; c_rhs} =
  let scope =
    match c_guard with
    | None -> c_rhs.exp_loc
    | Some {exp_loc = {loc_start; _}; _} -> {c_rhs.exp_loc with loc_start}
  in
  pattern span scope c_lhs;
  option (expression span) c_guard ;
  expression span c_rhs

and pattern_desc span scope loc env typ = function
  | Tpat_var (_, s) ->
      found loc (print_type_scheme env typ, let_scope s scope)
  | Tpat_alias (p, _, s) ->
      pattern span scope p;
      if inside span s.loc then
        found loc (print_type_scheme env typ, let_scope s scope)
  | Tpat_tuple pl
  | Tpat_construct (_, _, pl) ->
      List.iter (pattern span scope) pl
  | Tpat_variant (_, Some p, _) ->
      pattern span scope p
  | Tpat_record (fields, _) ->
      List.iter (fun (_, _, p) -> pattern span scope p) fields
  | Tpat_array pl ->
      List.iter (pattern span scope) pl
  | Tpat_or (p1, p2, _) ->
      pattern span scope p1;
      pattern span scope p2
  | Tpat_lazy p ->
      pattern span scope p
  | _ ->
      ()

and pattern span scope {pat_loc; pat_env; pat_type; pat_desc; _} =
  if inside span pat_loc then begin
    pattern_desc span scope pat_loc pat_env pat_type pat_desc;
    found pat_loc (print_type_scheme pat_env pat_type, None)
  end

and structure_item_desc span scope loc = function
  | Tstr_eval (e, _) ->
      expression span e
  | Tstr_value (Recursive, vbs) ->
      List.iter (value_binding span {scope with loc_start = loc.loc_start}) vbs
  | Tstr_value (Nonrecursive, vbs) ->
      List.iter (value_binding span scope) vbs
  | Tstr_module mb ->
      module_binding span mb
  | Tstr_recmodule mbs ->
      List.iter (module_binding span) mbs
  | Tstr_class args ->
      List.iter (fun ({ci_expr; _}, _) -> class_expr span ci_expr) args
  | Tstr_include {incl_mod; _} ->
      module_expr span incl_mod
  | _ ->
      ()

and structure_item span scope {str_desc; str_loc; _} =
  if inside span str_loc then
    structure_item_desc span scope str_loc str_desc

and module_binding span {mb_expr; mb_loc; _} =
  if inside span mb_loc then
    module_expr span mb_expr

and structure span scope {str_items; _} =
  let rec loop scope = function
    | si :: ({str_loc = {loc_start; _}; _} :: _ as rest) ->
        structure_item span {scope with loc_start} si;
        loop scope rest
    | si :: [] ->
        structure_item span scope si
    | [] ->
        ()
  in
  loop scope str_items

let binary_part span part =
  match part with
  | Partial_structure str ->
      structure span Location.none str
  | Partial_structure_item si ->
      structure_item span Location.none si
  | Partial_expression e ->
      expression span e
  | Partial_pattern p ->
      pattern span Location.none p
  | Partial_class_expr ce ->
      class_expr span ce
  | _ ->
      ()

let binary_annots span annot =
  match annot with
  | Implementation typedtree ->
      structure span Location.none typedtree
  | Partial_implementation binary_parts ->
      Array.iter (binary_part span) binary_parts
  | _ ->
      ()

let binary_annots span annot =
  match binary_annots span annot with
  | () -> None
  | exception (Found info) -> Some info

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
        "-v", Arg.Set debug, " Print timings";
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
    let {cmt_loadpath; cmt_annots; cmt_use_summaries; _} =
      with_timer "read_cmt" (fun () -> read_cmt filename)
    in
    Envaux.reset_cache ();
    Config.load_path := cmt_loadpath;
    rebuild_env := cmt_use_summaries;
    let info =
      with_timer "binary_annots" (fun () -> binary_annots span cmt_annots)
    in
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
