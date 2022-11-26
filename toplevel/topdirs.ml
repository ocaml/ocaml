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

(* Toplevel directives *)

open Format
open Misc
open Types
open Toploop

let error_fmt () =
  if !Sys.interactive then
    Format.std_formatter
  else
    Format.err_formatter

let action_on_suberror b =
  if not b && not !Sys.interactive then
    raise (Compenv.Exit_with_status 125)

(* Directive sections (used in #help) *)
let section_general = "General"
let section_run = "Loading code"
let section_env = "Environment queries"

let section_print = "Pretty-printing"
let section_trace = "Tracing"
let section_options = "Compiler options"

let section_undocumented = "Undocumented"

(* we will print the sections in the first list,
   then all user-defined sections,
   then the sections in the second list,
   then all undocumented directives *)
let order_of_sections =
  ([
    section_general;
    section_run;
    section_env;
  ], [
    section_print;
    section_trace;
    section_options;

    section_undocumented;
  ])
(* Do not forget to keep the directives synchronized with the manual in
   manual/src/cmds/top.etex *)

(* To quit *)

let dir_quit () = raise (Compenv.Exit_with_status 0)

let _ = add_directive "quit" (Directive_none dir_quit)
    {
      section = section_general;
      doc = "Exit the toplevel.";
    }

(* To add a directory to the load path *)

let dir_directory s =
  let d = expand_directory Config.standard_library s in
  Dll.add_path [d];
  let dir = Load_path.Dir.create d in
  Load_path.prepend_dir dir;
  toplevel_env :=
    Stdlib.String.Set.fold
      (fun name env ->
         Env.add_persistent_structure (Ident.create_persistent name) env)
      (Env.persistent_structures_of_dir dir)
      !toplevel_env

let _ = add_directive "directory" (Directive_string dir_directory)
    {
      section = section_run;
      doc = "Add the given directory to search path for source and compiled \
             files.";
    }

(* To remove a directory from the load path *)
let dir_remove_directory s =
  let d = expand_directory Config.standard_library s in
  let keep id =
    match Load_path.find_uncap (Ident.name id ^ ".cmi") with
    | exception Not_found -> true
    | fn -> Filename.dirname fn <> d
  in
  toplevel_env := Env.filter_non_loaded_persistent keep !toplevel_env;
  Load_path.remove_dir s;
  Dll.remove_path [d]

let _ = add_directive "remove_directory" (Directive_string dir_remove_directory)
    {
      section = section_run;
      doc = "Remove the given directory from the search path.";
    }

let dir_show_dirs () =
  List.iter print_endline (Load_path.get_paths ())

let _ = add_directive "show_dirs" (Directive_none dir_show_dirs)
    {
      section = section_run;
      doc = "List directories currently in the search path.";
    }

(* To change the current directory *)

let dir_cd s = Sys.chdir s

let _ = add_directive "cd" (Directive_string dir_cd)
    {
      section = section_run;
      doc = "Change the current working directory.";
    }


let with_error_fmt f x = f (error_fmt ()) x

let dir_load ppf name =
  action_on_suberror (Topeval.load_file false ppf name)

let _ = add_directive "load" (Directive_string (with_error_fmt dir_load))
    {
      section = section_run;
      doc = "Load in memory a bytecode object, produced by ocamlc.";
    }

let dir_load_rec ppf name =
  action_on_suberror (Topeval.load_file true ppf name)

let _ = add_directive "load_rec"
    (Directive_string (with_error_fmt dir_load_rec))
    {
      section = section_run;
      doc = "As #load, but loads dependencies recursively.";
    }

let load_file = Topeval.load_file false

(* Load commands from a file *)

let dir_use ppf name =
  action_on_suberror (Toploop.use_input ppf (Toploop.File name))
let dir_use_output ppf name = action_on_suberror (Toploop.use_output ppf name)
let dir_mod_use ppf name =
  action_on_suberror (Toploop.mod_use_input ppf (Toploop.File name))

let _ = add_directive "use" (Directive_string (with_error_fmt dir_use))
    {
      section = section_run;
      doc = "Read, compile and execute source phrases from the given file.";
    }

let _ = add_directive "use_output"
    (Directive_string (with_error_fmt dir_use_output))
    {
      section = section_run;
      doc = "Execute a command and read, compile and execute source phrases \
             from its output.";
    }

let _ = add_directive "mod_use" (Directive_string (with_error_fmt dir_mod_use))
    {
      section = section_run;
      doc = "Usage is identical to #use but #mod_use \
             wraps the contents in a module.";
    }

(* Install, remove a printer *)

module Printer = struct
  type kind =
    | Old of Types.type_expr
      (* 'a -> unit *)
    | Simple of Types.type_expr
      (* Format.formatter -> 'a -> unit *)
    | Generic of { ty_path: Path.t; arity: int; }
      (* (formatter -> 'a1 -> unit) ->
         (formatter -> 'a2 -> unit) ->
         ... ->
         (formatter -> 'an -> unit) ->
         formatter -> ('a1, 'a2, ..., 'an) t -> unit
      *)
end

let filter_arrow ty =
  let ty = Ctype.expand_head !toplevel_env ty in
  match get_desc ty with
  | Tarrow (lbl, l, r, _) when not (Btype.is_optional lbl) -> Some (l, r)
  | _ -> None

let extract_last_arrow ty =
  let rec extract last ty = match filter_arrow desc with
    | None -> last
    | Some ((_, rest) as next) -> extract (Some next) rest
  in extract None ty

let extract_target_type ty =
  Option.map fst (extract_last_arrow ty)

let extract_target_parameters ty =
  match extract_target_type ty with
  | None -> None
  | Some tgt ->
      let tgt = Ctype.expand_head !toplevel_env tgt in
      match get_desc tgt with
      | Tconstr (path, (_ :: _ as args), _)
        when Ctype.all_distinct_vars !toplevel_env args ->
          Some (path, args)
      | _ -> None

let match_simple_printer_type desc ~is_old_style =
  let make_printer_type =
    if is_old_style
    then Topprinters.printer_type_old
    else Topprinters.printer_type_new
  in
  Ctype.begin_def();
  let ty_arg = Ctype.newvar() in
  match
    Ctype.unify !toplevel_env
      (make_printer_type ty_arg)
      (Ctype.instance desc.val_type);
  with
  | exception Ctype.Unify _ -> None
  | () ->
      Ctype.end_def();
      Ctype.generalize ty_arg;
      if is_old_style
      then Some (Printer.Old ty_arg)
      else Some (Printer.Simple ty_arg)

let match_generic_printer_type desc ty_path args =
  let make_printer_type = Topprinters.printer_type_new in
  Ctype.begin_def();
  let args = List.map (fun _ -> Ctype.newvar ()) args in
  let ty_target = Ctype.newty (Tconstr (ty_path, args, ref Mnil)) in
  let printer_args_ty =
    List.map (fun ty_var -> make_printer_type ty_var) args in
  let ty_expected =
    List.fold_right Topprinters.type_arrow
      printer_args_ty (make_printer_type ty_target) in
  match
    Ctype.unify !toplevel_env
      ty_expected
      (Ctype.instance desc.val_type);
  with
  | exception Ctype.Unify _ -> None
  | _ ->
    Ctype.end_def();
    Ctype.generalize ty_expected;
    if Ctype.all_distinct_vars !toplevel_env args
    then Some ()
    else None

let match_printer_type desc =
  match match_simple_printer_type desc ~is_old_style:false with
  | Some _ as res -> res
  | None ->
  match match_simple_printer_type desc ~is_old_style:true with
  | Some _ as res -> res
  | None ->
  match extract_target_parameters desc.val_type with
  | None -> None
  | Some (ty_path, args) ->
    match match_generic_printer_type desc ty_path args with
    | None -> None
    | Some () ->
      Some (Printer.Generic { ty_path; arity = List.length args; })

exception Bad_printing_function

let find_printer lid =
  match Env.find_value_by_name lid !toplevel_env with
  | exception Not_found -> raise Not_found
  | (path, desc) ->
    match match_printer_type desc with
    | None -> raise Bad_printing_function
    | Some kind -> (path, kind)

let dir_install_printer ppf lid =
  match find_printer lid with
  | exception Not_found ->
      fprintf ppf "Unbound value %a.@."
        Printtyp.longident lid
  | exception Bad_printing_function ->
      fprintf ppf "%a has the wrong type for a printing function.@."
        Printtyp.longident lid
  | (path, kind) ->
    let v = eval_value_path !toplevel_env path in
    match kind with
    | Printer.Old ty_arg ->
      install_printer path ty_arg
        (fun _formatter repr -> Obj.obj v (Obj.obj repr))
    | Printer.Simple ty_arg ->
      install_printer path ty_arg
        (fun formatter repr -> Obj.obj v formatter (Obj.obj repr))
    | Printer.Generic { ty_path; arity } ->
       let rec build v = function
         | 0 ->
            Zero
              (fun formatter repr -> Obj.obj v formatter (Obj.obj repr))
         | n ->
            Succ
              (fun fn -> build ((Obj.obj v : _ -> Obj.t) fn) (n - 1)) in
       install_generic_printer' path ty_path (build v arity)

let dir_remove_printer ppf lid =
  match find_printer ppf lid with
  | exception Exit -> ()
  | (path, _kind) ->
    begin try
      remove_printer path
    with Not_found ->
      fprintf ppf "No printer named %a.@." Printtyp.longident lid
    end

let _ = add_directive "install_printer"
    (Directive_ident (with_error_fmt dir_install_printer))
    {
      section = section_print;
      doc = "Registers a printer for values of a certain type.";
    }

let _ = add_directive "remove_printer"
    (Directive_ident (with_error_fmt dir_remove_printer))
    {
      section = section_print;
      doc = "Remove the named function from the table of toplevel printers.";
    }

let parse_warnings ppf iserr s =
  try Option.iter Location.(prerr_alert none) @@ Warnings.parse_options iserr s
  with Arg.Bad err -> fprintf ppf "%s.@." err; action_on_suberror true

(* Typing information *)

let trim_signature = function
    Mty_signature sg ->
      Mty_signature
        (List.map
           (function
               Sig_module (id, pres, md, rs, priv) ->
                 let attribute =
                   Ast_helper.Attr.mk
                     (Location.mknoloc "...")
                     (Parsetree.PStr [])
                 in
                 Sig_module (id, pres, {md with md_attributes =
                                          attribute :: md.md_attributes},
                             rs, priv)
             (*| Sig_modtype (id, Modtype_manifest mty) ->
                 Sig_modtype (id, Modtype_manifest (trim_modtype mty))*)
             | item -> item)
           sg)
  | mty -> mty

let show_prim to_sig ppf lid =
  let env = !toplevel_env in
  let loc = Location.none in
  try
    let s =
      match lid with
      | Longident.Lident s -> s
      | Longident.Ldot (_,s) -> s
      | Longident.Lapply _ ->
          fprintf ppf "Invalid path %a@." Printtyp.longident lid;
          raise Exit
    in
    let id = Ident.create_persistent s in
    let sg = to_sig env loc id lid in
    Printtyp.wrap_printing_env ~error:false env
      (fun () -> fprintf ppf "@[%a@]@." Printtyp.signature sg)
  with
  | Not_found ->
      fprintf ppf "@[Unknown element.@]@."
  | Exit -> ()

let all_show_funs = ref []

let reg_show_prim name to_sig doc =
  all_show_funs := to_sig :: !all_show_funs;
  add_directive
    name
    (Directive_ident (show_prim to_sig std_formatter))
    {
      section = section_env;
      doc;
    }

let () =
  reg_show_prim "show_val"
    (fun env loc id lid ->
       let _path, desc = Env.lookup_value ~loc lid env in
       [ Sig_value (id, desc, Exported) ]
    )
    "Print the signature of the corresponding value."

let is_nonrec_type id td =
  (* We track both recursive uses of t (`type t = X of t`) and
     nonrecursive uses (`type nonrec t = t`) to only print the nonrec keyword
     when it is necessary to make the type printable.
  *)
  let recursive_use = ref false in
  let nonrecursive_use = ref false in
  let it_path = function
    | Path.Pident id' when Ident.name id' = Ident.name id ->
        if Ident.same id id' then
          recursive_use := true
        else
          nonrecursive_use:= true
    | _ -> ()
  in
  let it =  Btype.{type_iterators with it_path } in
  let () =
    it.it_type_declaration it td;
    Btype.unmark_iterators.it_type_declaration Btype.unmark_iterators td
  in
  match !recursive_use, !nonrecursive_use with
  | false, true -> Trec_not
  | true, _ | _, false -> Trec_first
    (* note: true, true is possible *)

let () =
  reg_show_prim "show_type"
    (fun env loc id lid ->
       let path, desc = Env.lookup_type ~loc lid env in
       let id, rs = match path with
         | Pident id -> id, is_nonrec_type id desc
         | _ -> id, Trec_first
       in
       [ Sig_type (id, desc, rs, Exported) ]
    )
    "Print the signature of the corresponding type constructor."

(* Each registered show_prim function is called in turn
 * and any output produced is sent to std_out.
 * Two show_prim functions are needed for constructors,
 * one for exception constructors and another for
 * non-exception constructors (normal and extensible variants). *)
let is_exception_constructor env type_expr =
  Ctype.is_equal env true [type_expr] [Predef.type_exn]

let is_extension_constructor = function
  | Cstr_extension _ -> true
  | _ -> false

let () =
  (* This show_prim function will only show constructor types
   * that are not also exception types. *)
  reg_show_prim "show_constructor"
    (fun env loc id lid ->
       let desc = Env.lookup_constructor ~loc Env.Positive lid env in
       if is_exception_constructor env desc.cstr_res then
         raise Not_found;
       let path = Btype.cstr_type_path desc in
       let type_decl = Env.find_type path env in
       if is_extension_constructor desc.cstr_tag then
         let ret_type =
           if desc.cstr_generalized then Some desc.cstr_res
           else None
         in
         let ext =
           { ext_type_path = path;
             ext_type_params = type_decl.type_params;
             ext_args = Cstr_tuple desc.cstr_args;
             ext_ret_type = ret_type;
             ext_private = Asttypes.Public;
             ext_loc = desc.cstr_loc;
             ext_attributes = desc.cstr_attributes;
             ext_uid = desc.cstr_uid; }
           in
             [Sig_typext (id, ext, Text_first, Exported)]
       else
         (* make up a fake Ident.t as type_decl : Types.type_declaration
          * does not have an Ident.t yet. Ident.create_presistent is a
          * good choice because it has no side-effects.
          * *)
         let type_id = Ident.create_persistent (Path.name path) in
         [ Sig_type (type_id, type_decl, Trec_first, Exported) ]
    )
    "Print the signature of the corresponding value constructor."

let () =
  reg_show_prim "show_exception"
    (fun env loc id lid ->
       let desc = Env.lookup_constructor ~loc Env.Positive lid env in
       if not (is_exception_constructor env desc.cstr_res) then
         raise Not_found;
       let ret_type =
         if desc.cstr_generalized then Some Predef.type_exn
         else None
       in
       let ext =
         { ext_type_path = Predef.path_exn;
           ext_type_params = [];
           ext_args = Cstr_tuple desc.cstr_args;
           ext_ret_type = ret_type;
           ext_private = Asttypes.Public;
           ext_loc = desc.cstr_loc;
           ext_attributes = desc.cstr_attributes;
           ext_uid = desc.cstr_uid;
         }
       in
         [Sig_typext (id, ext, Text_exception, Exported)]
    )
    "Print the signature of the corresponding exception."

let is_rec_module id md =
  let exception Exit in
  let rec it_path = function
    | Path.Pdot(root, _ ) -> it_path root
    | Path.Pident id' -> if (Ident.same id id') then raise Exit
    | _ -> ()
  in
  let it =  Btype.{type_iterators with it_path } in
  let rs = match it.it_module_declaration it md with
    | () -> Trec_not
    | exception Exit -> Trec_first
  in
  Btype.unmark_iterators.it_module_declaration Btype.unmark_iterators md;
  rs


let () =
  reg_show_prim "show_module"
    (fun env loc id lid ->
       let path, md = Env.lookup_module ~loc lid env in
       let id = match path with
         | Pident id -> id
         | _ -> id
       in
       let rec accum_aliases md acc =
         let acc rs =
           Sig_module (id, Mp_present,
                       {md with md_type = trim_signature md.md_type},
                       rs, Exported) :: acc in
         match md.md_type with
         | Mty_alias path ->
             let md = Env.find_module path env in
             accum_aliases md (acc Trec_not)
         | Mty_ident _ | Mty_signature _ | Mty_functor _ ->
             List.rev (acc (is_rec_module id md))
       in
       accum_aliases md []
    )
    "Print the signature of the corresponding module."

let () =
  reg_show_prim "show_module_type"
    (fun env loc id lid ->
       let _path, desc = Env.lookup_modtype ~loc lid env in
       [ Sig_modtype (id, desc, Exported) ]
    )
    "Print the signature of the corresponding module type."

let () =
  reg_show_prim "show_class"
    (fun env loc id lid ->
       let _path, desc_class = Env.lookup_class ~loc lid env in
       let _path, desc_cltype = Env.lookup_cltype ~loc lid env in
       let _path, typedcl = Env.lookup_type ~loc lid env in
       [
         Sig_class (id, desc_class, Trec_not, Exported);
         Sig_class_type (id, desc_cltype, Trec_not, Exported);
         Sig_type (id, typedcl, Trec_not, Exported);
       ]
    )
    "Print the signature of the corresponding class."

let () =
  reg_show_prim "show_class_type"
    (fun env loc id lid ->
       let _path, desc = Env.lookup_cltype ~loc lid env in
       let _path, typedcl = Env.lookup_type ~loc lid env in
       [
         Sig_class_type (id, desc, Trec_not, Exported);
         Sig_type (id, typedcl, Trec_not, Exported);
       ]
    )
    "Print the signature of the corresponding class type."

let show env loc id lid =
  let sg =
    List.fold_left
      (fun sg f -> try (f env loc id lid) @ sg with _ -> sg)
      [] !all_show_funs
  in
  if sg = [] then raise Not_found else sg

let () =
  add_directive "show" (Directive_ident (show_prim show std_formatter))
    {
      section = section_env;
      doc = "Print the signatures of components \
             from any of the categories below.";
    }

(* Control the printing of values *)

let _ = add_directive "print_depth"
    (Directive_int(fun n -> max_printer_depth := n))
    {
      section = section_print;
      doc = "Limit the printing of values to a maximal depth of n.";
    }

let _ = add_directive "print_length"
    (Directive_int(fun n -> max_printer_steps := n))
    {
      section = section_print;
      doc = "Limit the number of value nodes printed to at most n.";
    }

(* Set various compiler flags *)

let _ = add_directive "debug"
    (Directive_bool(fun b -> Clflags.debug := b))
    {
      section = section_options;
      doc = "Choose whether to generate debugging events.";
    }

let _ = add_directive "labels"
    (Directive_bool(fun b -> Clflags.classic := not b))
    {
      section = section_options;
      doc = "Choose whether to ignore labels in function types.";
    }

let _ = add_directive "principal"
    (Directive_bool(fun b -> Clflags.principal := b))
    {
      section = section_options;
      doc = "Make sure that all types are derived in a principal way.";
    }

let _ = add_directive "rectypes"
    (Directive_none(fun () -> Clflags.recursive_types := true))
    {
      section = section_options;
      doc = "Allow arbitrary recursive types during type-checking.";
    }

let _ = add_directive "ppx"
    (Directive_string(fun s -> Clflags.all_ppx := s :: !Clflags.all_ppx))
    {
      section = section_options;
      doc = "After parsing, pipe the abstract \
          syntax tree through the preprocessor command.";
    }

let _ = add_directive "warnings"
    (Directive_string (with_error_fmt(fun ppf s -> parse_warnings ppf false s)))
    {
      section = section_options;
      doc = "Enable or disable warnings according to the argument.";
    }

let _ = add_directive "warn_error"
    (Directive_string (with_error_fmt(fun ppf s -> parse_warnings ppf true s)))
    {
      section = section_options;
      doc = "Treat as errors the warnings enabled by the argument.";
    }

(* #help directive *)

let directive_sections () =
  let sections = Hashtbl.create 10 in
  let add_dir name =
    let dir =
      match get_directive name with
      | Some dir -> dir
      | None -> assert false
    in
    let section, doc =
      match get_directive_info name with
      | Some { section; doc } -> section, Some doc
      | None -> "Undocumented", None
    in
    Hashtbl.replace sections section
      ((name, dir, doc)
       :: (try Hashtbl.find sections section with Not_found -> []))
  in
  List.iter add_dir (all_directive_names ());
  let take_section section =
    if not (Hashtbl.mem sections section) then (section, [])
    else begin
      let section_dirs =
        Hashtbl.find sections section
        |> List.sort (fun (n1, _, _) (n2, _, _) -> String.compare n1 n2) in
      Hashtbl.remove sections section;
      (section, section_dirs)
    end
  in
  let before, after = order_of_sections in
  let sections_before = List.map take_section before in
  let sections_after = List.map take_section after in
  let sections_user =
    Hashtbl.fold (fun section _ acc -> section::acc) sections []
    |> List.sort String.compare
    |> List.map take_section in
  sections_before @ sections_user @ sections_after

let print_directive ppf (name, directive, doc) =
  let param = match directive with
    | Directive_none _ -> ""
    | Directive_string _ -> " <str>"
    | Directive_int _ -> " <int>"
    | Directive_bool _ -> " <bool>"
    | Directive_ident _ -> " <ident>" in
  match doc with
  | None -> fprintf ppf "#%s%s@." name param
  | Some doc ->
      fprintf ppf "@[<hov 2>#%s%s@\n%a@]@."
        name param
        Format.pp_print_text doc

let print_section ppf (section, directives) =
  if directives <> [] then begin
    fprintf ppf "%30s%s@." "" section;
    List.iter (print_directive ppf) directives;
    fprintf ppf "@.";
  end

let print_directives ppf () =
  List.iter (print_section ppf) (directive_sections ())

let _ = add_directive "help"
    (Directive_none (print_directives std_formatter))
    {
      section = section_general;
      doc = "Prints a list of all available directives, with \
          corresponding argument type if appropriate.";
    }
