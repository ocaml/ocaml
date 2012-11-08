(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Fabrice Le Fessant, INRIA Saclay                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Generate an .annot file from a .cmt file. *)

open Typedtree
open TypedtreeIter

let pattern_scopes = ref []

let push_None () =
  pattern_scopes := None :: !pattern_scopes
let push_Some annot =
  pattern_scopes := (Some annot) :: !pattern_scopes
let pop_scope () =
  match !pattern_scopes with
    [] -> assert false
  | _ :: scopes -> pattern_scopes := scopes

let rebuild_env = ref false

module ForIterator = struct
    open Asttypes

    include DefaultIteratorArgument

    let structure_begin_scopes = ref []
    let structure_end_scopes = ref []

    let rec find_last list =
      match list with
        [] -> assert false
      | [x] -> x
      | _ :: tail -> find_last tail

    let enter_structure str =
      match str.str_items with
        [] -> ()
      | _ ->
          let loc =
            match !structure_end_scopes with
              [] -> Location.none
            | _ ->
                let s = find_last str.str_items in
                s.str_loc
          in
          structure_end_scopes := loc :: !structure_end_scopes;

          let rec iter list =
            match list with
              [] -> assert false
            | [ { str_desc = Tstr_value (Nonrecursive, _); str_loc = loc } ] ->
                structure_begin_scopes := loc.Location.loc_end
                  :: !structure_begin_scopes
            | [ _ ] -> ()
            | item :: tail ->
                iter tail;
                match item, tail with
                  { str_desc = Tstr_value (Nonrecursive,_) },
                  { str_loc = loc } :: _ ->
                    structure_begin_scopes := loc.Location.loc_start
                      :: !structure_begin_scopes
                | _ -> ()
          in
          iter str.str_items

    let leave_structure str =
      match str.str_items with
        [] -> ()
      | _ ->
          match !structure_end_scopes with
            [] -> assert false
          | _ :: scopes -> structure_end_scopes := scopes

    let enter_class_expr node =
      Stypes.record (Stypes.Ti_class node)
    let enter_module_expr node =
      Stypes.record (Stypes.Ti_mod node)

    let add_variable pat id =
      match !pattern_scopes with
      | [] -> assert false
      | None :: _ -> ()
      | (Some s) :: _ ->
          Stypes.record (Stypes.An_ident (pat.pat_loc, Ident.name id, s))

    let enter_pattern pat =
      match pat.pat_desc with
      | Tpat_var (id, _)
      | Tpat_alias (_, id,_)
        -> add_variable pat id
      | Tpat_any -> ()
      | Tpat_constant _
      | Tpat_tuple _
      | Tpat_construct _
      | Tpat_lazy _
      | Tpat_or _
      | Tpat_array _
      | Tpat_record _
      | Tpat_variant _
        -> ()

    let leave_pattern pat =
      Stypes.record (Stypes.Ti_pat pat)

    let enter_expression exp =
      match exp.exp_desc with
        Texp_ident (path, _, _) ->
          let full_name = Path.name ~paren:Oprint.parenthesized_ident path in
          let env =
            if !rebuild_env then
              try
                Env.env_of_only_summary Envaux.env_from_summary exp.exp_env
              with Envaux.Error err ->
                Format.eprintf "%a@." Envaux.report_error err;
                exit 2
            else
              exp.exp_env
          in
          let annot =
            try
              let desc = Env.find_value path env in
              let dloc = desc.Types.val_loc in
              if dloc.Location.loc_ghost then Annot.Iref_external
              else Annot.Iref_internal dloc
            with Not_found ->
              Annot.Iref_external
          in
          Stypes.record
            (Stypes.An_ident (exp.exp_loc, full_name , annot))

      | Texp_let (rec_flag, _, body) ->
          begin
            match rec_flag with
            | Recursive -> push_Some (Annot.Idef exp.exp_loc)
            | Nonrecursive -> push_Some (Annot.Idef body.exp_loc)
            | Default -> push_None ()
          end
      | Texp_function _ -> push_None ()
      | Texp_match _ -> push_None ()
      | Texp_try _ -> push_None ()
      | _ -> ()

    let leave_expression exp =
      if not exp.exp_loc.Location.loc_ghost then
        Stypes.record (Stypes.Ti_expr exp);
      match exp.exp_desc with
      | Texp_let _
      | Texp_function _
      | Texp_match _
      | Texp_try _
        -> pop_scope ()
      | _ -> ()

    let enter_binding pat exp =
      let scope =
        match !pattern_scopes with
        | [] -> assert false
        | None :: _ -> Some (Annot.Idef exp.exp_loc)
        | scope :: _ -> scope
      in
      pattern_scopes := scope :: !pattern_scopes

    let leave_binding _ _ =
      pop_scope ()

    let enter_class_expr exp =
      match exp.cl_desc with
      | Tcl_fun _ -> push_None ()
      | Tcl_let _ -> push_None ()
      | _ -> ()

    let leave_class_expr exp =
      match exp.cl_desc with
      | Tcl_fun _
      | Tcl_let _ -> pop_scope ()
      | _ -> ()

    let enter_class_structure _ =
      push_None ()

    let leave_class_structure _ =
      pop_scope ()

(*
    let enter_class_field cf =
      match cf.cf_desc with
        Tcf_let _ -> push_None ()
      | _ -> ()

    let leave_class_field cf =
      match cf.cf_desc with
        Tcf_let _ -> pop_scope ()
      | _ -> ()
*)

    let enter_structure_item s =
      Stypes.record_phrase s.str_loc;
      match s.str_desc with
        Tstr_value (rec_flag, _) ->
          begin
            let loc = s.str_loc in
            let scope = match !structure_end_scopes with
                [] -> assert false
              | scope :: _ -> scope
            in
            match rec_flag with
            | Recursive -> push_Some
                  (Annot.Idef { scope with
                    Location.loc_start = loc.Location.loc_start})
            | Nonrecursive ->
(* TODO: do it lazily, when we start the next element ! *)
(*
                 let start = match srem with
                  | [] -> loc.Location.loc_end
                  | {pstr_loc = loc2} :: _ -> loc2.Location.loc_start
in  *)
                let start =
                  match !structure_begin_scopes with
                    [] -> assert false
                  | loc :: tail ->
                      structure_begin_scopes := tail;
                      loc
                in
                push_Some (Annot.Idef {scope with Location.loc_start = start})
            | Default -> push_None ()
          end
      | _ -> ()

    let leave_structure_item s =
      match s.str_desc with
        Tstr_value _ -> pop_scope ()
      | _ -> ()


  end

module Iterator = MakeIterator(ForIterator)

let gen_annot target_filename filename {Cmt_format.cmt_loadpath; cmt_annots; cmt_use_summaries; _} =
  Envaux.reset_cache ();
  Config.load_path := cmt_loadpath;
  rebuild_env := cmt_use_summaries;
  match cmt_annots with
  | Cmt_format.Implementation typedtree ->
      Iterator.iter_structure typedtree;
      let target_filename =
        match target_filename with
        | None -> Some (filename ^ ".annot")
        | Some "-" -> None
        | Some filename -> target_filename
      in
      Stypes.dump target_filename
  | Cmt_format.Interface _ ->
      Printf.fprintf stderr "Cannot generate annotations for interface file\n%!";
      exit 2
  | _ ->
      (* TODO: support Partial_implementation... *)
      Printf.fprintf stderr "File was generated with an error\n%!";
      exit 2



let gen_ml target_filename filename cmt =
  let (printer, ext) =
    match cmt.Cmt_format.cmt_annots with
      | Cmt_format.Implementation typedtree ->
        (fun ppf -> Pprintast.structure ppf (Untypeast.untype_structure typedtree)), ".ml"
      | Cmt_format.Interface typedtree ->
        (fun ppf -> Pprintast.signature ppf (Untypeast.untype_signature typedtree)), ".mli"
      | _ ->
        Printf.fprintf stderr "File was generated with an error\n%!";
        exit 2
  in
  let target_filename = match target_filename with
      None -> Some (filename ^ ext)
    | Some "-" -> None
    | Some filename -> target_filename
  in
  let oc = match target_filename with
      None -> None
    | Some filename -> Some (open_out filename) in
  let ppf = match oc with
      None -> Format.std_formatter
    | Some oc -> Format.formatter_of_out_channel oc in
  printer ppf;
  Format.pp_print_flush ppf ();
  match oc with
      None -> flush stdout
    | Some oc -> close_out oc
