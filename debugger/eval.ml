(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Debugger_config
open Misc
open Path
open Instruct
open Types
open Parser_aux

type error =
    Unbound_identifier of Ident.t
  | Not_initialized_yet of Path.t
  | Unbound_long_identifier of Longident.t
  | Unknown_name of int
  | Tuple_index of type_expr * int * int
  | Array_index of int * int
  | List_index of int * int
  | String_index of string * int * int
  | Wrong_item_type of type_expr * int
  | Wrong_label of type_expr * string
  | Not_a_record of type_expr
  | No_result

exception Error of error

let rec path event = function
    Pident id ->
      if Ident.global id then
        Debugcom.Remote_value.global (Symtable.get_global_position id)
      else begin
        try
          let pos = Ident.find_same id event.ev_compenv.ce_stack in
          Debugcom.Remote_value.local (event.ev_stacksize - pos)
        with Not_found ->
        try
          let pos = Ident.find_same id event.ev_compenv.ce_heap in
          Debugcom.Remote_value.from_environment (pos + 1)
        with Not_found ->
          raise(Error(Unbound_identifier id))
      end
  | Pdot(root, fieldname, pos) ->
      let v = path event root in
      if not (Debugcom.Remote_value.is_block v) then
        raise(Error(Not_initialized_yet root));
      Debugcom.Remote_value.field v pos
  | Papply(p1, p2) ->
      fatal_error "Eval.path: Papply"

let rec expression event env = function
    E_ident lid ->
      begin try
        let (p, valdesc) = Env.lookup_value lid env in
        (path event p, Ctype.correct_levels env valdesc.val_type)
      with Not_found ->
        raise(Error(Unbound_long_identifier lid))
      end
  | E_result ->
      begin match event.ev_kind with
        Event_after ty when !Frames.current_frame = 0 ->
          (Debugcom.Remote_value.accu(), ty)
      | _ ->
          raise(Error(No_result))
      end
  | E_name n ->
      begin try
        Printval.find_named_value n
      with Not_found ->
        raise(Error(Unknown_name n))
      end
  | E_item(arg, n) ->
      let (v, ty) = expression event env arg in
      begin match (Ctype.repr(Ctype.expand_head env ty)).desc with
        Ttuple ty_list ->
          if n < 1 || n > List.length ty_list
          then raise(Error(Tuple_index(ty, List.length ty_list, n)))
          else (Debugcom.Remote_value.field v (n-1), List.nth ty_list (n-1))
      | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_array ->
          let size = Debugcom.Remote_value.size v in
          if n >= size
          then raise(Error(Array_index(size, n)))
          else (Debugcom.Remote_value.field v n, ty_arg)
      | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_list ->
          let rec nth pos v =
            if not (Debugcom.Remote_value.is_block v) then
              raise(Error(List_index(pos, n)))
            else if pos = n then
              (Debugcom.Remote_value.field v 0, ty_arg)
            else
              nth (pos + 1) (Debugcom.Remote_value.field v 1)
          in nth 0 v
      | Tconstr(path, [], _) when Path.same path Predef.path_string ->
          let s = (Debugcom.Remote_value.obj v : string) in
          if n >= String.length s
          then raise(Error(String_index(s, String.length s, n)))
          else (Debugcom.Remote_value.of_int(Char.code s.[n]),
                Predef.type_char)
      | _ ->
          raise(Error(Wrong_item_type(ty, n)))
      end
  | E_field(arg, lbl) ->
      let (v, ty) = expression event env arg in
      begin match (Ctype.repr(Ctype.expand_head env ty)).desc with
        Tconstr(path, args, _) ->
          let tydesc = Env.find_type path env in
          begin match tydesc.type_kind with
            Type_record lbl_list ->
              let (pos, ty_res) =
                find_label lbl env ty path tydesc 0 lbl_list in
              (Debugcom.Remote_value.field v pos, ty_res)
          | _ -> raise(Error(Not_a_record ty))
          end
      | _ -> raise(Error(Not_a_record ty))
      end

and find_label lbl env ty path tydesc pos = function
    [] ->
      raise(Error(Wrong_label(ty, lbl)))
  | (name, mut, ty_arg) :: rem ->
      if name = lbl then begin
        let ty_res =
          Ctype.newgenty(Tconstr(path, tydesc.type_params, ref Mnil))
        in
        (* XXX What should be done if Ctype.apply fails ? *)
        (pos, Ctype.apply env [ty_res] ty_arg [ty])
      end else
        find_label lbl env ty path tydesc (pos + 1) rem

(* Error report *)

open Format

let report_error error =
  open_box 0;
  begin match error with
    Unbound_identifier id ->
      print_string "Unbound identifier "; print_string (Ident.name id)
  | Not_initialized_yet path ->
      print_string "The module path "; Printtyp.path path;
      print_string " is not yet initialized."; print_space();
      print_string "Please run program forward until its initialization code is executed."
  | Unbound_long_identifier lid ->
      print_string "Unbound identifier "; Printtyp.longident lid
  | Unknown_name n ->
      print_string "Unknown value name $"; print_int n
  | Tuple_index(ty, len, pos) ->
      print_string "Cannot extract field number "; print_int pos;
      print_string " from a "; print_int len;
      print_string "-components tuple of type ";
      Printtyp.reset (); Printtyp.mark_loops ty;
      print_space(); Printtyp.type_expr ty
  | Array_index(len, pos) ->
      print_string "Cannot extract element number "; print_int pos;
      print_string " from array of length "; print_int len
  | List_index(len, pos) ->
      print_string "Cannot extract element number "; print_int pos;
      print_string " from list of length "; print_int len
  | String_index(s, len, pos) ->
      print_string "Cannot extract character number "; print_int pos;
      print_string " from the following string of length "; print_int len;
      print_string ":"; print_space();
      print_char '"'; print_string(String.escaped s); print_char '"'
  | Wrong_item_type(ty, pos) ->
      print_string "Cannot extract item number "; print_int pos;
      print_string " from a value of type"; print_space();
      Printtyp.type_expr ty
  | Wrong_label(ty, lbl) ->
      print_string "The record type"; print_space(); Printtyp.type_expr ty;
      print_space(); print_string " has no label named "; print_string lbl
  | Not_a_record ty ->
      print_string "The type"; print_space(); Printtyp.type_expr ty;
      print_space(); print_string " is not a record type"
  | No_result ->
      print_string "No result available at current program event"
  end;
  close_box(); print_newline()
