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

(* To print values *)

open Misc
open Obj
open Format
open Longident
open Path
open Types

(* To name printed and ellipsed values *)

let named_values =
  (Hashtbl.create 29 : (int, Debugcom.remote_value * type_expr) Hashtbl.t)
let next_name = ref 1

let reset_named_values () =
  Hashtbl.clear named_values;
  next_name := 1

let name_value v ty =
  let name = !next_name in
  incr next_name;
  Hashtbl.add named_values name (v, ty);
  name

let find_named_value name =
  Hashtbl.find named_values name

(* Given an exception value, we cannot recover its type,
   hence we cannot print its arguments in general.
   Here, we do a feeble attempt to print
   integer, string and float arguments... *)

let print_exception obj =
  print_string
    (Debugcom.marshal_obj(Debugcom.get_field (Debugcom.get_field obj 0) 0));
  let (tag, field) = Debugcom.get_obj obj in
  if Array.length field > 1 then begin
    open_hovbox 1;
    print_string "(";
    for i = 1 to Array.length field - 1 do
      if i > 1 then begin print_string ","; print_space() end;
      let arg = field.(i) in
      if Debugcom.remote_value_is_int arg then
        print_int(Debugcom.int_value arg) (* Note: this could be a char! *)
      else begin
        let (tag, sz) = Debugcom.get_header arg in
        if tag = 252 then begin
          print_string "\"";
          print_string (String.escaped (Debugcom.marshal_obj arg : string));
          print_string "\""
        end else if tag = 253 then
          print_float (Debugcom.marshal_obj arg : float)
        else
          print_string "_"
      end
    done;
    print_string ")";
    close_box()
  end

(* Recover a constructor by its tag *)

exception Constr_not_found

let rec find_constr tag num_const num_nonconst = function
    [] ->
      raise Constr_not_found
  | (name, [] as cstr) :: rem ->
      if tag = Cstr_constant num_const
      then cstr
      else find_constr tag (num_const + 1) num_nonconst rem
  | (name, _ as cstr) :: rem ->
      if tag = Cstr_block num_nonconst
      then cstr
      else find_constr tag num_const (num_nonconst + 1) rem

(* The user-defined printers. Also used for some builtin types. *)

let printers = ref ([
  Pident(Ident.create "print_int"), Predef.type_int,
    (fun x -> print_int (Debugcom.int_value x));
  Pident(Ident.create "print_float"), Predef.type_float,
    (fun x -> print_float(Debugcom.marshal_obj x : float));
  Pident(Ident.create "print_char"), Predef.type_char,
    (fun x -> print_string "'";
              print_string (Char.escaped(Char.chr(Debugcom.int_value x)));
              print_string "'");
  Pident(Ident.create "print_string"), Predef.type_string,
    (fun x -> print_string "\"";
              print_string (String.escaped(Debugcom.marshal_obj x : string));
              print_string "\"")
] : (Path.t * type_expr * (Debugcom.remote_value -> unit)) list)

let find_printer env ty =
  let rec find = function
    [] -> raise Not_found
  | (name, sch, printer) :: remainder ->
      if Ctype.moregeneral env sch ty
      then printer
      else find remainder
  in find !printers

(* The main printing function *)

let max_printer_depth = ref 20
let max_printer_steps = ref 300
exception Ellipsis

let cautious f arg =
  try f arg with Ellipsis -> print_string "..."

let print_value max_depth obj ty env =

  let printer_steps = ref !max_printer_steps in

  let rec print_val prio depth obj ty =
    decr printer_steps;
    if !printer_steps < 0 then raise Ellipsis;
    if depth < 0 then begin
      let n = name_value obj ty in
      print_char '$'; print_int n
    end else begin
      try
        find_printer env ty obj; ()
      with Not_found ->
        match (Ctype.repr ty).desc with
          Tvar ->
            print_string "<poly>"
        | Tarrow(ty1, ty2) ->
            print_string "<fun>"
        | Ttuple(ty_list) ->
            if prio > 0
            then begin open_hovbox 1; print_string "(" end
            else open_hovbox 0;
            print_val_list 1 depth obj ty_list;
            if prio > 0 then print_string ")";
            close_box()
        | Tconstr(path, [], _) when Path.same path Predef.path_exn ->
            if prio > 1
            then begin open_hovbox 2; print_string "(" end
            else open_hovbox 1;
            print_exception obj;
            if prio > 1 then print_string ")";
            close_box()
        | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_list ->
            let rec print_conses cons =
              if not (Debugcom.remote_value_is_int cons) then begin
                print_val 0 (depth - 1) (Debugcom.get_field cons 0) ty_arg;
                let next_obj = Debugcom.get_field cons 1 in
                if not (Debugcom.remote_value_is_int next_obj) then begin
                  print_string ";"; print_space();
                  print_conses next_obj
                end
              end in
            open_hovbox 1;
            print_string "[";
            cautious print_conses obj;
            print_string "]";
            close_box()
        | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_array ->
            let (tag, fields) = Debugcom.get_obj obj in
            let rec print_items i =
              if i < Array.length fields then begin
                if i > 0 then begin print_string ";"; print_space() end;
                print_val 0 (depth - 1) fields.(i) ty_arg;
                print_items (i + 1)
              end in
            open_hovbox 2;
            print_string "[|";
            cautious print_items 0;
            print_string "|]";
            close_box()
        | Tconstr(path, ty_list, _) ->
            begin try
              let decl = Env.find_type path env in
              match decl with
                {type_kind = Type_abstract; type_manifest = None} ->
                  print_string "<abstr>"
              | {type_kind = Type_abstract; type_manifest = Some body} ->
                  print_val prio depth obj
                            (Ctype.substitute [] decl.type_params ty_list body)
              | {type_kind = Type_variant constr_list} ->
                  let tag =
                    if Debugcom.remote_value_is_int obj then
                      Cstr_constant(Debugcom.int_value obj)
                    else
                      let (tag, sz) = Debugcom.get_header obj in
                      Cstr_block tag in
                  let (constr_name, constr_args) =
                    find_constr tag 0 0 constr_list in
                  let ty_args =
                    List.map (Ctype.substitute [] decl.type_params ty_list)
                        constr_args in
                  begin match ty_args with
                    [] ->
                      print_string constr_name
                  | [ty1] ->
                      if prio > 1
                      then begin open_hovbox 2; print_string "(" end
                      else open_hovbox 1;
                      print_string constr_name;
                      print_space();
                      cautious
                        (print_val 2 (depth - 1) (Debugcom.get_field obj 0))
                        ty1;
                      if prio > 1 then print_string ")";
                      close_box()
                  | tyl ->
                      if prio > 1
                      then begin open_hovbox 2; print_string "(" end
                      else open_hovbox 1;
                      print_string constr_name;
                      print_space();
                      open_hovbox 1;
                      print_string "(";
                      print_val_list 1 depth obj tyl;
                      print_string ")";
                      close_box();
                      if prio > 1 then print_string ")";
                      close_box()
                  end
              | {type_kind = Type_record lbl_list} ->
                  let rec print_fields pos = function
                    [] -> ()
                  | (lbl_name, _, lbl_arg) :: remainder ->
                      if pos > 0 then begin
                        print_string ";"; print_space()
                      end;
                      open_hovbox 1;
                      print_string lbl_name;
                      print_string "="; print_cut();
                      let ty_arg =
                        Ctype.substitute [] decl.type_params ty_list lbl_arg in
                      cautious
                        (print_val 0 (depth - 1) (Debugcom.get_field obj pos))
                        ty_arg;
                      close_box();
                      print_fields (pos + 1) remainder in
                  open_hovbox 1;
                  print_string "{";
                  cautious (print_fields 0) lbl_list;
                  print_string "}";
                  close_box()
            with
              Not_found ->                (* raised by Env.find_type *)
                print_string "<abstr>"
            | Constr_not_found ->         (* raised by find_constr *)
                print_string "<unknown constructor>"
            end
        | Tobject (_, _) ->
            print_string "<obj>"
        | Tfield(_, _, _) | Tnil | Tlink _ ->
            fatal_error "Printval.print_value"
      end

  and print_val_list prio depth obj ty_list =
    let rec print_list i = function
      [] -> ()
    | ty :: ty_list ->
        if i > 0 then begin print_string ","; print_space() end;
        print_val prio (depth - 1) (Debugcom.get_field obj i) ty;
        print_list (i + 1) ty_list in
    cautious (print_list 0) ty_list

in print_val 0 max_depth obj ty

let print_named_value max_depth obj ty env =
  open_hovbox 2;
  let n = name_value obj ty in
  print_char '$'; print_int n;
  print_string " :"; print_space(); Printtyp.type_expr ty;
  print_space(); print_string "="; print_space();
  print_value max_depth obj ty env;
  close_box();
  print_newline()

let print_ident_value max_depth lid obj ty env =
  open_hovbox 2;
  Printtyp.longident lid;
  print_string " :"; print_space(); Printtyp.type_expr ty;
  print_space(); print_string "="; print_space();
  print_value max_depth obj ty env;
  close_box();
  print_newline()

