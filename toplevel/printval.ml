(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* To print values *)

open Obj
open Format
open Longident
open Path
open Typedtree


(* Given an exception value, we cannot recover its type,
   hence we cannot print its arguments in general.
   Here, we do a feeble attempt to print
   integer, string and float arguments... *)

let print_exception obj =
  print_string (Obj.magic(Obj.field(Obj.field obj 0) 0) : string);
  if Obj.size obj > 1 then begin
    open_hovbox 1;
    print_string "(";
    for i = 1 to Obj.size obj - 1 do
      if i > 1 then begin print_string ","; print_space() end;
      let arg = Obj.field obj i in
      if not (Obj.is_block arg) then
        print_int(Obj.magic arg : int)  (* Note: this could be a char! *)
      else if Obj.tag arg = 252 then begin
        print_string "\"";
        print_string (String.escaped (Obj.magic arg : string));
        print_string "\""
      end else if Obj.tag arg = 253 then
        print_float (Obj.magic arg : float)
      else
        print_string "_"
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
  Pident(Ident.new "print_int"), Predef.type_int,
    (fun x -> print_int (Obj.magic x : int));
  Pident(Ident.new "print_float"), Predef.type_float,
    (fun x -> print_float(Obj.magic x : float));
  Pident(Ident.new "print_char"), Predef.type_char,
    (fun x -> print_string "'";
              print_string (Char.escaped (Obj.magic x : char));
              print_string "'");
  Pident(Ident.new "print_string"), Predef.type_string,
    (fun x -> print_string "\"";
              print_string (String.escaped (Obj.magic x : string));
              print_string "\"")
] : (Path.t * type_expr * (Obj.t -> unit)) list)

let find_printer env ty =
  let rec find = function
    [] -> raise Not_found
  | (name, sch, printer) :: remainder ->
      if Ctype.moregeneral env sch ty
      then printer
      else find remainder
  in find !printers

(* Print a constructor or label, giving it the same prefix as the type
   it comes from. Attempt to omit the prefix if the type comes from
   a module that has been opened. *)

let print_qualified lookup_fun env ty_path name =
  match ty_path with
    Pident id ->
      print_string name
  | Pdot(p, s, pos) ->
      if try
           match lookup_fun (Lident name) env with
             Tconstr(ty_path', _) -> Path.same ty_path ty_path'
           | _ -> false
         with Not_found -> false
      then print_string name
      else (Printtyp.path p; print_string "."; print_string name)
  | Papply(p1, p2) ->
      Printtyp.path ty_path

let print_constr =
  print_qualified (fun lid env -> (Env.lookup_constructor lid env).cstr_res)
and print_label =
  print_qualified (fun lid env -> (Env.lookup_label lid env).lbl_res)

(* The main printing function *)

let max_printer_depth = ref 100
let max_printer_steps = ref 300
exception Ellipsis

let cautious f arg = try f arg with Ellipsis -> print_string "..."

let print_value env obj ty =

  let printer_steps = ref !max_printer_steps in

  let rec print_val prio depth obj ty =
    decr printer_steps;
    if !printer_steps < 0 or depth < 0 then raise Ellipsis;
    try
      find_printer env ty obj; ()
    with Not_found ->
      match Ctype.repr ty with
        Tvar _ ->
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
      | Tconstr(path, []) when Path.same path Predef.path_exn ->
          if prio > 1
          then begin open_hovbox 2; print_string "(" end
          else open_hovbox 1;
          print_exception obj;
          if prio > 1 then print_string ")";
          close_box()
      | Tconstr(path, [ty_arg]) when Path.same path Predef.path_list ->
          let rec print_conses depth cons =
            if Obj.is_block cons then begin
              print_val 0 (depth - 1) (Obj.field cons 0) ty_arg;
              let next_obj = Obj.field cons 1 in
              if Obj.is_block next_obj then begin
                print_string ";"; print_space();
                print_conses (depth - 1) next_obj
              end
            end in
          open_hovbox 1;
          print_string "[";
          cautious (print_conses depth) obj;
          print_string "]";
          close_box()
      | Tconstr(path, [ty_arg]) when Path.same path Predef.path_array ->
          let rec print_items depth i =
            if i < Obj.size obj then begin
              if i > 0 then begin print_string ";"; print_space() end;
              print_val 0 (depth - 1) (Obj.field obj i) ty_arg;
              print_items (depth - 1) (i + 1)
            end in
          open_hovbox 2;
          print_string "[|";
          cautious (print_items depth) 0;
          print_string "|]";
          close_box()
      | Tconstr(path, ty_list) ->
          begin try
            let decl = Env.find_type path env in
            match decl with
              {type_kind = Type_abstract; type_manifest = None} ->
                print_string "<abstr>"
            | {type_kind = Type_abstract; type_manifest = Some body} ->
                print_val prio depth obj
                          (Ctype.substitute decl.type_params ty_list body)
            | {type_kind = Type_variant constr_list} ->
                let tag =
                  if Obj.is_block obj
                  then Cstr_block(Obj.tag obj)
                  else Cstr_constant(Obj.magic obj) in
                let (constr_name, constr_args) =
                  find_constr tag 0 0 constr_list in
                let ty_args =
                  List.map (Ctype.substitute decl.type_params ty_list)
                      constr_args in
                begin match ty_args with
                  [] ->
                    print_constr env path constr_name
                | [ty1] ->
                    if prio > 1
                    then begin open_hovbox 2; print_string "(" end
                    else open_hovbox 1;
                    print_constr env path constr_name;
                    print_space();
                    cautious (print_val 2 (depth - 1) (Obj.field obj 0)) ty1;
                    if prio > 1 then print_string ")";
                    close_box()
                | tyl ->
                    if prio > 1
                    then begin open_hovbox 2; print_string "(" end
                    else open_hovbox 1;
                    print_constr env path constr_name;
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
                let rec print_fields depth pos = function
                  [] -> ()
                | (lbl_name, _, lbl_arg) :: remainder ->
                    if pos > 0 then begin print_string ";"; print_space() end;
                    open_hovbox 1;
                    print_label env path lbl_name;
                    print_string "="; print_cut();
                    let ty_arg =
                      Ctype.substitute decl.type_params ty_list lbl_arg in
                    cautious (print_val 0 (depth - 1) (Obj.field obj pos))
                             ty_arg;
                    close_box();
                    print_fields (depth - 1) (pos + 1) remainder in
                open_hovbox 1;
                print_string "{";
                cautious (print_fields depth 0) lbl_list;
                print_string "}";
                close_box()
          with
            Not_found ->                (* raised by Env.find_type *)
              print_string "<abstr>"
          | Constr_not_found ->         (* raised by find_constr *)
              print_string "<unknown constructor>"
          end

  and print_val_list prio depth obj ty_list =
    let rec print_list depth i = function
      [] -> ()
    | ty :: ty_list ->
        if i > 0 then begin print_string ","; print_space() end;
        print_val prio (depth - 1) (Obj.field obj i) ty;
        print_list (depth - 1) (i + 1) ty_list in
  cautious (print_list depth 0) ty_list

in print_val 0 !max_printer_depth obj ty
