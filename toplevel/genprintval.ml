(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* To print values *)

open Misc
open Format
open Longident
open Path
open Types

module type OBJ =
  sig
    type t
    val obj : t -> 'a
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
  end

module type EVALPATH =
  sig
    type value
    val eval_path: Path.t -> value
    exception Error
  end

module type S =
  sig
    type t
    val install_printer : Path.t -> Types.type_expr -> (t -> unit) -> unit
    val remove_printer : Path.t -> unit
    val print_untyped_exception : formatter -> t -> unit
    val print_value :
          int -> int -> (int -> t -> Types.type_expr -> bool) ->
          Env.t -> t -> formatter -> type_expr -> unit
  end

module Make(O : OBJ)(EVP : EVALPATH with type value = O.t) = struct

    type t = O.t

    (* Given an exception value, we cannot recover its type,
       hence we cannot print its arguments in general.
       Here, we do a feeble attempt to print
       integer, string and float arguments... *)

    let print_untyped_exception_args obj ppf start_offset =
      if O.size obj > start_offset then begin
        fprintf ppf "@[<1>(";
        for i = start_offset to O.size obj - 1 do
          if i > start_offset then fprintf ppf ",@ ";
          let arg = O.field obj i in
          if not (O.is_block arg) then
            fprintf ppf "%i" (O.obj arg : int)
               (* Note: this could be a char or a constant constructor... *)
          else if O.tag arg = Obj.string_tag then
            fprintf ppf "\"%s\"" (String.escaped (O.obj arg : string))
          else if O.tag arg = Obj.double_tag then
            fprintf ppf "%f" (O.obj arg : float)
          else
            fprintf ppf "_"
        done;
        fprintf ppf ")@]"
      end

    let print_untyped_exception ppf bucket =
      let name = (O.obj(O.field(O.field bucket 0) 0) : string) in
      if (name = "Match_failure" || name = "Assert_failure")
      && O.size bucket = 2
      && O.tag(O.field bucket 1) = 0
      then fprintf ppf "%s%a" name
                       (print_untyped_exception_args (O.field bucket 1)) 0
      else fprintf ppf "%s%a" name
                       (print_untyped_exception_args bucket) 1

    (* The user-defined printers. Also used for some builtin types. *)

    let printers = ref ([
      Pident(Ident.create "print_int"), Predef.type_int,
        (fun ppf x -> fprintf ppf "%i" (O.obj x : int));
      Pident(Ident.create "print_float"), Predef.type_float,
        (fun ppf x -> fprintf ppf "%f" (O.obj x : float));
      Pident(Ident.create "print_char"), Predef.type_char,
        (fun ppf x ->
          fprintf ppf "'%s'" (Char.escaped (O.obj x : char)));
      Pident(Ident.create "print_string"), Predef.type_string,
        (fun ppf x ->
          fprintf ppf "\"%s\"" (String.escaped (O.obj x : string)));
      Pident(Ident.create "print_int32"), Predef.type_int32,
        (fun ppf x ->
          fprintf ppf "<int32 %s>" (Int32.to_string (O.obj x : int32)));
      Pident(Ident.create "print_nativeint"), Predef.type_nativeint,
        (fun ppf x ->
          fprintf ppf "<nativeint %s>"
            (Nativeint.to_string (O.obj x : nativeint)));
      Pident(Ident.create "print_int64"), Predef.type_int64,
        (fun ppf x -> 
          fprintf ppf "<int64 %s>" (Int64.to_string (O.obj x : int64)));
    ] : (Path.t * type_expr * (Format.formatter -> O.t -> unit)) list)

    let install_printer path ty fn =
      let print_val ppf obj =
        try fn obj with
        | exn ->
           fprintf ppf "<printer %a raised an exception>" Printtyp.path path in
      printers := (path, ty, print_val) :: !printers

    let remove_printer path =
      let rec remove = function
      | [] -> raise Not_found
      | (p, ty, fn as printer) :: rem ->
          if Path.same p path then rem else printer :: remove rem in
      printers := remove !printers

    let find_printer env ty =
      let rec find = function
      | [] -> raise Not_found
      | (name, sch, printer) :: remainder ->
          if Ctype.moregeneral env false sch ty
          then printer
          else find remainder
      in find !printers

    (* Print a constructor or label, giving it the same prefix as the type
       it comes from. Attempt to omit the prefix if the type comes from
       a module that has been opened. *)

    let print_qualified lookup_fun env ty_path ppf name =
      match ty_path with
      | Pident id ->
          fprintf ppf "%s" name
      | Pdot(p, s, pos) ->
          if try
               match (lookup_fun (Lident name) env).desc with
               | Tconstr(ty_path', _, _) -> Path.same ty_path ty_path'
               | _ -> false
             with Not_found -> false
          then fprintf ppf "%s" name
          else fprintf ppf "%a.%s" Printtyp.path p name
      | Papply(p1, p2) ->
          Printtyp.path ppf ty_path

    let print_constr =
      print_qualified
        (fun lid env -> (Env.lookup_constructor lid env).cstr_res)

    and print_label =
      print_qualified (fun lid env -> (Env.lookup_label lid env).lbl_res)

    (* An abstract type *)

    let abstract_type =
      Ctype.newty (Tconstr (Pident (Ident.create "abstract"), [], ref Mnil))

    (* The main printing function *)

    exception Ellipsis

    let cautious f ppf arg = try f arg with Ellipsis -> fprintf ppf "..."

    let print_value max_steps max_depth check_depth env obj ppf ty =

      let printer_steps = ref max_steps in

      let rec print_val prio depth obj ppf ty =
        decr printer_steps;
        if !printer_steps < 0 or depth < 0 then raise Ellipsis;
        try
          find_printer env ty ppf obj
        with Not_found ->
          match (Ctype.repr ty).desc with
          | Tvar ->
              fprintf ppf "<poly>"
          | Tarrow(_, ty1, ty2) ->
              fprintf ppf "<fun>"
          | Ttuple(ty_list) ->
              if check_depth depth obj ty then begin
                if prio > 0
                then
                  fprintf ppf "@[<1>(%a)@]" 
                          (print_val_list 1 0 depth obj) ty_list
                else fprintf ppf "@[%a@]"
                          (print_val_list 1 0 depth obj) ty_list
              end
          | Tconstr(path, [], _) when Path.same path Predef.path_exn ->
              print_exception prio depth ppf obj
          | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_list ->
              if O.is_block obj then begin
                if check_depth depth obj ty then begin
                  let rec print_conses ppf cons =
                    print_val 0 (depth - 1) (O.field cons 0) ppf ty_arg;
                    let next_obj = O.field cons 1 in
                    if O.is_block next_obj then
                      fprintf ppf ";@ %a" print_conses next_obj
                  in
                  fprintf ppf "@[<1>[%a]@]" (cautious (print_conses ppf)) obj
                end
              end else
                fprintf ppf "[]"
          | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_array ->
              let length = O.size obj in
              if length = 0 then
                fprintf ppf "[||]"
              else if check_depth depth obj ty then begin
                let rec print_items ppf i =
                  if i < length then begin
                    if i > 0 then fprintf ppf ";@ ";
                    print_val 0 (depth - 1) (O.field obj i) ppf ty_arg;
                    print_items ppf (i + 1)
                  end in
                fprintf ppf "@[<2>[|%a|]@]" (cautious (print_items ppf)) 0;
              end
          | Tconstr(path, ty_list, _) ->
              begin try
                let decl = Env.find_type path env in
                match decl with
                | {type_kind = Type_abstract; type_manifest = None} ->
                    fprintf ppf "<abstr>"
                | {type_kind = Type_abstract; type_manifest = Some body} ->
                    print_val prio depth obj ppf
                      (try Ctype.apply env decl.type_params body ty_list with
                         Ctype.Cannot_apply -> abstract_type)
                | {type_kind = Type_variant constr_list} ->
                    let tag =
                      if O.is_block obj
                      then Cstr_block(O.tag obj)
                      else Cstr_constant(O.obj obj) in
                    let (constr_name, constr_args) =
                      Datarepr.find_constr_by_tag tag constr_list in
                    let ty_args =
                      List.map
                        (function ty ->
                           try Ctype.apply env decl.type_params ty ty_list with
                             Ctype.Cannot_apply -> abstract_type)
                        constr_args in
                    print_constr_with_args (print_constr env path) constr_name
                                           prio 0 depth obj ppf ty_args
                | {type_kind = Type_record(lbl_list, rep)} ->
                    if check_depth depth obj ty then begin
                      let rec print_fields pos ppf = function
                      | [] -> ()
                      | (lbl_name, _, lbl_arg) :: remainder ->
                          let ty_arg =
                            try
                              Ctype.apply env decl.type_params lbl_arg ty_list
                            with
                              Ctype.Cannot_apply -> abstract_type in
                          if pos > 0 then fprintf ppf ";@ ";
                          fprintf ppf "@[<1>%a=@,%a@]"
                          (print_label env path) lbl_name
                          (cautious (print_val 0 (depth - 1)
                                     (O.field obj pos) ppf))
                          ty_arg;
                          (print_fields (pos + 1)) ppf remainder in

                      fprintf ppf "@[<1>{%a}@]"
                      (cautious (print_fields 0 ppf)) lbl_list;
                    end
              with
                Not_found ->                (* raised by Env.find_type *)
                  fprintf ppf "<abstr>"
              | Datarepr.Constr_not_found -> (* raised by find_constr_by_tag *)
                  fprintf ppf "<unknown constructor>"
              end
          | Tvariant row ->
              let row = Btype.row_repr row in
              if O.is_block obj then begin
                let tag : int = O.obj (O.field obj 0) in
                (if prio > 1
                 then fprintf ppf "@[<2>(`%a)@]"
                 else fprintf ppf "`%a")
                (fun ppf ->
                  List.iter
                   (fun (l, f) -> if Btype.hash_variant l = tag then
                     match Btype.row_field_repr f with
                     | Rpresent(Some ty) ->
                        fprintf ppf "%s@ %a" l
                        (cautious (print_val 2 (depth - 1) (O.field obj 1) ppf))
                        ty
                     | _ -> ()))
                  row.row_fields;
              end else begin
                let tag : int = O.obj obj in
                let pr_rows ppf =
                  List.iter
                  (fun (l, _) ->
                    if Btype.hash_variant l = tag then fprintf ppf "%s" l) in
                fprintf ppf "`%a" pr_rows row.row_fields
              end
          | Tobject (_, _) ->
              fprintf ppf "<obj>"
          | Tsubst ty ->
              print_val prio (depth - 1) obj ppf ty
          | Tfield(_, _, _, _) | Tnil | Tlink _ ->
              fatal_error "Printval.print_value"

      and print_val_list prio start depth obj ppf ty_list =
        let rec print_list i = function
          |  [] -> ()
          | ty :: ty_list ->
              if i > start then fprintf ppf ",@ ";
              print_val prio (depth - 1) (O.field obj i) ppf ty;
              print_list (i + 1) ty_list in
      cautious (print_list start) ppf ty_list

      and print_constr_with_args
             print_cstr cstr_name prio start depth obj ppf ty_args =
        match ty_args with
          [] ->
            print_cstr ppf cstr_name
        | [ty1] ->
            if check_depth depth obj ty then
              (if prio > 1
              then fprintf ppf "@[<2>(%a@ %a)@]"
              else fprintf ppf "@[<1>%a@ %a@]")
              print_cstr cstr_name
              (cautious
                 (print_val 2 (depth - 1) (O.field obj start) ppf))
              ty1;
        | tyl ->
            if check_depth depth obj ty then
              (if prio > 1
              then fprintf ppf "@[<2>(%a@ @[<1>(%a)@])@]"
              else fprintf ppf "@[<1>%a@ @[<1>(%a)@]@]")
              print_cstr cstr_name
              (print_val_list 1 start depth obj) tyl;

    and print_exception prio depth ppf bucket =
      let name = (O.obj(O.field(O.field bucket 0) 0) : string) in
      let lid = Longident.parse name in
      try
        (* Attempt to recover the constructor description for the exn
           from its name *)
        let cstr = Env.lookup_constructor lid env in
        let path =
          match cstr.cstr_tag with
            Cstr_exception p -> p | _ -> raise Not_found in
        (* Make sure this is the right exception and not an homonym,
           by evaluating the exception found and comparing with the identifier
           contained in the exception bucket *)
        if O.field bucket 0 != EVP.eval_path path then raise Not_found;
        print_constr_with_args
           pp_print_string name prio 1 depth bucket ppf cstr.cstr_args
      with Not_found | EVP.Error ->
        if check_depth depth obj ty then begin
          if prio > 1
          then fprintf ppf "@[<2>(%a)@]" print_untyped_exception obj
          else fprintf ppf "@[<1>%a@]" print_untyped_exception obj
        end

    in cautious (print_val 0 max_depth obj ppf) ppf ty

end
