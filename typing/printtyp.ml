(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Printing functions *)

open Misc
open Ctype
open Format
open Longident
open Path
open Asttypes
open Types


(* Print a long identifier *)

let rec longident = function
    Lident s -> print_string s
  | Ldot(p, s) -> longident p; print_string "."; print_string s
  | Lapply(p1, p2) ->
      longident p1; print_string "("; longident p2; print_string ")"

(* Print an identifier *)

let ident id =
  print_string(Ident.name id)

(* Print a path *)

let ident_pervasive = Ident.create_persistent "Pervasives"

let rec path = function
    Pident id ->
      ident id
  | Pdot(Pident id, s, pos) when Ident.same id ident_pervasive ->
      print_string s
  | Pdot(p, s, pos) ->
      path p; print_string "."; print_string s
  | Papply(p1, p2) ->
      path p1; print_string "("; path p2; print_string ")"

(* Print a type expression *)

let names = ref ([] : (type_expr * string) list)
let name_counter = ref 0

let reset_names () = names := []; name_counter := 0

let new_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter)) 
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
           string_of_int(!name_counter / 26)
  in
    incr name_counter;
    name

let name_of_type t =
  try List.assq t !names with Not_found ->
    let name = new_name () in
    names := (t, name) :: !names;
    name

let rec list_removeq a =
  function
    [] ->
      []
  | (b, _) as e::l ->
      if a == b then l else e::list_removeq a l

let remove_name_of_type t =
  names := list_removeq t !names

let visited_objects = ref ([] : type_expr list)
let aliased = ref ([] : type_expr list)

let rec mark_loops_rec visited ty =
  let ty = repr ty in
  if List.memq ty visited then begin
    if not (List.memq ty !aliased) then
      aliased := ty :: !aliased
  end else
    let visited = ty :: visited in
    match ty.desc with
      Tvar                -> ()
    | Tarrow(ty1, ty2)    ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
    | Ttuple tyl          -> List.iter (mark_loops_rec visited) tyl
    | Tconstr(_, tyl, _)  ->
        List.iter (mark_loops_rec visited) tyl
    | Tobject (fi, nm)    ->
        if List.memq ty !visited_objects then begin
          if not (List.memq ty !aliased) then
            aliased := ty :: !aliased
        end else begin
          if opened_object ty then
            visited_objects := ty :: !visited_objects;
          let name =
            match !nm with
              None -> None
            | Some (n, v::l) ->
                let v' = repr v in
                begin match v'.desc with
                  Tvar -> Some (n, v'::l)
                | _    -> None
                end
            | _ ->
                fatal_error "Printtyp.mark_loops_rec"
          in
          nm := name;
          begin match !nm with
            None ->
              mark_loops_rec visited fi
          | Some (_, l) ->
              List.iter (mark_loops_rec visited) l
          end
        end
    | Tfield(_, ty1, ty2) ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
    | Tnil                -> ()
    | Tlink _             -> fatal_error "Printtyp.mark_loops_rec (2)"

let mark_loops ty = mark_loops_rec [] ty

let reset_loop_marks () =
  visited_objects := []; aliased := []

let reset () =
  reset_names (); reset_loop_marks ()

let rec typexp sch prio0 ty =
  let ty = repr ty in
  try
    List.assq ty !names;
    print_string "'";
    print_string (name_of_type ty)
  with Not_found ->
    let alias = List.memq ty !aliased in
    if alias then begin
      name_of_type ty;
      if prio0 >= 1 then begin open_box 1; print_string "(" end
      else open_box 0
    end;
    let prio = if alias then 0 else prio0 in
    begin match ty.desc with
      Tvar ->
        if (not sch) or ty.level = generic_level
        then print_string "'"
        else print_string "'_";
        print_string(name_of_type ty)
    | Tarrow(ty1, ty2) ->
        if prio >= 2 then begin open_box 1; print_string "(" end
                     else open_box 0;
        typexp sch 2 ty1;
        print_string " ->"; print_space();
        typexp sch 1 ty2;
        if prio >= 2 then print_string ")";
        close_box()
    | Ttuple tyl ->
        if prio >= 3 then begin open_box 1; print_string "(" end
                     else open_box 0;
        typlist sch 3 " *" tyl;
        if prio >= 3 then print_string ")";
        close_box()
    | Tconstr(p, tyl, abbrev) ->
        open_box 0;
        begin match tyl with
          [] -> ()
        | [ty1] ->
            typexp sch 3 ty1; print_space()
        | tyl ->
            open_box 1; print_string "("; typlist sch 0 "," tyl;
            print_string ")"; close_box(); print_space()
        end;
        path p;
        close_box()
    | Tobject (fi, nm) ->
        typobject sch ty fi nm
(*
| Tfield _ -> typobject sch ty ty (ref None)
| Tnil -> typobject sch ty ty (ref None)
*)
    | _ ->
        fatal_error "Printtyp.typexp"
    end;
    if alias then begin
      print_string " as ";
      print_string "'";
      print_string (name_of_type ty);
      if not (opened_object ty) then
        remove_name_of_type ty;
      if prio0 >= 1 then print_string ")";
      close_box()
    end

and typlist sch prio sep = function
    [] -> ()
  | [ty] -> typexp sch prio ty
  | ty::tyl ->
      typexp sch prio ty; print_string sep; print_space();
      typlist sch prio sep tyl

and typobject sch ty fi nm =
  begin match !nm with
    None ->
      open_box 2;
      print_string "< ";
      (let (fields, rest) = flatten_fields fi in
         typfields sch rest fields);
      print_string " >";
      close_box ()
  | Some (p, {desc = Tvar}::tyl) ->
      open_box 0;
      begin match tyl with
        [] -> ()
      | [ty1] ->
          typexp sch 3 ty1; print_space()
      | tyl ->
          open_box 1; print_string "("; typlist sch 0 "," tyl;
          print_string ")"; close_box(); print_space()
      end;
      if sch & ty.level <> generic_level then
        print_string "_";
      print_string "#";
      path p;
      close_box()
  | _ ->
        fatal_error "Printtyp.typobject"
  end

and typfields sch rest =
  function
    [] ->
      begin match rest.desc with
        Tvar -> if sch & rest.level <> generic_level then
                  print_string "_";
                print_string ".."
      | Tnil -> ()
      | _    -> fatal_error "typfields (1)"
      end
  | [(s, t)] ->
      print_string s;
      print_string " : ";
      typexp sch 0 t;
      begin match rest.desc with
        Tvar -> print_string ";"; print_space ()
      | Tnil -> ()
      | _    -> fatal_error "typfields (2)"
      end;
      typfields sch rest []
  | (s, t)::l ->
      print_string s;
      print_string " : ";
      typexp sch 0 t;
      print_string ";"; print_space ();
      typfields sch rest l

let type_expr ty =
  typexp false 0 ty

and type_sch ty =
  typexp true 0 ty

and type_scheme ty =
  reset(); mark_loops ty; typexp true 0 ty

(* Print one type declaration *)

let constrain ty =
  let ty' = unalias ty in
  if ty != ty' then begin
    print_space ();
    open_box 2;
    print_string "constraint ";
    type_sch ty;
    print_string " =";
    print_space();
    type_sch ty';
    close_box()
  end

let rec type_declaration id decl =
  reset();

  let params = List.map repr decl.type_params in

  aliased := params @ !aliased;
  List.iter mark_loops params;
  List.iter name_of_type params;
  begin match decl.type_manifest with
    None    -> ()
  | Some ty -> mark_loops ty
  end;
  begin match decl.type_kind with
    Type_abstract -> ()
  | Type_variant [] -> ()
  | Type_variant cstrs ->
      List.iter (fun (_, args) -> List.iter mark_loops args) cstrs
  | Type_record (lbl1 :: lbls as l) ->
      List.iter (fun (_, _, ty) -> mark_loops ty) l
  end;

  open_hvbox 2;
  print_string "type ";
  type_expr {desc = Tconstr(Pident id, params, ref Mnil);
             level = generic_level};
  begin match decl.type_manifest with
    None -> ()
  | Some ty ->
      print_string " ="; print_space(); type_expr ty
  end;
  begin match decl.type_kind with
    Type_abstract -> ()
  | Type_variant [] -> ()
      (* A fatal error actually, except when printing type exn... *)
  | Type_variant cstrs ->
      print_string " =";
      List.iter
        (fun cstr -> print_space(); print_string "| "; constructor cstr)
        cstrs
  | Type_record (lbl1 :: lbls as l) ->
      print_string " ="; print_space();
      print_string "{ "; label lbl1;
      List.iter
        (fun lbl -> print_string ";"; print_break 1 2; label lbl)
        lbls;
      print_string " }"
  end;
  List.iter constrain params;
  close_box()

and constructor (name, args) =
  print_string name;
  match args with
    [] -> ()
  | _  -> print_string " of ";
          open_box 2; typlist false 3 " *" args; close_box()

and label (name, mut, arg) =
  begin match mut with
      Immutable -> ()
    | Mutable -> print_string "mutable "
  end;
  print_string name;
  print_string ": ";
  type_expr arg

(* Print an exception declaration *)

let exception_declaration id decl =
  print_string "exception "; constructor (Ident.name id, decl)

(* Print a value declaration *)

let value_description id decl =
  open_box 2;
  print_string "val "; ident id; print_string " :"; print_space();
  type_scheme decl.val_type;
  begin match decl.val_kind with
    Val_prim p ->
      print_space(); print_string "= "; Primitive.print_description p
  | _ -> ()
  end;
  close_box()

(* Print a class type *)

let class_arg arg =
  print_space ();
  open_box 1; print_string "(";
  type_sch arg;
  print_string ")"; close_box ()

let class_var l (m, t) =
  print_space ();
  open_box 2;
  print_string "val ";
  begin match m with
    Immutable -> ()
  | Mutable -> print_string "mutable "
  end;
  print_string l;
  print_string " :";
  print_space();
  type_sch t;
  close_box()

let metho kind (l, t) =
  print_space ();
  open_box 2;
  print_string kind;
  print_string l;
  print_string " :";
  print_space();
  type_sch t;
  close_box()

let methods_of_type ty =
  match (repr ty).desc with
    Tobject (m, _) -> m
  | _              -> fatal_error "Printtyp.methods_of_type"

let rec list_meths ty =
  match (repr ty).desc with
    Tfield(lab, ty, ty') -> (lab, ty) :: (list_meths ty')
  | _                    -> []

let class_type id cl_ty =
  let self = repr cl_ty.cty_self in
  let params = List.map repr cl_ty.cty_params in
  let args = cl_ty.cty_args in
  let vars = cl_ty.cty_vars in

  reset ();
  (* Self may have a name *)
  visited_objects := self :: !visited_objects;
  aliased := params @ !aliased;
  begin match self.desc with
    Tobject (fi, _) -> mark_loops fi
  | _               -> fatal_error "Printtyp.class_type"
  end;
  List.iter mark_loops params;
  List.iter mark_loops args;
  Vars.iter (fun _ (_, ty) -> mark_loops ty) vars;
  List.iter name_of_type params;
  open_hvbox 2;
  open_box 0;
  print_string "class ";
  if cl_ty.cty_new = None then
    print_string "virtual ";
  if not (opened_object self) then
    print_string "closed ";
  type_sch {desc = Tconstr(Pident id, params, ref Mnil); level = 0};
  if List.memq self !aliased then
    (name_of_type self; ());
  List.iter class_arg args;
  if List.memq self !aliased then begin
    print_string " : ";
    print_string "'";
    print_string (name_of_type self)
  end;
  print_string " =";
  close_box ();
  List.iter constrain params;
  Vars.iter class_var vars;
  let meths = list_meths (methods_of_type self) in
  let (meths, virt) =
    List.fold_right
      (fun ((lab, ty) as m) (ml, vl) ->
         if Concr.mem lab cl_ty.cty_concr then
           (m::ml, vl)
         else
           (ml, m::vl))
      meths
      ([], []) in
  List.iter (metho "method ") meths;
  List.iter (metho "virtual ") virt;
  print_break 1 (-2);
  print_string "end";
  close_box()

(* Print a module type *)

let rec modtype = function
    Tmty_ident p ->
      path p
  | Tmty_signature sg ->
      open_hvbox 2;
      print_string "sig"; signature_body true sg; 
      print_break 1 (-2); print_string "end";
      close_box()
  | Tmty_functor(param, ty_arg, ty_res) ->
      open_box 2;
      print_string "functor"; print_cut();
      print_string "("; ident param; print_string " : ";
      modtype ty_arg;
      print_string ") ->"; print_space();
      modtype ty_res;
      close_box()

and signature_body spc = function
    [] -> ()
  | item :: rem ->
      if spc then print_space();
      let cont =
        match item with
          Tsig_value(id, decl) ->
            value_description id decl; rem
        | Tsig_type(id, decl)  ->
            type_declaration id decl; rem
        | Tsig_exception(id, decl)  ->
            exception_declaration id decl; rem
        | Tsig_module(id, mty)  ->
            open_box 2; print_string "module "; ident id; print_string " :";
            print_space(); modtype mty; close_box(); rem
        | Tsig_modtype(id, decl)  ->
            modtype_declaration id decl; rem
        | Tsig_class(id, decl) ->
            class_type id decl;
            match rem with tydecl1 :: tydecl2 :: rem -> rem | _ -> []
      in signature_body true cont

and modtype_declaration id decl =
  open_box 2; print_string "module type "; ident id;
  begin match decl with
    Tmodtype_abstract -> ()
  | Tmodtype_manifest mty ->
      print_string " ="; print_space(); modtype mty
  end;
  close_box()

(* Print a signature body (used when compiling a .mli and printing results
   in interactive use). *)

let signature sg =
  open_vbox 0;
  signature_body false sg;
  close_box()

(* Print an unification error *)

let type_expansion t t' =
  if t == t' then
    type_expr t
  else begin
    open_box 2;
    type_expr t;
    print_space (); print_string "="; print_space ();
    type_expr t';
    close_box ()
  end

let rec trace fst txt =
  function
    (t1, t1')::(t2, t2')::rem ->
      if not fst then
        print_cut ();
      open_box 0;
      print_string "Type"; print_break 1 2;
      type_expansion t1 t1'; print_space ();
      txt (); print_break 1 2;
      type_expansion t2 t2';
      close_box ();
      trace false txt rem
  | _ ->
      ()

let unification_error tr txt1 txt2 =
  reset ();
  List.iter
    (function (t, t') -> mark_loops t; if t != t' then mark_loops t')
    tr;
  open_box 0;
  let (t1, t1') = List.hd tr in
  let (t2, t2') = List.hd (List.tl tr) in
  txt1 (); print_break 1 2;
  type_expansion t1 t1'; print_space();
  txt2 (); print_break 1 2;
  type_expansion t2 t2';
  close_box();
  trace false (fun _ -> print_string "is not compatible with type")
        (List.tl (List.tl tr))
