(* Printing functions *)

open Format
open Misc
open Longident
open Path
open Asttypes
open Typedtree


(* Print a long identifier *)

let rec longident = function
    Lident s -> print_string s
  | Ldot(p, s) -> longident p; print_string "."; print_string s

(* Print an identifier *)

let ident id =
  print_string(Ident.name id)

(* Print a path *)

let ident_pervasive = Ident.new_persistent "Pervasives"

let rec path = function
    Pident id ->
      ident id
  | Pdot(Pident id, s, pos) when Ident.same id ident_pervasive ->
      print_string s
  | Pdot(p, s, pos) ->
      path p; print_string "."; print_string s

(* Print a type expression *)

let var_names = ref ([] : (type_expr * string) list)
let var_counter = ref 0

let reset_var_names () = var_names := []; var_counter := 0

let name_of_var v =
  try
    List.assq v !var_names
  with Not_found ->
    let name = 
      if !var_counter < 26
      then String.make 1 (Char.chr(97 + !var_counter)) 
      else String.make 1 (Char.chr(97 + !var_counter mod 26)) ^
           string_of_int(!var_counter / 26) in
    var_names := (v, name) :: !var_names;
    incr var_counter;
    name

let rec typexp sch prio = function
    Tvar {tvar_link = Some ty} ->
      typexp sch prio ty
  | Tvar {tvar_link = None; tvar_level = lvl} as v ->
      if not sch or lvl = -1 (* generic *)
      then print_string "'"
      else print_string "'_";
      print_string(name_of_var v)
  | Tarrow(ty1, ty2) ->
      if prio >= 1 then begin open_hovbox 1; print_string "(" end
                   else open_hovbox 0;
      typexp sch 1 ty1;
      print_string " ->"; print_space();
      typexp sch 0 ty2;
      if prio >= 1 then print_string ")";
      close_box()
  | Ttuple tyl ->
      if prio >= 2 then begin open_hovbox 1; print_string "(" end
                   else open_hovbox 0;
      typlist sch 2 " *" tyl;
      if prio >= 2 then print_string ")";
      close_box()
  | Tconstr(p, tyl) ->
      open_hovbox 0;
      begin match tyl with
        [] -> ()
      | [ty1] ->
          typexp sch 2 ty1; print_space()
      | tyl ->
          open_hovbox 1; print_string "("; typlist sch 0 "," tyl;
          print_string ")"; close_box(); print_space()
      end;
      path p;
      close_box()

and typlist sch prio sep = function
    [] -> ()
  | [ty] -> typexp sch prio ty
  | ty::tyl ->
      typexp sch prio ty; print_string sep; print_space();
      typlist sch prio sep tyl

let type_expr ty = typexp false 0 ty
and type_scheme ty = reset_var_names(); typexp true 0 ty

(* Print one type declaration *)

let rec type_declaration id decl =
  reset_var_names();
  open_hvbox 2;
  print_string "type ";
  type_expr (Tconstr(Pident id, decl.type_params));
  begin match decl.type_kind with
    Type_abstract -> ()
  | Type_manifest ty ->
      print_string " ="; print_space(); type_expr ty
  | Type_variant (cstr1 :: cstrs) ->
      print_string " ="; print_break(1,2);
      constructor cstr1;
      List.iter (fun cstr -> print_space(); print_string "| "; constructor cstr)
              cstrs
  | Type_record (lbl1 :: lbls) ->
      print_string " ="; print_space();
      print_string "{ "; label lbl1;
      List.iter (fun lbl -> print_string ";"; print_break(1,2); label lbl)
              lbls;
      print_string " }"
  | _ ->
      () (* A fatal error actually, except when printing type exn... *)
  end;
  close_box()

and constructor (name, args) =
  print_string name;
  match args with
    [] -> ()
  | _  -> print_string " of ";
          open_hovbox 2; typlist false 2 " *" args; close_box()

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
  open_hovbox 2;
  begin match decl.val_prim with
    Not_prim ->
      print_string "val "; ident id; print_string " :"; print_space();
      type_scheme decl.val_type
  | Primitive(p, ar) ->
      print_string "val "; ident id; print_string " :"; print_space();
      type_scheme decl.val_type; print_space();
      print_string "= \""; print_string p; print_string "\""
  end;
  close_box()

(* Print a module type *)

let rec modtype = function
    Tmty_ident p ->
      path p
  | Tmty_signature [] ->
      print_string "sig end"
  | Tmty_signature(item :: rem) ->
      open_hvbox 2;
      print_string "sig"; print_space(); 
      signature_item item;
      List.iter
        (fun item -> print_space(); signature_item item)
      rem;
      print_break(1, -2); print_string "end";
      close_box()
  | Tmty_functor(param, ty_arg, ty_res) ->
      open_hovbox 2;
      print_string "functor"; print_cut();
      print_string "("; ident param; print_string " : ";
      modtype ty_arg;
      print_string ") ->"; print_space();
      modtype ty_res;
      close_box()

and signature_item = function
    Tsig_value(id, decl) ->
      value_description id decl
  | Tsig_type(id, decl) ->
      type_declaration id decl
  | Tsig_exception(id, decl) ->
      exception_declaration id decl
  | Tsig_module(id, mty) ->
      open_hovbox 2; print_string "module "; ident id; print_string " :";
      print_space(); modtype mty; close_box()
  | Tsig_modtype(id, decl) ->
      modtype_declaration id decl

and modtype_declaration id decl =
  open_hovbox 2; print_string "module type "; ident id;
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
  List.iter (fun item -> signature_item item; print_space()) sg;
  close_box()
