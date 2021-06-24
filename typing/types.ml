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

(* Representation of types and declarations *)

open Asttypes

(* Type expressions for the core language *)

type transient_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable scope: int;
    id: int }

and type_expr = transient_expr

and type_desc =
    Tvar of string option
  | Tarrow of arg_label * type_expr * type_expr * commutable
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr * type_expr option
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * (Longident.t * type_expr) list

and row_desc =
    { row_fields: (label * row_field) list;
      row_more: type_expr;
      row_bound: unit;
      row_closed: bool;
      row_fixed: fixed_explanation option;
      row_name: (Path.t * type_expr list) option }
and fixed_explanation =
  | Univar of type_expr | Fixed_private | Reified of Path.t | Rigid
and row_field =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

and abbrev_memo =
    Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and field_kind =
    Fvar of field_kind option ref
  | Fpresent
  | Fabsent

and commutable =
    Cok
  | Cunknown
  | Clink of commutable ref

module TransientTypeOps = struct
  type t = type_expr
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

(* *)

module Uid = struct
  type t =
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int }
    | Internal
    | Predef of string

  include Identifiable.Make(struct
    type nonrec t = t

    let equal (x : t) y = x = y
    let compare (x : t) y = compare x y
    let hash (x : t) = Hashtbl.hash x

    let print fmt = function
      | Internal -> Format.pp_print_string fmt "<internal>"
      | Predef name -> Format.fprintf fmt "<predef:%s>" name
      | Compilation_unit s -> Format.pp_print_string fmt s
      | Item { comp_unit; id } -> Format.fprintf fmt "%s.%d" comp_unit id

    let output oc t =
      let fmt = Format.formatter_of_out_channel oc in
      print fmt t
  end)

  let id = ref (-1)

  let reinit () = id := (-1)

  let mk  ~current_unit =
      incr id;
      Item { comp_unit = current_unit; id = !id }

  let of_compilation_unit_id id =
    if not (Ident.persistent id) then
      Misc.fatal_errorf "Types.Uid.of_compilation_unit_id %S" (Ident.name id);
    Compilation_unit (Ident.name id)

  let of_predef_id id =
    if not (Ident.is_predef id) then
      Misc.fatal_errorf "Types.Uid.of_predef_id %S" (Ident.name id);
    Predef (Ident.name id)

  let internal_not_actually_unique = Internal

  let for_actual_declaration = function
    | Item _ -> true
    | _ -> false
end

(* Maps of methods and instance variables *)

module Meths = Misc.Stdlib.String.Map
module Vars = Meths

(* Value descriptions *)

type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind;
    val_loc: Location.t;
    val_attributes: Parsetree.attributes;
    val_uid: Uid.t;
  }

and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of (Ident.t * type_expr) Meths.t ref *
                (Ident.t * Asttypes.mutable_flag *
                 Asttypes.virtual_flag * type_expr) Vars.t ref *
                string * type_expr
                                        (* Self *)
  | Val_anc of (string * Ident.t) list * string
                                        (* Ancestor *)

(* Variance *)

module Variance = struct
  type t = int
  type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv
  let single = function
    | May_pos -> 1
    | May_neg -> 2
    | May_weak -> 4
    | Inj -> 8
    | Pos -> 16
    | Neg -> 32
    | Inv -> 64
  let union v1 v2 = v1 lor v2
  let inter v1 v2 = v1 land v2
  let subset v1 v2 = (v1 land v2 = v1)
  let eq (v1 : t) v2 = (v1 = v2)
  let set x b v =
    if b then v lor single x else  v land (lnot (single x))
  let mem x = subset (single x)
  let null = 0
  let unknown = 7
  let full = 127
  let covariant = single May_pos lor single Pos lor single Inj
  let swap f1 f2 v =
    let v' = set f1 (mem f2 v) v in set f2 (mem f1 v) v'
  let conjugate v = swap May_pos May_neg (swap Pos Neg v)
  let get_upper v = (mem May_pos v, mem May_neg v)
  let get_lower v = (mem Pos v, mem Neg v, mem Inv v, mem Inj v)
  let unknown_signature ~injective ~arity =
    let v = if injective then set Inj true unknown else unknown in
    Misc.replicate_list v arity
end

module Separability = struct
  type t = Ind | Sep | Deepsep
  type signature = t list
  let eq (m1 : t) m2 = (m1 = m2)
  let rank = function
    | Ind -> 0
    | Sep -> 1
    | Deepsep -> 2
  let compare m1 m2 = compare (rank m1) (rank m2)
  let max m1 m2 = if rank m1 >= rank m2 then m1 else m2

  let print ppf = function
    | Ind -> Format.fprintf ppf "Ind"
    | Sep -> Format.fprintf ppf "Sep"
    | Deepsep -> Format.fprintf ppf "Deepsep"

  let print_signature ppf modes =
    let pp_sep ppf () = Format.fprintf ppf ",@," in
    Format.fprintf ppf "@[(%a)@]"
      (Format.pp_print_list ~pp_sep print) modes

  let default_signature ~arity =
    let default_mode = if Config.flat_float_array then Deepsep else Ind in
    Misc.replicate_list default_mode arity
end

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_decl_kind;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: Variance.t list;
    type_separability: Separability.t list;
    type_is_newtype: bool;
    type_expansion_scope: int;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_immediate: Type_immediacy.t;
    type_unboxed_default: bool;
    type_uid: Uid.t;
 }

and type_decl_kind = (label_declaration, constructor_declaration) type_kind

and ('lbl, 'cstr) type_kind =
    Type_abstract
  | Type_record of 'lbl list * record_representation
  | Type_variant of 'cstr list * variant_representation
  | Type_open

and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)
  | Record_unboxed of bool    (* Unboxed single-field record, inlined or not *)
  | Record_inlined of int               (* Inlined record *)
  | Record_extension of Path.t          (* Inlined record under extension *)

and variant_representation =
    Variant_regular          (* Constant or boxed constructors *)
  | Variant_unboxed          (* One unboxed single-field constructor *)

and label_declaration =
  {
    ld_id: Ident.t;
    ld_mutable: mutable_flag;
    ld_type: type_expr;
    ld_loc: Location.t;
    ld_attributes: Parsetree.attributes;
    ld_uid: Uid.t;
  }

and constructor_declaration =
  {
    cd_id: Ident.t;
    cd_args: constructor_arguments;
    cd_res: type_expr option;
    cd_loc: Location.t;
    cd_attributes: Parsetree.attributes;
    cd_uid: Uid.t;
  }

and constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

type extension_constructor =
  { ext_type_path: Path.t;
    ext_type_params: type_expr list;
    ext_args: constructor_arguments;
    ext_ret_type: type_expr option;
    ext_private: private_flag;
    ext_loc: Location.t;
    ext_attributes: Parsetree.attributes;
    ext_uid: Uid.t;
  }

and type_transparence =
    Type_public      (* unrestricted expansion *)
  | Type_new         (* "new" type *)
  | Type_private     (* private type *)

(* Type expressions for the class language *)

module Concr = Misc.Stdlib.String.Set

type class_type =
    Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type

and class_signature =
  { csig_self: type_expr;
    csig_vars:
      (Asttypes.mutable_flag * Asttypes.virtual_flag * type_expr) Vars.t;
    csig_concr: Concr.t;
    csig_inher: (Path.t * type_expr list) list }

type class_declaration =
  { cty_params: type_expr list;
    mutable cty_type: class_type;
    cty_path: Path.t;
    cty_new: type_expr option;
    cty_variance: Variance.t list;
    cty_loc: Location.t;
    cty_attributes: Parsetree.attributes;
    cty_uid: Uid.t;
 }

type class_type_declaration =
  { clty_params: type_expr list;
    clty_type: class_type;
    clty_path: Path.t;
    clty_variance: Variance.t list;
    clty_loc: Location.t;
    clty_attributes: Parsetree.attributes;
    clty_uid: Uid.t;
  }

(* Type expressions for the module language *)

type visibility =
  | Exported
  | Hidden

type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t

and functor_parameter =
  | Unit
  | Named of Ident.t option * module_type

and module_presence =
  | Mp_present
  | Mp_absent

and signature = signature_item list

and signature_item =
    Sig_value of Ident.t * value_description * visibility
  | Sig_type of Ident.t * type_declaration * rec_status * visibility
  | Sig_typext of Ident.t * extension_constructor * ext_status * visibility
  | Sig_module of
      Ident.t * module_presence * module_declaration * rec_status * visibility
  | Sig_modtype of Ident.t * modtype_declaration * visibility
  | Sig_class of Ident.t * class_declaration * rec_status * visibility
  | Sig_class_type of Ident.t * class_type_declaration * rec_status * visibility

and module_declaration =
  {
    md_type: module_type;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
    md_uid: Uid.t;
  }

and modtype_declaration =
  {
    mtd_type: module_type option;  (* Note: abstract *)
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
    mtd_uid: Uid.t;
  }

and rec_status =
    Trec_not                   (* first in a nonrecursive group *)
  | Trec_first                 (* first in a recursive group *)
  | Trec_next                  (* not first in a recursive/nonrecursive group *)

and ext_status =
    Text_first                     (* first constructor of an extension *)
  | Text_next                      (* not first constructor of an extension *)
  | Text_exception                 (* an exception *)


(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: Parsetree.attributes;
    cstr_inlined: type_declaration option;
    cstr_uid: Uid.t;
   }

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_unboxed                        (* Constructor of an unboxed type *)
  | Cstr_extension of Path.t * bool     (* Extension constructor
                                           true if a constant false if a block*)

let equal_tag t1 t2 =
  match (t1, t2) with
  | Cstr_constant i1, Cstr_constant i2 -> i2 = i1
  | Cstr_block i1, Cstr_block i2 -> i2 = i1
  | Cstr_unboxed, Cstr_unboxed -> true
  | Cstr_extension (path1, b1), Cstr_extension (path2, b2) ->
      Path.same path1 path2 && b1 = b2
  | (Cstr_constant _|Cstr_block _|Cstr_unboxed|Cstr_extension _), _ -> false

let may_equal_constr c1 c2 =
  c1.cstr_arity = c2.cstr_arity
  && (match c1.cstr_tag,c2.cstr_tag with
     | Cstr_extension _,Cstr_extension _ ->
         (* extension constructors may be rebindings of each other *)
         true
     | tag1, tag2 ->
         equal_tag tag1 tag2)

type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
   }

let rec bound_value_identifiers = function
    [] -> []
  | Sig_value(id, {val_kind = Val_reg}, _) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_typext(id, _, _, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_module(id, Mp_present, _, _, _) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_class(id, _, _, _) :: rem -> id :: bound_value_identifiers rem
  | _ :: rem -> bound_value_identifiers rem

let signature_item_id = function
  | Sig_value (id, _, _)
  | Sig_type (id, _, _, _)
  | Sig_typext (id, _, _, _)
  | Sig_module (id, _, _, _, _)
  | Sig_modtype (id, _, _)
  | Sig_class (id, _, _, _)
  | Sig_class_type (id, _, _, _)
    -> id

(* migrating repr from Btype.. *)

(**** Definitions for backtracking ****)

type change =
    Ctype of type_expr * type_desc
  | Ccompress of type_expr * type_desc * type_desc
  | Clevel of type_expr * int
  | Cscope of type_expr * int
  | Cname of
      (Path.t * type_expr list) option ref * (Path.t * type_expr list) option
  | Crow of row_field option ref * row_field option
  | Ckind of field_kind option ref * field_kind option
  | Ccommu of commutable ref * commutable
  | Cuniv of type_expr option ref * type_expr option

type changes =
    Change of change * changes ref
  | Unchanged
  | Invalid

let trail = Local_store.s_table ref Unchanged

let log_change ch =
  let r' = ref Unchanged in
  !trail := Change (ch, r');
  trail := r'

(**** Representative of a type ****)

let rec field_kind_repr =
  function
    Fvar {contents = Some kind} -> field_kind_repr kind
  | kind                        -> kind

let rec repr_link (t : type_expr) d : type_expr -> type_expr =
 function
   {desc = Tlink t' as d'} ->
     repr_link t d' t'
 | {desc = Tfield (_, k, _, t') as d'} when field_kind_repr k = Fabsent ->
     repr_link t d' t'
 | t' ->
     log_change (Ccompress (t, t.desc, d));
     t.desc <- d;
     t'

let repr_link1 t = function
   {desc = Tlink t' as d'} ->
     repr_link t d' t'
 | {desc = Tfield (_, k, _, t') as d'} when field_kind_repr k = Fabsent ->
     repr_link t d' t'
 | t' -> t'

let repr t =
  match t.desc with
   Tlink t' ->
     repr_link1 t t'
 | Tfield (_, k, _, t') when field_kind_repr k = Fabsent ->
     repr_link1 t t'
 | _ -> t

(* getters for type_expr *)

let get_desc t = (repr t).desc
let get_level t = (repr t).level
let get_scope t = (repr t).scope
let get_id t = (repr t).id

(* transient type_expr *)

module Transient_expr = struct
  let create desc ~level ~scope ~id = {desc; level; scope; id}
  let set_desc ty d = ty.desc <- d
  let set_stub_desc ty d = assert (ty.desc = Tvar None); ty.desc <- d
  let set_level ty lv = ty.level <- lv
  let set_scope ty sc = ty.scope <- sc
  let coerce ty = ty
  let repr = repr
  let type_expr ty = ty
end

(* Comparison for [type_expr]; cannot be used for functors *)

let eq_type t1 t2 = t1 == t2 || repr t1 == repr t2
let compare_type t1 t2 = compare (get_id t1) (get_id t2)

(**** Some type creators ****)

let new_id = Local_store.s_ref (-1)

let create_expr = Transient_expr.create

let newty3 ~level ~scope desc  =
  incr new_id;
  create_expr desc ~level ~scope ~id:!new_id

let newty2 ~level desc =
  newty3 ~level ~scope:Ident.lowest_scope desc

                  (**********************************)
                  (*  Utilities for backtracking    *)
                  (**********************************)

let undo_change = function
    Ctype  (ty, desc) -> Transient_expr.set_desc ty desc
  | Ccompress  (ty, desc, _) -> Transient_expr.set_desc ty desc
  | Clevel (ty, level) -> Transient_expr.set_level ty level
  | Cscope (ty, scope) -> Transient_expr.set_scope ty scope
  | Cname  (r, v) -> r := v
  | Crow   (r, v) -> r := v
  | Ckind  (r, v) -> r := v
  | Ccommu (r, v) -> r := v
  | Cuniv  (r, v) -> r := v

type snapshot = changes ref * int
let last_snapshot = Local_store.s_ref 0

let log_type ty =
  if ty.id <= !last_snapshot then log_change (Ctype (ty, ty.desc))
let link_type ty ty' =
  let ty = repr ty in
  let ty' = repr ty' in
  log_type ty;
  let desc = ty.desc in
  Transient_expr.set_desc ty (Tlink ty');
  (* Name is a user-supplied name for this unification variable (obtained
   * through a type annotation for instance). *)
  match desc, ty'.desc with
    Tvar name, Tvar name' ->
      begin match name, name' with
      | Some _, None -> log_type ty'; Transient_expr.set_desc ty' (Tvar name)
      | None, Some _ -> ()
      | Some _, Some _ ->
          if ty.level < ty'.level then
            (log_type ty'; Transient_expr.set_desc ty' (Tvar name))
      | None, None   -> ()
      end
  | _ -> ()
  (* ; assert (check_memorized_abbrevs ()) *)
  (*  ; check_expans [] ty' *)
(* TODO: consider eliminating set_type_desc, replacing it with link types *)
let set_type_desc ty td =
  let ty = repr ty in
  if td != ty.desc then begin
    log_type ty;
    Transient_expr.set_desc ty td
  end
(* TODO: separate set_level into two specific functions: *)
(*  set_lower_level and set_generic_level *)
let set_level ty level =
  let ty = repr ty in
  if level <> ty.level then begin
    if ty.id <= !last_snapshot then log_change (Clevel (ty, ty.level));
    Transient_expr.set_level ty level
  end
(* TODO: introduce a guard and rename it to set_higher_scope? *)
let set_scope ty scope =
  let ty = repr ty in
  if scope <> ty.scope then begin
    if ty.id <= !last_snapshot then log_change (Cscope (ty, ty.scope));
    Transient_expr.set_scope ty scope
  end
let set_univar rty ty =
  log_change (Cuniv (rty, !rty)); rty := Some ty
let set_name nm v =
  log_change (Cname (nm, !nm)); nm := v
let set_row_field e v =
  log_change (Crow (e, !e)); e := Some v
let set_kind rk k =
  log_change (Ckind (rk, !rk)); rk := Some k
let set_commu rc c =
  log_change (Ccommu (rc, !rc)); rc := c

let snapshot () =
  let old = !last_snapshot in
  last_snapshot := !new_id;
  (!trail, old)

let rec rev_log accu = function
    Unchanged -> accu
  | Invalid -> assert false
  | Change (ch, next) ->
      let d = !next in
      next := Invalid;
      rev_log (ch::accu) d

let backtrack ~cleanup_abbrev (changes, old) =
  match !changes with
    Unchanged -> last_snapshot := old
  | Invalid -> failwith "Btype.backtrack"
  | Change _ as change ->
      cleanup_abbrev ();
      let backlog = rev_log [] change in
      List.iter undo_change backlog;
      changes := Unchanged;
      last_snapshot := old;
      trail := changes

let rec rev_compress_log log r =
  match !r with
    Unchanged | Invalid ->
      log
  | Change (Ccompress _, next) ->
      rev_compress_log (r::log) next
  | Change (_, next) ->
      rev_compress_log log next

let undo_compress (changes, _old) =
  match !changes with
    Unchanged
  | Invalid -> ()
  | Change _ ->
      let log = rev_compress_log [] changes in
      List.iter
        (fun r -> match !r with
          Change (Ccompress (ty, desc, d), next) when ty.desc == d ->
            Transient_expr.set_desc ty desc; r := !next
        | _ -> ())
        log
