(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Misc
open Asttypes
open Typedtree

let constructor_descrs ty_res cstrs =
  let num_consts = ref 0 and num_nonconsts = ref 0 in
  List.iter
    (function (name, []) -> incr num_consts
            | (name, _)  -> incr num_nonconsts)
    cstrs;
  let rec describe_constructors idx_const idx_nonconst = function
      [] -> []
    | (name, ty_args) :: rem ->
        let (tag, descr_rem) =
          match ty_args with
            [] -> (Cstr_constant idx_const,
                   describe_constructors (idx_const+1) idx_nonconst rem)
          | _  -> (Cstr_block idx_nonconst,
                   describe_constructors idx_const (idx_nonconst+1) rem) in
        let cstr =
          { cstr_res = ty_res;
            cstr_args = ty_args;
            cstr_arity = List.length ty_args;
            cstr_tag = tag;
            cstr_consts = !num_consts;
            cstr_nonconsts = !num_nonconsts } in
        (name, cstr) :: descr_rem in
  describe_constructors 0 0 cstrs

let exception_descr path_exc decl =
  { cstr_res = Predef.type_exn;
    cstr_args = decl;
    cstr_arity = List.length decl;
    cstr_tag = Cstr_exception path_exc;
    cstr_consts = -1;
    cstr_nonconsts = -1 }

let dummy_label =
  { lbl_res = Ttuple []; lbl_arg = Ttuple []; lbl_mut = Immutable;
    lbl_pos = (-1); lbl_all = [||] }

let label_descrs ty_res lbls =
  let all_labels = Array.new (List.length lbls) dummy_label in
  let rec describe_labels num = function
      [] -> []
    | (name, mut_flag, ty_arg) :: rest ->
        let lbl =
          { lbl_res = ty_res;
            lbl_arg = ty_arg;
            lbl_mut = mut_flag;
            lbl_pos = num;
            lbl_all = all_labels } in
        all_labels.(num) <- lbl;
        (name, lbl) :: describe_labels (num+1) rest in
  describe_labels 0 lbls
