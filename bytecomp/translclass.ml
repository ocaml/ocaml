(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Misc
open Asttypes
open Types
open Typedtree
open Lambda
open Translobj
open Translcore

(* XXX Rajouter des evenements... *)

let lfunction params body =
  match body with
    Lfunction (Curried, params', body') ->
      Lfunction (Curried, params @ params', body')
  |  _ ->
      Lfunction (Curried, params, body)

let lapply func args =
  match func with
    Lapply(func', args') ->
      Lapply(func', args' @ args)
  | _ ->
      Lapply(func, args)

let lsequence l1 l2 =
  if l2 = lambda_unit then l1 else Lsequence(l1, l2)

let transl_label l = Lconst (Const_base (Const_string l))

let rec transl_meth_list lst =
  Lconst
    (List.fold_right
       (fun lab rem -> Const_block (0, [Const_base (Const_string lab); rem]))
       lst (Const_pointer 0))

let set_inst_var obj id expr =
  let kind = if Typeopt.maybe_pointer expr then Paddrarray else Pintarray in
  Lprim(Parraysetu kind, [Lvar obj; Lvar id; transl_exp expr])

let copy_inst_var obj id expr templ offset =
  let kind = if Typeopt.maybe_pointer expr then Paddrarray else Pintarray in
  let id' = Ident.create (Ident.name id) in
  Llet(Strict, id', Lprim (Pidentity, [Lvar id]),
  Lprim(Parraysetu kind,
        [Lvar obj; Lvar id';
         Lprim(Parrayrefu kind, [Lvar templ; Lprim(Paddint,
                                                   [Lvar id';
                                                    Lvar offset])])]))

let transl_val tbl create name id rem =
  Llet(StrictOpt, id, Lapply (oo_prim (if create then "new_variable"
                                       else           "get_variable"),
                              [Lvar tbl; transl_label name]),
       rem)

let transl_vals tbl create vals rem =
  List.fold_right
    (fun (name, id) rem -> transl_val tbl create name id rem)
    vals rem

let transl_super tbl meths inh_methods rem =
  List.fold_right
    (fun (nm, id) rem ->
       begin try
         Llet(StrictOpt, id, Lapply (oo_prim "get_method",
                                     [Lvar tbl; Lvar (Meths.find nm meths)]),
              rem)
       with Not_found ->
         rem
       end)
    inh_methods rem

let rec build_object_init obj inh_init cl =
  match cl.cl_desc with
    Tclass_ident path ->
      let obj_init = Ident.create "obj_init" in
      (obj_init::inh_init, Lapply(Lvar obj_init, [Lvar obj]))
  | Tclass_structure str ->
      List.fold_right
        (fun field (inh_init, obj_init) ->
           match field with
             Cf_inher (cl, _, _) ->
               let (inh_init, obj_init') = build_object_init obj inh_init cl in
               (inh_init, lsequence obj_init' obj_init)
           | Cf_val (_, id, exp) ->
               (inh_init, lsequence (set_inst_var obj id exp) obj_init)
           | Cf_meth _ | Cf_init _ ->
               (inh_init, obj_init)
           | Cf_let (rec_flag, defs, vals) ->
               (inh_init,
                Translcore.transl_let rec_flag defs
                  (List.fold_right
                     (fun (id, expr) rem ->
                        lsequence (Lifused(id, set_inst_var obj id expr)) rem)
                     vals obj_init)))
        str.cl_field
        (inh_init, lambda_unit)
  | Tclass_fun (pat, vals, cl) ->
      let build params rem =
        let param = name_pattern "param" [pat, ()] in
        let rem =
          List.fold_right
            (fun (id, expr) rem ->
              lsequence (Lifused (id, set_inst_var obj id expr)) rem)
            vals rem
        in
        Lfunction (Curried, param::params,
                   Matching.for_function
                     pat.pat_loc None (Lvar param) [pat, rem])
      in
      let (inh_init, obj_init) =  build_object_init obj inh_init cl in
      (inh_init,
       begin match obj_init with
         Lfunction (Curried, params, rem) -> build params rem
       | rem                              -> build [] rem
       end)
  | Tclass_apply (cl, exprs) ->
      let (inh_init, obj_init) =  build_object_init obj inh_init cl in
      (inh_init, lapply obj_init (List.map transl_exp exprs))
  | Tclass_let (rec_flag, defs, vals, cl) ->
      let (inh_init, obj_init) =  build_object_init obj inh_init cl in
      (inh_init,
       Translcore.transl_let rec_flag defs
         (List.fold_right
            (fun (id, expr) rem ->
              lsequence (Lifused(id, set_inst_var obj id expr)) rem)
            vals obj_init))
  | Tclass_constraint (cl, vals, pub_meths, concr_meths) ->
      build_object_init obj inh_init cl

(*
let inherited_values vals =
  Lconst
    (List.fold_right
       (fun (v, _) rem ->
          Const_block(0, [Const_base (Const_string v); rem]))
       vals (Const_pointer 0))

let inherited_meths methods =
  Lconst
    (List.fold_right
       (fun v rem ->
          Const_block(0, [Const_base (Const_string v); rem]))
       methods (Const_pointer 0))
*)

let bind_method tbl public_methods lab id cl_init =
  if List.mem lab public_methods then
    Llet(Alias, id, Lvar (meth lab), cl_init)
  else
    Llet(StrictOpt, id, Lapply (oo_prim "get_method_label",
                                [Lvar tbl; transl_label lab]),
    cl_init)

let bind_methods tbl public_methods meths cl_init =
  Meths.fold (bind_method tbl public_methods) meths cl_init

let rec build_class_init cla pub_meths cstr inh_init cl_init cl =
  match cl.cl_desc with
    Tclass_ident path ->
      begin match inh_init with
        obj_init::inh_init ->
          (inh_init,
           Llet (Strict, obj_init, 
(*                 Lapply(oo_prim "get_class", [Lvar cla; transl_path path]), *)
                 Lapply(Lprim(Pfield 1,
                              [Lapply(Lprim(Pfield 1, [transl_path path]),
                                      [Lvar cla])]), [lambda_unit]),
                 cl_init))
      | _ ->
          assert false
      end
  | Tclass_structure str ->
      let (inh_init, cl_init) =
        List.fold_right
          (fun field (inh_init, cl_init) ->
            match field with
              Cf_inher (cl, vals, meths) ->
                build_class_init cla pub_meths false inh_init
                  (transl_vals cla false vals
                     (transl_super cla str.cl_meths meths cl_init))
                  cl
            | Cf_val (name, id, exp) ->
                (inh_init, transl_val cla true name id cl_init)
            | Cf_meth (name, exp) ->
                let met = Ident.create ("method_" ^ name) in
                (inh_init,
                 Lsequence(Lapply (oo_prim "set_method",
                                   [Lvar cla;
                                    Lvar (Meths.find name str.cl_meths);
                                    Llet(Strict, met, transl_exp exp,
                                         Lvar met)]),
                           cl_init))
            | Cf_let (rec_flag, defs, vals) ->
                let vals =
                  List.map (function (id, _) -> (Ident.name id, id)) vals
                in
                (inh_init, transl_vals cla true vals cl_init)
            | Cf_init exp ->
                (inh_init,
                 Lsequence(Lapply (oo_prim "add_initializer",
                                   [Lvar cla; transl_exp exp]),
                           cl_init)))
          str.cl_field
          (inh_init, cl_init)
      in
      (inh_init, bind_methods cla pub_meths str.cl_meths cl_init)
  | Tclass_fun (pat, vals, cl) ->
      let (inh_init, cl_init) =
        build_class_init cla pub_meths cstr inh_init cl_init cl
      in
      let vals = List.map (function (id, _) -> (Ident.name id, id)) vals in
      (inh_init, transl_vals cla true vals cl_init)
  | Tclass_apply (cl, exprs) ->
      build_class_init cla pub_meths cstr inh_init cl_init cl
  | Tclass_let (rec_flag, defs, vals, cl) ->
      let (inh_init, cl_init) =
        build_class_init cla pub_meths cstr inh_init cl_init cl
      in
      let vals = List.map (function (id, _) -> (Ident.name id, id)) vals in
      (inh_init, transl_vals cla true vals cl_init)
  | Tclass_constraint (cl, vals, meths, concr_meths) ->
      let core cl_init =
        build_class_init cla pub_meths true inh_init cl_init cl
      in
      if cstr then
        core cl_init
      else
        let virt_meths =
          List.fold_right
            (fun lab rem ->
               if Concr.mem lab concr_meths then rem else lab::rem)
            meths
            []
        in
        let (inh_init, cl_init) =
          core (Lsequence (Lapply (oo_prim "widen", [Lvar cla]),
                           cl_init))
        in
        (inh_init,
         Lsequence(Lapply (oo_prim "narrow",
                           [Lvar cla;
                             transl_meth_list vals;
                             transl_meth_list virt_meths;
                             transl_meth_list (Concr.elements concr_meths)]),
                   cl_init))

let rec make_params =
  function
    0 -> []
  | n -> (Ident.create "param") :: (make_params (n - 1))

let creator arity cla obj_init =
  let params = make_params arity in
  let params' = List.map (fun p -> Lvar p) params in
  let self = Ident.create "self" in
  let rem = Lvar self in
  let rem =
    if arity = 0 then rem else
    Lsequence(Lapply (oo_prim "run_initializers", [Lvar self; Lvar cla]),
              rem)
  in
  let body =
    Llet(Strict, self,
         Lapply (oo_prim "create_object", [Lvar cla]),
         Lsequence(Lapply (Lvar obj_init, (Lvar self) :: params'),
                   rem))
  in
  if arity = 0 then body else Lfunction (Curried, params, body)

(*
   XXX Il devrait etre peu couteux d'ecrire des classes :
     class c x y = d e f
*)
(*
   XXX
   Exploiter le fait que les methodes sont definies dans l'ordre pour
   l'initialisation des classes (et les variables liees par un
   let ???) ?
*)

(*
let rec simpl_class =
  function
    Tclass_ident path     -> true
  | Tclass_structure _    -> false
  | Tclass_fun (_, _, cl) -> simpl_class cl
  | Tclass_apply (cl, _)  -> simpl_class cl
  | Tclass_constraint (cl, _, _, _) -> simpl_class cl
*)
let rec transl_var_copy_rec inh_init self offset templ cl rem =
  match cl.cl_desc with
    Tclass_ident path ->
      begin match inh_init with
        obj_init::inh_init ->
          (inh_init, lsequence (Lapply (Lvar obj_init, [Lvar self])) rem)
      | _ ->
          assert false
      end
  | Tclass_structure str ->
      List.fold_right (fun field (inh_init, rem) ->
        match field with
          Cf_inher (cl, _, _) ->
            transl_var_copy_rec inh_init self offset templ cl rem
        | Cf_val (name, id, expr) ->
            (inh_init, lsequence (copy_inst_var self id expr templ offset) rem)
        | Cf_let (rec_flag, defs, vals) ->
            (inh_init,
             Translcore.transl_let rec_flag defs
               (List.fold_right
                  (fun (id, expr) rem ->
                    lsequence (Lifused(id,
                                       copy_inst_var self id expr
                                         templ offset))
                              rem)
                  vals rem))
        | Cf_init _ | Cf_meth _ ->
            (inh_init, rem))
        str.cl_field
        (inh_init, rem)
  | Tclass_constraint (cl, _, _, _) ->
      transl_var_copy_rec inh_init self offset templ cl rem
  | Tclass_fun _ | Tclass_apply _ | Tclass_let _ ->
      raise Exit

let transl_var_copy inh_init cl_id cla cl =
  try
    let templ = Ident.create "template" in
    let offset = Ident.create "offset" in
    let self = Ident.create "self" in
    let (inh_init, body) =
      transl_var_copy_rec inh_init self offset templ cl lambda_unit
    in
    assert (inh_init = []);
    Lfunction (Curried, [Ident.create "any"],
               Llet(StrictOpt, templ, Lprim(Pfield 0, [Lvar cl_id]),
               Llet(StrictOpt, offset,
                    Lprim(Psubint,
                          [Lprim(Pfield 0, [Lprim(Pfield 2, [Lvar cl_id])]);
                           Lprim(Pfield 0, [Lvar cla])]),
               Lfunction (Curried, [self], body))))
  with Exit ->
    Lapply(oo_prim "copy_variables", [Lvar cl_id; Lvar cla])

let transl_class cl_id arity pub_meths cl =
  let obj = Ident.create "self" in
  let (inh_init, obj_init) = build_object_init obj [] cl in
  let cla = Ident.create "class" in
  let obj_init =
    if arity = 0 then
      Lprim(Pmakeblock(0, Immutable),
            [lfunction [obj] obj_init;
(*
              Lapply(oo_prim "copy_variables", [Lvar cl_id; Lvar cla])])
*)
        transl_var_copy (List.rev inh_init) cl_id cla cl])
    else
      let init = Ident.create "init" in
      Llet(Strict, init, lfunction [obj] obj_init,
           Lprim(Pmakeblock(0, Immutable),
                 [Lvar init; Lfunction (Curried, [Ident.create "any"],
                                        Lvar init)]))
  in
  let (inh_init, cl_init) =
    build_class_init cla pub_meths true (List.rev inh_init) obj_init cl
  in
  assert (inh_init = []);
(*
  Lapply (oo_prim "create_class",
          [Lvar cl_id;
           transl_meth_list pub_meths;
           Lfunction(Curried, [cla; initial], cl_init);
           creator arity])
*)
  let table = Ident.create "table" in
  let class_init = Ident.create "class_init" in
  let obj_init = Ident.create "obj_init" in
  Llet(Strict, table,
       Lapply (oo_prim "create_table", [transl_meth_list pub_meths]),
  Llet(Strict, class_init,
       Lfunction(Curried, [cla], cl_init),
  Llet(Strict, obj_init,
       Lprim(Pfield 0, [Lapply(Lvar class_init, [Lvar table])]),
  Lsequence(Lapply (oo_prim "init_class", [Lvar table]),
            Lprim(Pmakeblock(0, Immutable),
                  [creator arity table obj_init;
                   Lvar class_init;
                   Lvar table])))))

let class_stub =
  Lprim(Pmakeblock(0, Mutable), [lambda_unit; lambda_unit; lambda_unit])
