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

let create_object cl obj init =
  let obj' = Ident.create "self" in
  let (inh_init, obj_init) = init obj' in
  (inh_init,
   Llet(Strict, obj', Lifthenelse(Lvar obj, Lvar obj,
                                  Lapply (oo_prim "create_object", [Lvar cl])),
        Lsequence(obj_init,
        Lsequence(Lifthenelse(Lvar obj, lambda_unit,
                              Lapply (oo_prim "run_initializers",
                                      [Lvar obj'; Lvar cl])),
                  Lvar obj'))))

let rec build_object_init cl_table obj params inh_init cl =
  match cl.cl_desc with
    Tclass_ident path ->
      let obj_init = Ident.create "obj_init" in
      (obj_init::inh_init, Lapply(Lvar obj_init, [Lvar obj]))
  | Tclass_structure str ->
      create_object cl_table obj (fun obj ->
        let (inh_init, obj_init) =
          List.fold_right
            (fun field (inh_init, obj_init) ->
               match field with
                 Cf_inher (cl, _, _) ->
                   let (inh_init, obj_init') =
                     build_object_init cl_table obj [] inh_init cl
                   in
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
                            lsequence (Lifused(id, set_inst_var obj id expr))
                                      rem)
                         vals obj_init)))
            str.cl_field
            (inh_init, lambda_unit)
        in
        (inh_init,
         List.fold_right
           (fun (id, expr) rem ->
              lsequence (Lifused (id, set_inst_var obj id expr)) rem)
           params obj_init))
  | Tclass_fun (pat, vals, cl) ->
      let (inh_init, obj_init) =
        build_object_init cl_table obj (vals @ params) inh_init cl
      in
      (inh_init,
       let build params rem =
         let param = name_pattern "param" [pat, ()] in
         Lfunction (Curried, param::params,
                    Matching.for_function
                      pat.pat_loc None (Lvar param) [pat, rem])
       in
       begin match obj_init with
         Lfunction (Curried, params, rem) -> build params rem
       | rem                              -> build [] rem
       end)
  | Tclass_apply (cl, exprs) ->
      let (inh_init, obj_init) =
        build_object_init cl_table obj params inh_init cl
      in
      (inh_init, lapply obj_init (List.map transl_exp exprs))
  | Tclass_let (rec_flag, defs, vals, cl) ->
      let (inh_init, obj_init) =
        build_object_init cl_table obj (vals @ params) inh_init cl
      in
      (inh_init, Translcore.transl_let rec_flag defs obj_init)
  | Tclass_constraint (cl, vals, pub_meths, concr_meths) ->
      build_object_init cl_table obj params inh_init cl

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
                 Lapply(Lprim(Pfield 1, [transl_path path]),
                        [Lvar cla]),
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

let transl_class cl_id arity pub_meths cl =
  let cla = Ident.create "class" in
  let obj = Ident.create "self" in
  let (inh_init, obj_init) = build_object_init cla obj [] [] cl in
  let obj_init = lfunction [obj] obj_init in
  let (inh_init, cl_init) =
    build_class_init cla pub_meths true (List.rev inh_init) obj_init cl
  in
  assert (inh_init = []);
  let table = Ident.create "table" in
  let class_init = Ident.create "class_init" in
  let obj_init = Ident.create "obj_init" in
  Llet(Strict, table,
       Lapply (oo_prim "create_table", [transl_meth_list pub_meths]),
  Llet(Strict, class_init,
       Lfunction(Curried, [cla], cl_init),
  Llet(Strict, obj_init, Lapply(Lvar class_init, [Lvar table]),
  Lsequence(Lapply (oo_prim "init_class", [Lvar table]),
            Lprim(Pmakeblock(0, Immutable),
                  [Lvar obj_init;
                   Lvar class_init;
                   Lvar table])))))

let class_stub =
  Lprim(Pmakeblock(0, Mutable), [lambda_unit; lambda_unit; lambda_unit])
