(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Lambda

(* Get oo primitives identifiers *)

let oo_prim = Lambda.transl_prim "CamlinternalOO"

(* Share blocks *)

let consts : (structured_constant, Ident.t) Hashtbl.t = Hashtbl.create 17

let share c =
  match c with
    Const_block (_n, l) when l <> [] ->
      begin try
        Lvar (Hashtbl.find consts c)
      with Not_found ->
        let id = Ident.create_local "shared" in
        Hashtbl.add consts c id;
        Lvar id
      end
  | _ -> Lconst c

(* Collect labels *)

let meth_tag s = Lconst(Const_base(Const_int(Btype.hash_variant s)))

let reset_labels () =
  Hashtbl.clear consts

(* Also use it for required globals *)
let transl_label_init f =
  let expr, size = f () in
  let expr =
    Hashtbl.fold
      (fun c id expr -> Llet(Alias, Pgenval, id, Lconst c, expr))
      consts expr
  in
  (*let expr =
    List.fold_right
      (fun id expr -> Lsequence(Lprim(Pgetglobal id, [], Location.none), expr))
      (Env.get_required_globals ()) expr
  in
  Env.reset_required_globals ();*)
  reset_labels ();
  expr, size

(* Share classes *)

let wrapping = ref false
let cache_required = ref false
let top_env = ref Env.empty
let classes = ref []
let method_ids = ref Ident.Set.empty

let oo_add_class id =
  classes := id :: !classes;
  (!top_env, !cache_required)

let oo_wrap env req f x =
  if !wrapping then
    if !cache_required then f x else
      Misc.protect_refs [Misc.R (cache_required, true)] (fun () ->
          f x
        )
  else
    Misc.protect_refs [Misc.R (wrapping, true); Misc.R (top_env, env)]
      (fun () ->
         cache_required := req;
         classes := [];
         method_ids := Ident.Set.empty;
         let lambda = f x in
         let lambda =
           List.fold_left
             (fun lambda id ->
                Llet(StrictOpt, Pgenval, id,
                     Lprim(Pmakeblock(0, Mutable, None),
                           [lambda_unit; lambda_unit; lambda_unit],
                           Loc_unknown),
                     lambda))
             lambda !classes
         in
         lambda
      )

let reset () =
  Hashtbl.clear consts;
  wrapping := false;
  top_env := Env.empty;
  classes := [];
  method_ids := Ident.Set.empty
