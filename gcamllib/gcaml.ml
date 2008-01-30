(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                   Jun Furuse, University of Tokyo                   *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

generic val typeof : {'a} => 'a -> Rtype.type_expr =
  fun ty v -> ty

type dyn = Rtype.type_expr * Obj.t
exception Coercion_failure of Rtype.type_expr * Rtype.type_expr

generic val dyn : {'a} => 'a -> dyn =
  fun ty v -> ty, v

generic val coerce : {'a} => dyn -> 'a =
  fun ty (ty',v) ->
    (* We cannot use the normal value equality, i.e. ty = ty', since
       types may contain data type information with loops. *) 
    if Rtype.equal ty ty' then Obj.obj v
    else raise (Coercion_failure (ty', ty))

let type_of_dyn = (Obj.magic fst : dyn -> Rtype.type_expr)
let obj_of_dyn = (Obj.magic snd : dyn -> Obj.t)
let dyn_of ty obj = (ty, obj : dyn)
