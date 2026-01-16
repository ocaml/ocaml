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

(* Auxiliary functions for parsing *)

type error =
  | Unbound of string
  | Immutable_used_as_mutable of string

exception Error of error

type mutability = Immutable | Mutable

let tbl_ident : (string, (Backend_var.t * mutability)) Hashtbl.t =
  Hashtbl.create 57
let tbl_label = (Hashtbl.create 57 : (string, int) Hashtbl.t)

let ident_name s =
  match String.index s '/' with
  | exception Not_found -> s
  | n -> String.sub s 0 n

let bind_ident s mut =
  let id = Backend_var.create_local (ident_name s) in
  Hashtbl.add tbl_ident s (id, mut);
  Backend_var.With_provenance.create id

let find_ident s =
  match Hashtbl.find tbl_ident s with
  | exception Not_found ->
    raise(Error(Unbound s))
  | id, Immutable -> Cmm.Cvar id
  | id, Mutable -> Cmm.Cvar_mut id

let find_mut_ident s =
  match Hashtbl.find tbl_ident s with
  | exception Not_found ->
    raise(Error(Unbound s))
  | _, Immutable -> raise(Error(Immutable_used_as_mutable s))
  | id, Mutable -> id

let unbind_ident id =
  Hashtbl.remove tbl_ident (Backend_var.With_provenance.name id)

let find_label s =
  try
    Hashtbl.find tbl_label s
  with Not_found ->
    let lbl = Lambda.next_raise_count () in
    Hashtbl.add tbl_label s lbl;
    lbl

let report_error = function
  | Unbound s ->
      prerr_string "Unbound identifier "; prerr_string s; prerr_endline "."
  | Immutable_used_as_mutable s ->
      prerr_string "Identifier bound as immutable used as mutable: ";
      prerr_string s; prerr_endline "."

let debuginfo ?(loc=Location.symbol_rloc ()) () =
  Debuginfo.(from_location
               (Scoped_location.of_location
                  ~scopes:Scoped_location.empty_scopes loc
               )
            )
