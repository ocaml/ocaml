(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation and manipulation of method / function / class parameters. *)

(** Types *)

type simple_name = {
    sn_name : string ;
    sn_type : Types.type_expr ;
    mutable sn_text : Odoc_types.text option ;
  }

type param_info =
  | Simple_name of simple_name
  | Tuple of param_info list * Types.type_expr

type parameter = param_info

(** Functions *)

let complete_name p =
  let rec iter pi =
    match pi with
      Simple_name sn ->
        sn.sn_name
    | Tuple ([], _) -> (* anonymous parameter *)
        "??"
    | Tuple (pi_list, _) ->
        "("^(String.concat "," (List.map iter pi_list))^")"
  in
  iter p

let typ pi =
  match pi with
    Simple_name sn -> sn.sn_type
  | Tuple (_, typ) -> typ

let update_parameter_text f p =
  let rec iter pi =
    match pi with
      Simple_name sn ->
        sn.sn_text <- f sn.sn_name
    | Tuple (l, _) ->
        List.iter iter l
  in
  iter p

let desc_by_name pi name =
  let rec iter acc pi =
    match pi with
      Simple_name sn ->
        (sn.sn_name, sn.sn_text) :: acc
    | Tuple (pi_list, _) ->
        List.fold_left iter acc pi_list
      in
  let l = iter [] pi in
  List.assoc name l

let names pi =
  let rec iter acc pi =
    match pi with
      Simple_name sn ->
        sn.sn_name :: acc
    | Tuple (pi_list, _) ->
            List.fold_left iter acc pi_list
  in
  iter [] pi

let type_by_name pi name =
  let rec iter acc pi =
    match pi with
      Simple_name sn ->
        (sn.sn_name, sn.sn_type) :: acc
    | Tuple (pi_list, _) ->
        List.fold_left iter acc pi_list
      in
  let l = iter [] pi in
  List.assoc name l

let desc_from_info_opt info_opt s =
  match info_opt with
    None -> None
  | Some i ->
      match s with
        "" -> None
      | _ ->
          try
            Some (List.assoc s i.Odoc_types.i_params)
          with
            Not_found -> None
