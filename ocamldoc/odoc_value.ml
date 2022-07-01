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

(** Representation and manipulation of values, class attributes and class methods. *)

module Name = Odoc_name

type t_value = {
    val_name : Name.t ;
    mutable val_info : Odoc_types.info option ;
    val_type : Types.type_expr ;
    val_recursive : bool ;
    mutable val_parameters : Odoc_parameter.parameter list ;
    mutable val_code : string option ;
    mutable val_loc : Odoc_types.location ;
  }

type t_attribute = {
    att_value : t_value ; (** an attribute has almost all the same information
                             as a value *)
    att_mutable : bool ;
    att_virtual : bool ;
  }

type t_method = {
    met_value : t_value ; (** a method has almost all the same information
                             as a value *)
    met_private : bool ;
    met_virtual : bool ;
  }

let value_parameter_text_by_name v name =
  match v.val_info with
    None -> None
  | Some i ->
      try
        let t = List.assoc name i.Odoc_types.i_params in
        Some t
      with
        Not_found ->
          None

let update_value_parameters_text v =
  let f p =
    Odoc_parameter.update_parameter_text (value_parameter_text_by_name v) p
  in
  List.iter f v.val_parameters

(** Create a list of (parameter name, typ) from a type, according to the arrows.
   [parameter_list_from_arrows t = [ a ; b ]] if t = a -> b -> c.*)
let parameter_list_from_arrows typ =
  let rec iter t =
    match Types.get_desc t with
      Types.Tarrow (l, t1, t2, _) ->
        (l, t1) :: (iter t2)
    | Types.Tlink texp
    | Types.Tpoly (texp, _) -> iter texp
    | Types.Tvar _
    | Types.Ttuple _
    | Types.Tconstr _
    | Types.Tobject _
    | Types.Tfield _
    | Types.Tnil
    | Types.Tunivar _
    | Types.Tpackage _
    | Types.Tvariant _ ->
        []
    | Types.Tsubst _ ->
        assert false
  in
  iter typ

let dummy_parameter_list typ =
  let normal_name = Odoc_misc.label_name in
  let liste_param = parameter_list_from_arrows typ in
  let rec iter (label, t) =
    match Types.get_desc t with
    | Types.Ttuple l ->
        let open Asttypes in
        if label = Nolabel then
          Odoc_parameter.Tuple
            (List.map (fun t2 -> iter (Nolabel, t2)) l, t)
        else
          (* if there is a label, then we don't want to decompose the tuple *)
          Odoc_parameter.Simple_name
            { Odoc_parameter.sn_name = normal_name label ;
              Odoc_parameter.sn_type = t ;
              Odoc_parameter.sn_text = None }
    | Types.Tlink t2 ->
        (iter (label, t2))
    | Types.Tsubst _ ->
        assert false
    | _ ->
        Odoc_parameter.Simple_name
          { Odoc_parameter.sn_name = normal_name label ;
             Odoc_parameter.sn_type = t ;
            Odoc_parameter.sn_text = None }
  in
  List.map iter liste_param

let is_function v =
  let rec f t =
    match Types.get_desc t with
      Types.Tarrow _ ->
        true
    | Types.Tlink t ->
        f t
        | _ ->
            false
      in
  f v.val_type
