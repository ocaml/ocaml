(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(** The functions to get a string from different kinds of elements (types, modules, ...). *)

module Name = Odoc_name

let string_of_type t =
  let module M = Odoc_type in
  "type "^
  (String.concat ""
     (List.map 
        (fun p -> (Odoc_misc.string_of_type_expr p)^" ")
        t.M.ty_parameters
     )
  )^
  (Name.simple t.M.ty_name)^" "^
  (match t.M.ty_manifest with
    None -> ""
  | Some typ -> "= "^(Odoc_misc.string_of_type_expr typ)^" "
  )^
  (match t.M.ty_kind with
    M.Type_abstract -> 
      ""
  | M.Type_variant (l, priv) ->
      "="^(if priv then " private" else "")^"\n"^
      (String.concat ""
         (List.map 
            (fun cons ->
              "  | "^cons.M.vc_name^
              (match cons.M.vc_args with
                [] -> "" 
              | l -> 
                  " of "^(String.concat " * " 
                            (List.map (fun t -> "("^(Odoc_misc.string_of_type_expr t)^")") l))
              )^
              (match cons.M.vc_text with
                None ->
                  ""
              | Some t ->
                  "(* "^(Odoc_misc.string_of_text t)^" *)"
              )^"\n"
            )
            l
         )
      )
  | M.Type_record (l, priv) ->
      "= "^(if priv then "private " else "")^"{\n"^
      (String.concat ""
         (List.map 
            (fun record ->
              "   "^(if record.M.rf_mutable then "mutable " else "")^
              record.M.rf_name^" : "^(Odoc_misc.string_of_type_expr record.M.rf_type)^";"^
              (match record.M.rf_text with
                None ->
                  ""
              | Some t ->
                  "(* "^(Odoc_misc.string_of_text t)^" *)"
              )^"\n"
            )
            l
         )
      )^
      "}\n"
  )^
  (match t.M.ty_info with
    None -> ""
  | Some info -> Odoc_misc.string_of_info info)

let string_of_exception e =
  let module M = Odoc_exception in
  "exception "^(Name.simple e.M.ex_name)^
  (match e.M.ex_args with
    [] -> ""
  | _ ->" : "^
      (String.concat " -> " 
         (List.map (fun t -> "("^(Odoc_misc.string_of_type_expr t)^")") e.M.ex_args)
      )
  )^
  (match e.M.ex_alias with
    None -> ""
  | Some ea ->
      " = "^
      (match ea.M.ea_ex with
        None -> ea.M.ea_name
      | Some e2 -> e2.M.ex_name
      )
  )^"\n"^
  (match e.M.ex_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)

let string_of_value v =
  let module M = Odoc_value in
  "val "^(Name.simple v.M.val_name)^" : "^
  (Odoc_misc.string_of_type_expr v.M.val_type)^"\n"^
  (match v.M.val_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)

let string_of_attribute a =
  let module M = Odoc_value in
  "val "^
  (if a.M.att_mutable then Odoc_messages.mutab^" " else "")^
  (Name.simple a.M.att_value.M.val_name)^" : "^
  (Odoc_misc.string_of_type_expr a.M.att_value.M.val_type)^"\n"^
  (match a.M.att_value.M.val_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)

let string_of_method m =
  let module M = Odoc_value in
  "method "^
  (if m.M.met_private then Odoc_messages.privat^" " else "")^
  (Name.simple m.M.met_value.M.val_name)^" : "^
  (Odoc_misc.string_of_type_expr m.M.met_value.M.val_type)^"\n"^
  (match m.M.met_value.M.val_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)
