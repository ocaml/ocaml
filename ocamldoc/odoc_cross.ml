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

(* $Id$ *)

(** Cross referencing. *)

module Name = Odoc_name
open Odoc_module
open Odoc_class
open Odoc_exception
open Odoc_types
open Odoc_value
open Odoc_type
open Odoc_parameter 

(*** Replacements of aliases : if e1 = e2 and e2 = e3, then replace e2 by e3 to have e1 = e3, 
   in order to associate the element with complete information. *)

(** The module used to keep what refs were modified. *)
module S = Set.Make 
    (
     struct type t = string * ref_kind option
       let compare = Pervasives.compare
     end
    )

let verified_refs = ref S.empty

let add_verified v = verified_refs := S.add v !verified_refs
let was_verified v = S.mem v !verified_refs

(** The module with the predicates used to get the aliased modules, classes and exceptions. *)
module P_alias =
  struct
    type t = int

    let p_module m _ = 
      (true,
       match m.m_kind with
         Module_alias _ -> true
       | _ -> false
      )
    let p_module_type mt _ =
      (true,
       match mt.mt_kind with
         Some (Module_type_alias _) -> true
       | _ -> false
      )
    let p_class c _ = (false, false)
    let p_class_type ct _ = (false, false)
    let p_value v _ = false
    let p_type t _ = false
    let p_exception e _ = e.ex_alias <> None
    let p_attribute a _ = false
    let p_method m _ = false
    let p_section s _ = false
  end

(** The module used to get the aliased elements. *)
module Search_alias = Odoc_search.Search (P_alias)

let rec build_alias_list (acc_m, acc_mt, acc_ex) = function
    [] ->
      (acc_m, acc_mt, acc_ex)
  | (Odoc_search.Res_module m) :: q ->
      let new_acc_m = 
        match m.m_kind with
          Module_alias ma -> (m.m_name, ma.ma_name) :: acc_m
        | _ -> acc_m
      in
      build_alias_list (new_acc_m, acc_mt, acc_ex) q
  | (Odoc_search.Res_module_type mt) :: q ->
      let new_acc_mt = 
        match mt.mt_kind with
          Some (Module_type_alias mta) -> (mt.mt_name, mta.mta_name) :: acc_mt
        | _ -> acc_mt
      in
      build_alias_list (acc_m, new_acc_mt, acc_ex) q
  | (Odoc_search.Res_exception e) :: q ->
      let new_acc_ex = 
        match e.ex_alias with
          None -> acc_ex
        | Some ea -> (e.ex_name, ea.ea_name) :: acc_ex
      in
      build_alias_list (acc_m, acc_mt, new_acc_ex) q
  | _ :: q ->
      build_alias_list (acc_m, acc_mt, acc_ex) q



(** Couples of module name aliases. *)
let module_aliases = ref [] ;;

(** Couples of module type name aliases. *)
let module_type_aliases = ref [] ;;

(** Couples of exception name aliases. *)
let exception_aliases = ref [] ;;

(** Retrieve the aliases for modules, module types and exceptions and put them in global variables. *)
let get_alias_names module_list =
  let (alias_m, alias_mt, alias_ex) = 
    build_alias_list
      ([], [], []) 
      (Search_alias.search module_list 0)
  in
  module_aliases := alias_m ;
  module_type_aliases := alias_mt ;
  exception_aliases := alias_ex 
  

(** The module with lookup predicates. *)
module P_lookup = 
  struct
    type t = Name.t
    let p_module m name = (Name.prefix m.m_name name, m.m_name = (Name.name_alias name !module_aliases))
    let p_module_type mt name = (Name.prefix mt.mt_name name, mt.mt_name = (Name.name_alias name (!module_aliases @ !module_type_aliases)))
    let p_class c name = (false, c.cl_name = (Name.name_alias name (!module_aliases @ !module_type_aliases)))
    let p_class_type ct name = (false, ct.clt_name = (Name.name_alias name (!module_aliases @ !module_type_aliases)))
    let p_value v name = false
    let p_type t name = false
    let p_exception e name = e.ex_name = (Name.name_alias name !exception_aliases)
    let p_attribute a name = false
    let p_method m name = false
    let p_section s name = false
  end

(** The module used to search by a complete name.*)
module Search_by_complete_name = Odoc_search.Search (P_lookup)

let rec lookup_module module_list name =
  let l = List.filter
      (fun res ->
        match res with
          Odoc_search.Res_module _ -> true
        | _ -> false
      )
      (Search_by_complete_name.search module_list name)
  in
  match l with
    (Odoc_search.Res_module m) :: _ -> m
  | _ -> raise Not_found

let rec lookup_module_type module_list name =
  let l = List.filter
      (fun res ->
        match res with
          Odoc_search.Res_module_type _ -> true
        | _ -> false
      )
      (Search_by_complete_name.search module_list name)
  in
  match l with
    (Odoc_search.Res_module_type mt) :: _ -> mt
  | _ -> raise Not_found

let rec lookup_class module_list name =
  let l = List.filter
      (fun res ->
        match res with
          Odoc_search.Res_class _ -> true
        | _ -> false
      )
      (Search_by_complete_name.search module_list name)
  in
  match l with
    (Odoc_search.Res_class c) :: _ -> c
  | _ -> raise Not_found

let rec lookup_class_type module_list name =
  let l = List.filter
      (fun res ->
        match res with
          Odoc_search.Res_class_type _ -> true
        | _ -> false
      )
      (Search_by_complete_name.search module_list name)
  in
  match l with
    (Odoc_search.Res_class_type ct) :: _ -> ct
  | _ -> raise Not_found

let rec lookup_exception module_list name =
  let l = List.filter
      (fun res ->
        match res with
          Odoc_search.Res_exception _ -> true
        | _ -> false
      )
      (Search_by_complete_name.search module_list name)
  in
  match l with
    (Odoc_search.Res_exception e) :: _ -> e
  | _ -> raise Not_found

(** The type to describe the names not found. *)
type not_found_name = 
    NF_m of Name.t
  | NF_mt of Name.t
  | NF_mmt of Name.t
  | NF_c of Name.t
  | NF_ct of Name.t
  | NF_cct of Name.t
  | NF_ex of Name.t

(** Functions to find and associate aliases elements. *)

let rec associate_in_module module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) m =
  let rec iter_kind (acc_b, acc_inc, acc_names) k =
    match k with
      Module_struct elements ->
        List.fold_left
          (associate_in_module_element module_list m.m_name)
          (acc_b, acc_inc, acc_names)
          elements
          
    | Module_alias ma ->
        (
         match ma.ma_module with
           Some _ ->
             (acc_b, acc_inc, acc_names)
         | None ->
             let mmt_opt =
               try Some (Mod (lookup_module module_list ma.ma_name))
               with Not_found ->
                 try Some (Modtype (lookup_module_type module_list ma.ma_name))
                 with Not_found -> None
             in
             match mmt_opt with
               None -> (acc_b, (Name.head m.m_name) :: acc_inc, 
                        (* we don't want to output warning messages for 
                           "sig ... end" or "struct ... end" modules not found *)
                        (if ma.ma_name = Odoc_messages.struct_end or 
                          ma.ma_name = Odoc_messages.sig_end then
                          acc_names
                        else
                          (NF_mmt ma.ma_name) :: acc_names)
                       )
             | Some mmt -> 
                 ma.ma_module <- Some mmt ;
                 (true, acc_inc, acc_names)
        )

    | Module_functor (_, k) ->
        iter_kind (acc_b, acc_inc, acc_names) k

    | Module_with (tk, _) ->
        associate_in_module_type module_list (acc_b, acc_inc, acc_names)
          { mt_name = "" ; mt_info = None ; mt_type = None ;
            mt_is_interface = false ; mt_file = ""; mt_kind = Some tk ;
            mt_loc = Odoc_types.dummy_loc }
          
    | Module_apply (k1, k2) ->
        let (acc_b2, acc_inc2, acc_names2) = iter_kind (acc_b, acc_inc, acc_names) k1 in
        iter_kind (acc_b2, acc_inc2, acc_names2) k2

    | Module_constraint (k, tk) ->
        let (acc_b2, acc_inc2, acc_names2) = iter_kind (acc_b, acc_inc, acc_names) k in
        associate_in_module_type module_list (acc_b2, acc_inc2, acc_names2)
          { mt_name = "" ; mt_info = None ; mt_type = None ;
            mt_is_interface = false ; mt_file = "" ; mt_kind = Some tk ;
            mt_loc = Odoc_types.dummy_loc }
  in
  iter_kind (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) m.m_kind
        
and associate_in_module_type module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) mt =
  let rec iter_kind (acc_b, acc_inc, acc_names) k =
    match k with
      Module_type_struct elements ->
        List.fold_left
          (associate_in_module_element module_list mt.mt_name)
          (acc_b, acc_inc, acc_names)
          elements

    | Module_type_functor (_, k) ->
        iter_kind (acc_b, acc_inc, acc_names) k

    | Module_type_with (k, _) ->
        iter_kind (acc_b, acc_inc, acc_names) k

    | Module_type_alias mta ->
        match mta.mta_module with
           Some _ ->
             (acc_b, acc_inc, acc_names)
         | None ->
             let mt_opt =
               try Some (lookup_module_type module_list mta.mta_name)
               with Not_found -> None
             in
             match mt_opt with
               None -> (acc_b, (Name.head mt.mt_name) :: acc_inc, 
                        (* we don't want to output warning messages for 
                           "sig ... end" or "struct ... end" modules not found *)
                        (if mta.mta_name = Odoc_messages.struct_end or 
                          mta.mta_name = Odoc_messages.sig_end then
                          acc_names 
                        else 
                          (NF_mt mta.mta_name) :: acc_names)
                       )
             | Some mt -> 
                 mta.mta_module <- Some mt ;
                 (true, acc_inc, acc_names)
  in
  match mt.mt_kind with
    None -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
  | Some k -> iter_kind (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) k
  
and associate_in_module_element module_list m_name (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) element =
   match element with
     Element_module m -> associate_in_module module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) m
   | Element_module_type mt -> associate_in_module_type module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) mt
   | Element_included_module im -> 
       (
        match im.im_module with
          Some _ -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
        | None ->
            let mmt_opt =
              try Some (Mod (lookup_module module_list im.im_name))
              with Not_found ->
                try Some (Modtype (lookup_module_type module_list im.im_name))
                with Not_found -> None
            in
            match mmt_opt with
              None -> (acc_b_modif, (Name.head m_name) :: acc_incomplete_top_module_names, 
                       (* we don't want to output warning messages for 
                           "sig ... end" or "struct ... end" modules not found *)
                        (if im.im_name = Odoc_messages.struct_end or 
                          im.im_name = Odoc_messages.sig_end then
                          acc_names_not_found
                        else
                          (NF_mmt im.im_name) :: acc_names_not_found)
                      )
            | Some mmt -> 
                im.im_module <- Some mmt ;
                (true, acc_incomplete_top_module_names, acc_names_not_found)
       )
   | Element_class cl -> associate_in_class module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) cl
   | Element_class_type ct -> associate_in_class_type module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) ct
   | Element_value _ -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
   | Element_exception ex ->
       (
        match ex.ex_alias with
          None -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
        | Some ea ->
            match ea.ea_ex with
              Some _ -> 
                (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
            | None -> 
                let ex_opt =
                  try Some (lookup_exception module_list ea.ea_name)
                  with Not_found -> None
                in
                match ex_opt with
                  None -> (acc_b_modif, (Name.head m_name) :: acc_incomplete_top_module_names, (NF_ex ea.ea_name) :: acc_names_not_found)
                | Some e ->
                    ea.ea_ex <- Some e ;
                    (true, acc_incomplete_top_module_names, acc_names_not_found)
       )
   | Element_type _ -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
   | Element_module_comment _ -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)

and associate_in_class module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) c =
  let rec iter_kind (acc_b, acc_inc, acc_names) k = 
    match k with
      Class_structure (inher_l, _) ->
        let f (acc_b2, acc_inc2, acc_names2) ic =
          match ic.ic_class with
          Some _ -> (acc_b2, acc_inc2, acc_names2)
        | None ->
            let cct_opt =
              try Some (Cl (lookup_class module_list ic.ic_name))
              with Not_found ->
                try Some (Cltype (lookup_class_type module_list ic.ic_name, []))
                with Not_found -> None
            in
            match cct_opt with
              None -> (acc_b2, (Name.head c.cl_name) :: acc_inc2,
                       (* we don't want to output warning messages for "object ... end" classes not found *)
                       (if ic.ic_name = Odoc_messages.object_end then acc_names2 else (NF_cct ic.ic_name) :: acc_names2))
            | Some cct -> 
                ic.ic_class <- Some cct ;
                (true, acc_inc2, acc_names2)
        in
        List.fold_left f (acc_b, acc_inc, acc_names) inher_l

    | Class_apply capp ->
        (
         match capp.capp_class with
           Some _ ->  (acc_b, acc_inc, acc_names)
         | None -> 
             let cl_opt =
               try Some (lookup_class module_list capp.capp_name)
               with Not_found -> None
             in
             match cl_opt with
               None -> (acc_b, (Name.head c.cl_name) :: acc_inc, 
                        (* we don't want to output warning messages for "object ... end" classes not found *)
                        (if capp.capp_name = Odoc_messages.object_end then acc_names else (NF_c capp.capp_name) :: acc_names))
             | Some c ->
                 capp.capp_class <- Some c ;
                 (true, acc_inc, acc_names)
        )

    | Class_constr cco ->
        (
         match cco.cco_class with
           Some _ ->  (acc_b, acc_inc, acc_names)
         | None -> 
             let cl_opt =
               try Some (lookup_class module_list cco.cco_name)
               with Not_found -> None
             in
             match cl_opt with
               None -> 
                 (
                  let clt_opt =
                    try Some (lookup_class_type module_list cco.cco_name)
                    with Not_found -> None
                  in
                  match clt_opt with
                    None ->
                      (acc_b, (Name.head c.cl_name) :: acc_inc, 
                        (* we don't want to output warning messages for "object ... end" classes not found *)
                       (if cco.cco_name = Odoc_messages.object_end then acc_names else (NF_cct cco.cco_name) :: acc_names))
                  | Some ct ->
                      cco.cco_class <- Some (Cltype (ct, [])) ;
                      (true, acc_inc, acc_names)
                 )
             | Some c ->
                 cco.cco_class <- Some (Cl c) ;
                 (true, acc_inc, acc_names)
        )
    | Class_constraint (ckind, ctkind) ->
        let (acc_b2, acc_inc2, acc_names2) = iter_kind (acc_b, acc_inc, acc_names) ckind in
        associate_in_class_type module_list (acc_b2, acc_inc2, acc_names2)
            { clt_name = "" ; clt_info = None ;
              clt_type = c.cl_type ; (* should be ok *)
              clt_type_parameters = [] ;
              clt_virtual = false ;
              clt_kind = ctkind ;
              clt_loc = Odoc_types.dummy_loc }
  in
  iter_kind (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) c.cl_kind

and associate_in_class_type module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) ct =
  let rec iter_kind (acc_b, acc_inc, acc_names) k = 
    match k with
      Class_signature (inher_l, _) ->
        let f (acc_b2, acc_inc2, acc_names2) ic =
          match ic.ic_class with
            Some _ -> (acc_b2, acc_inc2, acc_names2)
          | None ->
              let cct_opt =
                try Some (Cltype (lookup_class_type module_list ic.ic_name, []))
                with Not_found ->
                  try Some (Cl (lookup_class module_list ic.ic_name))
                  with Not_found -> None
              in
              match cct_opt with
                None -> (acc_b2, (Name.head ct.clt_name) :: acc_inc2, 
                         (* we don't want to output warning messages for "object ... end" class types not found *)
                         (if ic.ic_name = Odoc_messages.object_end then acc_names2 else (NF_cct ic.ic_name) :: acc_names2))
              | Some cct -> 
                  ic.ic_class <- Some cct ;
                  (true, acc_inc2, acc_names2)
        in
        List.fold_left f (acc_b, acc_inc, acc_names) inher_l

    | Class_type cta ->
        (
         match cta.cta_class with
           Some _ ->  (acc_b, acc_inc, acc_names)
         | None -> 
             let cct_opt =
               try Some (Cltype (lookup_class_type module_list cta.cta_name, []))
               with Not_found -> 
                 try Some (Cl (lookup_class module_list cta.cta_name))
                 with Not_found -> None
             in
             match cct_opt with
               None -> (acc_b, (Name.head ct.clt_name) :: acc_inc, 
                        (* we don't want to output warning messages for "object ... end" class types not found *)
                        (if cta.cta_name = Odoc_messages.object_end then acc_names else (NF_cct cta.cta_name) :: acc_names))
             | Some c ->
                 cta.cta_class <- Some c ;
                 (true, acc_inc, acc_names)
        )
  in
  iter_kind (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) ct.clt_kind

(*************************************************************)
(** Association of types to elements referenced in comments .*)

let ao = Odoc_misc.apply_opt 

let rec assoc_comments_text_elements module_list t_ele =
  match t_ele with
    | Raw _
    | Code _
    | CodePre _
    | Latex _
    | Verbatim _ -> t_ele
    | Bold t -> Bold (assoc_comments_text module_list t)
    | Italic t -> Italic (assoc_comments_text module_list t)
    | Center t -> Center (assoc_comments_text module_list t)
    | Left t -> Left (assoc_comments_text module_list t)
    | Right t -> Right (assoc_comments_text module_list t)
    | Emphasize t -> Emphasize (assoc_comments_text module_list t)
    | List l -> List (List.map (assoc_comments_text module_list) l)
    | Enum l -> Enum (List.map (assoc_comments_text module_list) l)
    | Newline -> Newline
    | Block t -> Block (assoc_comments_text module_list t)
    | Superscript t -> Superscript (assoc_comments_text module_list t)
    | Subscript t -> Subscript (assoc_comments_text module_list t)
    | Title (n, l_opt, t) -> Title (n, l_opt, (assoc_comments_text module_list t))
    | Link (s, t) -> Link (s, (assoc_comments_text module_list t))
    | Ref (name, None) ->
	(
	 (* we look for the first element with this name *)
         let re = Str.regexp ("^"^(Str.quote name)^"$") in
         let res = Odoc_search.Search_by_name.search module_list re in
         match res with
           [] ->
             Odoc_messages.pwarning (Odoc_messages.cross_element_not_found name);
             t_ele
         | ele :: _ ->
             let kind = 
               match ele with
                 Odoc_search.Res_module _ -> RK_module
               | Odoc_search.Res_module_type _ -> RK_module_type
               | Odoc_search.Res_class _ -> RK_class
               | Odoc_search.Res_class_type _ -> RK_class_type
               | Odoc_search.Res_value _ -> RK_value
               | Odoc_search.Res_type _ -> RK_type
               | Odoc_search.Res_exception _ -> RK_exception
               | Odoc_search.Res_attribute _ -> RK_attribute
               | Odoc_search.Res_method _ -> RK_method
               | Odoc_search.Res_section (_ ,t)-> RK_section t
             in
             add_verified (name, Some kind) ;
	     Ref (name, Some kind)
	)
    | Ref (name, Some kind) -> 
	let v = (name, Some kind) in
	(** we just verify that we find an element of this kind with this name *)
	let re = Str.regexp ("^"^(Str.quote name)^"$") in
        let res = Odoc_search.Search_by_name.search module_list re in
	if was_verified v then
	  Ref (name, Some kind)
	else
	  match kind with
	  | RK_section _ ->
	      (
	       try
		 let t = Odoc_search.find_section module_list re in
		 let v2 = (name, Some (RK_section t)) in
		 add_verified v2 ;
		 Ref (name, Some (RK_section t))
	       with
		 Not_found ->
		   Odoc_messages.pwarning (Odoc_messages.cross_section_not_found name);
		   Ref (name, None)
	      )
	  | _ ->
	      let (f,f_mes) = 
		match kind with
		  RK_module -> Odoc_search.module_exists, Odoc_messages.cross_module_not_found
		| RK_module_type -> Odoc_search.module_type_exists, Odoc_messages.cross_module_type_not_found
		| RK_class -> Odoc_search.class_exists, Odoc_messages.cross_class_not_found
		| RK_class_type -> Odoc_search.class_type_exists, Odoc_messages.cross_class_type_not_found
		| RK_value -> Odoc_search.value_exists, Odoc_messages.cross_value_not_found
		| RK_type -> Odoc_search.type_exists, Odoc_messages.cross_type_not_found
		| RK_exception -> Odoc_search.exception_exists, Odoc_messages.cross_exception_not_found
		| RK_attribute -> Odoc_search.attribute_exists, Odoc_messages.cross_attribute_not_found
		| RK_method -> Odoc_search.method_exists, Odoc_messages.cross_method_not_found
		| RK_section _ -> assert false
	      in
	      if f module_list re then
		(
		 add_verified v ;
		 Ref (name, Some kind)
		)
	      else
		(
		 Odoc_messages.pwarning (f_mes name);
		 Ref (name, None)
		)
	

and assoc_comments_text module_list text =
  List.map (assoc_comments_text_elements module_list) text

and assoc_comments_info module_list i =
  let ft = assoc_comments_text module_list in
  {
    i with
    i_desc = ao ft i.i_desc ;
    i_sees = List.map (fun (sr, t) -> (sr, ft t)) i.i_sees;
    i_deprecated = ao ft i.i_deprecated ;
    i_params = List.map (fun (name, t) -> (name, ft t)) i.i_params;
    i_raised_exceptions = List.map (fun (name, t) -> (name, ft t)) i.i_raised_exceptions;
    i_return_value = ao ft i.i_return_value ;
    i_custom = List.map (fun (tag, t) -> (tag, ft t)) i.i_custom ;
  } 
    

let rec assoc_comments_module_element module_list m_ele =
  match m_ele with
    Element_module m -> Element_module (assoc_comments_module module_list m)
  | Element_module_type mt -> Element_module_type (assoc_comments_module_type module_list mt)
  | Element_included_module _ -> m_ele (* don't go down into the aliases *)
  | Element_class c -> Element_class (assoc_comments_class module_list c)
  | Element_class_type ct -> Element_class_type (assoc_comments_class_type module_list ct)
  | Element_value v -> Element_value (assoc_comments_value module_list v)
  | Element_exception e -> Element_exception (assoc_comments_exception module_list e)
  | Element_type t -> Element_type (assoc_comments_type module_list t)
  | Element_module_comment t -> Element_module_comment (assoc_comments_text module_list t)

and assoc_comments_class_element module_list c_ele =
  match c_ele with
    Class_attribute a -> Class_attribute (assoc_comments_attribute module_list a)
  | Class_method m -> Class_method (assoc_comments_method module_list m)
  | Class_comment t -> Class_comment (assoc_comments_text module_list t)

and assoc_comments_module_kind module_list mk =
  match mk with
  | Module_struct eles -> 
      Module_struct (List.map (assoc_comments_module_element module_list) eles)
  | Module_alias _ 
  | Module_functor _ -> 
      mk
  | Module_apply (mk1, mk2) -> 
      Module_apply (assoc_comments_module_kind module_list mk1,
                    assoc_comments_module_kind module_list mk2)
  | Module_with (mtk, s) -> 
      Module_with (assoc_comments_module_type_kind module_list mtk, s)
  | Module_constraint (mk1, mtk) -> 
      Module_constraint (assoc_comments_module_kind module_list mk1,
                         assoc_comments_module_type_kind module_list mtk)

and assoc_comments_module_type_kind module_list mtk =
  match mtk with
  | Module_type_struct eles ->
      Module_type_struct (List.map (assoc_comments_module_element module_list) eles)
  | Module_type_functor (params, mtk1) -> 
      Module_type_functor (params, assoc_comments_module_type_kind module_list mtk1)
  | Module_type_alias _ ->
      mtk
  | Module_type_with (mtk1, s) ->
      Module_type_with (assoc_comments_module_type_kind module_list mtk1, s)

and assoc_comments_class_kind module_list ck =
  match ck with
    Class_structure (inher, eles) ->
      let inher2 = 
        List.map 
          (fun ic -> { ic with 
                       ic_text = ao (assoc_comments_text module_list) ic.ic_text })
          inher
      in
      Class_structure (inher2, List.map (assoc_comments_class_element module_list) eles)

  | Class_apply _
  | Class_constr _ -> ck
  | Class_constraint (ck1, ctk) ->
      Class_constraint (assoc_comments_class_kind module_list ck1,
                        assoc_comments_class_type_kind module_list ctk)

and assoc_comments_class_type_kind module_list ctk =
  match ctk with
    Class_signature (inher, eles) ->
      let inher2 = 
        List.map 
          (fun ic -> { ic with 
                       ic_text = ao (assoc_comments_text module_list) ic.ic_text })
          inher
      in
      Class_signature (inher2, List.map (assoc_comments_class_element module_list) eles)

  | Class_type _ -> ctk


and assoc_comments_module module_list m =
  m.m_info <- ao (assoc_comments_info module_list) m.m_info ;
  m.m_kind <- assoc_comments_module_kind module_list m.m_kind ;
  m

and assoc_comments_module_type module_list mt =
  mt.mt_info <- ao (assoc_comments_info module_list) mt.mt_info ;
  mt.mt_kind <- ao (assoc_comments_module_type_kind module_list) mt.mt_kind ;
  mt

and assoc_comments_class module_list c = 
  c.cl_info <- ao (assoc_comments_info module_list) c.cl_info ;
  c.cl_kind <- assoc_comments_class_kind module_list c.cl_kind ;
  assoc_comments_parameter_list module_list c.cl_parameters;
  c

and assoc_comments_class_type module_list ct =
  ct.clt_info <- ao (assoc_comments_info module_list) ct.clt_info ;
  ct.clt_kind <- assoc_comments_class_type_kind module_list ct.clt_kind ;
  ct

and assoc_comments_parameter module_list p =
  match p with
    Simple_name sn -> 
      sn.sn_text <- ao (assoc_comments_text module_list) sn.sn_text
  | Tuple (l, t) ->
      List.iter (assoc_comments_parameter module_list) l

and assoc_comments_parameter_list module_list pl =
  List.iter (assoc_comments_parameter module_list) pl

and assoc_comments_value module_list v =
  v.val_info <- ao (assoc_comments_info module_list) v.val_info ;
  assoc_comments_parameter_list module_list v.val_parameters;
  v

and assoc_comments_exception module_list e =
  e.ex_info <- ao (assoc_comments_info module_list) e.ex_info ;
  e

and assoc_comments_type module_list t =
  t.ty_info <- ao (assoc_comments_info module_list) t.ty_info ;
  (match t.ty_kind with
    Type_abstract -> ()
  | Type_variant (vl, _) ->
      List.iter 
        (fun vc -> vc.vc_text <- ao (assoc_comments_text module_list) vc.vc_text)
        vl 
  | Type_record (fl, _) ->
      List.iter 
        (fun rf -> rf.rf_text <- ao (assoc_comments_text module_list) rf.rf_text)
        fl
  );
  t

and assoc_comments_attribute module_list a =
  let _ = assoc_comments_value module_list a.att_value in
  a

and assoc_comments_method module_list m =
  let _ = assoc_comments_value module_list m.met_value in
  assoc_comments_parameter_list module_list m.met_value.val_parameters;
  m


let associate_type_of_elements_in_comments module_list =
  List.map (assoc_comments_module module_list) module_list


(***********************************************************)
(** The function which performs all the cross referencing. *)
let associate module_list =
  get_alias_names module_list ;
  let rec remove_doubles acc = function
      [] -> acc
    | h :: q ->
        if List.mem h acc then remove_doubles acc q
        else remove_doubles (h :: acc) q
  in
  let rec iter incomplete_modules =
    let (b_modif, remaining_inc_modules, acc_names_not_found) = 
      List.fold_left (associate_in_module module_list) (false, [], []) incomplete_modules
    in
    let remaining_no_doubles = remove_doubles [] remaining_inc_modules in
    let remaining_modules = List.filter
        (fun m -> List.mem m.m_name remaining_no_doubles)
        incomplete_modules
    in
    if b_modif then
      (* we may be able to associate something else *)
      iter remaining_modules
    else
      (* nothing changed, we won' be able to associate any more *)
      acc_names_not_found
  in
  let names_not_found = iter module_list in
  (
   match names_not_found with
     [] ->
       ()
   | l ->
       List.iter 
         (fun nf ->
           Odoc_messages.pwarning
             (
              match nf with
                NF_m n -> Odoc_messages.cross_module_not_found n
              | NF_mt n -> Odoc_messages.cross_module_type_not_found n
              | NF_mmt n -> Odoc_messages.cross_module_or_module_type_not_found n
              | NF_c n -> Odoc_messages.cross_class_not_found n
              | NF_ct n -> Odoc_messages.cross_class_type_not_found n
              | NF_cct n -> Odoc_messages.cross_class_or_class_type_not_found n
              | NF_ex n -> Odoc_messages.cross_exception_not_found n
             );
         )
         l
  ) ;

  (* Find a type for each name of element which is referenced in comments. *)
  let _ = associate_type_of_elements_in_comments module_list in
  ()
        

(* eof $Id$ *)
