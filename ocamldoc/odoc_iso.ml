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

(** A generator which performs some controls on the collected information. *)

open Odoc_info.Value
open Odoc_info.Type
open Odoc_info.Exception
open Odoc_info.Class
open Odoc_info.Module

open Odoc_info

let (<@) e l = List.mem e l

(** The generator class. *)
class iso =
  object (self)
    inherit Odoc_info.Scan.scanner

    method print_fail s = 
      incr Odoc_info.errors ;
      print_string s ; print_newline ()

    method check_info_error_messages =
      [
	Has_author, self#check_authors, Odoc_messages.has_no_author ;
	Has_since, self#check_since, Odoc_messages.has_no_since ;
	Has_version, self#check_version, Odoc_messages.has_no_version ;
	Has_return, self#check_return, Odoc_messages.has_no_return ;
      ] 

    method check_authors i = i.i_authors <> []
    method check_since i = i.i_since <> None
    method check_version i = i.i_version <> None
    method check_return i = i.i_return_value <> None

    method check_info prefix lchecks info_opt =
      match info_opt with
	None ->
	  if Has_description <@ lchecks then
	    self#print_fail (Odoc_messages.has_no_description prefix);

	  List.iter
	    (fun (check, f, m) ->
	      if check <@ lchecks then
		self#print_fail (m prefix)
	      else
		()
	    )
	    self#check_info_error_messages

      |	Some i ->
	  List.iter
	    (fun (check, f, m) ->
	      if check <@ lchecks then
		if not (f i) then
		  self#print_fail (m prefix)
		else
		  ()
	    )
	    self#check_info_error_messages

    method check_params l =
      let rec iter = function
	| Parameter.Simple_name sn ->
	    (sn.Parameter.sn_text <> None) or
	    (sn.Parameter.sn_name = "")
	| Parameter.Tuple (l, _) ->
	    List.for_all iter l
      in
      List.for_all iter l

    method check_type_fields l =
      List.for_all (fun f -> f.rf_text <> None) l

    method check_type_constructors l =
      List.for_all (fun c -> c.vc_text <> None) l

    method scan_value v = 
      let prefix = Odoc_messages.value_n v.val_name in
      self#check_info
	prefix
	!Odoc_args.iso_val_options 
	v.val_info;
      if Has_params <@ !Odoc_args.iso_val_options then
	if not (self#check_params v.val_parameters) then
	  self#print_fail (Odoc_messages.has_not_all_params_described prefix)

    method scan_type t = 
      let prefix = Odoc_messages.type_n t.ty_name in
      self#check_info
	prefix
	!Odoc_args.iso_type_options 
	t.ty_info;
      match t.ty_kind with
	Type.Type_record l when Has_fields_decribed <@ !Odoc_args.iso_type_options ->
	  if not (self#check_type_fields l) then
	    self#print_fail (Odoc_messages.has_not_all_fields_described prefix)

      |	 Type.Type_variant l when Has_constructors_decribed <@ !Odoc_args.iso_type_options ->
	  if not (self#check_type_constructors l) then
	    self#print_fail (Odoc_messages.has_not_all_cons_described prefix)

      | _ ->
	  ()

    method scan_exception e = 
      self#check_info
	(Odoc_messages.exception_n e.ex_name) 
	!Odoc_args.iso_exception_options 
	e.ex_info;

    method scan_attribute a = 
      self#check_info
	(Odoc_messages.attribute_n a.att_value.val_name) 
	!Odoc_args.iso_val_options 
	a.att_value.val_info;

    method scan_method m = 
      let prefix=  Odoc_messages.method_n m.met_value.val_name in
      self#check_info
	prefix
	!Odoc_args.iso_val_options 
	m.met_value.val_info;
      if Has_params <@ !Odoc_args.iso_val_options then
	if not (self#check_params m.met_value.val_parameters) then
	  self#print_fail (Odoc_messages.has_not_all_params_described prefix)

    method scan_class_pre c = 
      let prefix = Odoc_messages.class_n c.cl_name in
      self#check_info
	prefix
	!Odoc_args.iso_class_options 
	c.cl_info;
      if Has_params <@ !Odoc_args.iso_class_options then
	if not (self#check_params c.cl_parameters) then
	  self#print_fail (Odoc_messages.has_not_all_params_described prefix);
      true

    method scan_class_type_pre ct = 
      self#check_info
	(Odoc_messages.class_type_n ct.clt_name) 
	!Odoc_args.iso_class_options 
	ct.clt_info;
      true

    method scan_module_pre m = 
      let prefix = Odoc_messages.module_n m.m_name in
      self#check_info
	prefix
	!Odoc_args.iso_module_options 
	m.m_info; 
      true

    method scan_module_type_pre mt = 
      let prefix = Odoc_messages.module_type_n mt.mt_name in
       self#check_info
	prefix
	!Odoc_args.iso_module_options 
	mt.mt_info; 
      true

    method generate = self#scan_module_list

  end
