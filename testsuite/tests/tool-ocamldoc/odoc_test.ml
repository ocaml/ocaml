(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: odoc_test.ml 6127 2004-02-20 16:28:27Z guesdon $ *)

(** Custom generator to perform test on ocamldoc. *)

open Odoc_info
open Odoc_info.Module
open Odoc_info.Type

type test_kind =
    Types_display

let p = Format.fprintf

class string_gen =
  object(self)
    inherit Odoc_info.Scan.scanner

    val mutable test_kinds = []
    val mutable fmt = Format.str_formatter 

    method must_display_types = List.mem Types_display test_kinds

    method set_test_kinds_from_module m =
      test_kinds <- List.fold_left
	  (fun acc (s, _) ->
	    match s with
	      "test_types_display" -> Types_display :: acc
	    | _ -> acc
	  )
	  []
	  (
	   match m.m_info with
	     None -> []
	   | Some i -> i.i_custom
	  )
    method scan_type t =
      match test_kinds with
	[] -> ()
      |	_ ->
	  p fmt "# type %s:\n" t.ty_name;
	  if self#must_display_types then
	    (
	     p fmt "# manifest (Odoc_info.string_of_type_expr):\n<[%s]>\n"
	       (match t.ty_manifest with
		 None -> "None"
	       | Some e -> Odoc_info.string_of_type_expr e
	       );
	    );


    method scan_module_pre m =
      p fmt "#\n# module %s:\n" m.m_name ;
      if self#must_display_types then
	(
	 p fmt "# Odoc_info.string_of_module_type:\n<[%s]>\n"
	   (Odoc_info.string_of_module_type m.m_type);
	 p fmt "# Odoc_info.string_of_module_type ~complete: true :\n<[%s]>\n"
	   (Odoc_info.string_of_module_type ~complete: true m.m_type);
	);
      true

    method scan_module_type_pre m =
      p fmt "#\n# module type %s:\n" m.mt_name ;
      if self#must_display_types then
	(
	 p fmt "# Odoc_info.string_of_module_type:\n<[%s]>\n"
	   (match m.mt_type with
	     None -> "None"
	   | Some t -> Odoc_info.string_of_module_type t
	   );
	 p fmt "# Odoc_info.string_of_module_type ~complete: true :\n<[%s]>\n"
	   (match m.mt_type with
	     None -> "None"
	   | Some t -> Odoc_info.string_of_module_type ~complete: true t
	   );
	);
      true

    method generate (module_list: Odoc_info.Module.t_module list) =
      let oc = open_out !Odoc_info.Args.out_file in
      fmt <- Format.formatter_of_out_channel oc;
      (
       try
	 List.iter
	   (fun m -> 
	     self#set_test_kinds_from_module m;
	     self#scan_module_list [m];
	   )
	   module_list
       with
	 e ->
	   prerr_endline (Printexc.to_string e)
      );
      Format.pp_print_flush fmt ();
      close_out oc
  end


let my_generator = new string_gen
let _ = Odoc_info.Args.set_doc_generator 
    (Some (my_generator :> Odoc_info.Args.doc_generator))
