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


(** The man pages generator. *)
open Odoc_info 
open Parameter
open Value
open Type
open Exception
open Class 
open Module
open Search


(** A class used to get a [text] for info structures. *)
class virtual info =
  object (self)
    (** The list of pairs [(tag, f)] where [f] is a function taking
       the [text] associated to [tag] and returning man code. 
       Add a pair here to handle a tag.*)
    val mutable tag_functions = ([] : (string * (Odoc_info.text -> string)) list)

    (** Return man code for a [text]. *)
    method virtual man_of_text : Odoc_info.text -> string

    (** Groff string for an author list. *)
    method man_of_author_list l =
      match l with
        [] ->
          ""
      | _ ->
          ".B \""^Odoc_messages.authors^"\"\n:\n"^
          (String.concat ", " l)^
          "\n.sp\n"

    (** Groff string for the given optional version information.*)
    method man_of_version_opt v_opt =
      match v_opt with
        None -> ""
      | Some v -> ".B \""^Odoc_messages.version^"\"\n:\n"^v^"\n.sp\n"

    (** Groff string for the given optional since information.*)
    method man_of_since_opt s_opt =
      match s_opt with
        None -> ""
      | Some s -> ".B \""^Odoc_messages.since^"\"\n"^s^"\n.sp\n"

    (** Groff string for the given list of raised exceptions.*)
    method man_of_raised_exceptions l =
      match l with
        [] -> ""
      | (s, t) :: [] -> ".B \""^Odoc_messages.raises^" "^s^"\"\n"^(self#man_of_text t)^"\n.sp\n"
      | _ ->
          ".B \""^Odoc_messages.raises^"\"\n"^
          (String.concat ""
             (List.map
                (fun (ex, desc) -> ".TP\n.B \""^ex^"\"\n"^(self#man_of_text desc)^"\n")
                l
             )
          )^"\n.sp\n"

    (** Groff string for the given "see also" reference. *)
    method man_of_see (see_ref, t)  =
      let t_ref = 
        match see_ref with
          Odoc_info.See_url s -> [ Odoc_info.Link (s, t) ]
        | Odoc_info.See_file s -> (Odoc_info.Code s) :: (Odoc_info.Raw " ") :: t
        | Odoc_info.See_doc s -> (Odoc_info.Italic [Odoc_info.Raw s]) :: (Odoc_info.Raw " ") :: t
      in
      self#man_of_text t_ref

    (** Groff string for the given list of "see also" references.*)
    method man_of_sees l =
      match l with
        [] -> ""
      | see :: [] -> ".B \""^Odoc_messages.see_also^"\"\n"^(self#man_of_see see)^"\n.sp\n"
      | _ ->
          ".B \""^Odoc_messages.see_also^"\"\n"^
          (String.concat ""
             (List.map
                (fun see -> ".TP\n \"\"\n"^(self#man_of_see see)^"\n")
                l
             )
          )^"\n.sp\n"

    (** Groff string for the given optional return information.*)
    method man_of_return_opt return_opt =
      match return_opt with
        None -> ""
      | Some s -> ".B "^Odoc_messages.returns^"\n"^(self#man_of_text s)^"\n.sp\n"

    (** Return man code for the given list of custom tagged texts. *)
    method man_of_custom l =
      let buf = Buffer.create 50 in
      List.iter
        (fun (tag, text) ->
          try
            let f = List.assoc tag tag_functions in
            Buffer.add_string buf (f text)
          with
            Not_found ->
              Odoc_info.warning (Odoc_messages.tag_not_handled tag)
        )
        l;
      Buffer.contents buf

    (** Return the groff string to display an optional info structure. *)
    method man_of_info info_opt =
        match info_opt with
        None ->
          ""
      | Some info ->
          let module M = Odoc_info in
          (match info.M.i_deprecated with
            None -> ""
          | Some d -> ".B \""^Odoc_messages.deprecated^"\"\n"^(self#man_of_text d)^"\n.sp\n")^
          (match info.M.i_desc with
            None -> "" 
          | Some d when d = [Odoc_info.Raw ""] -> ""
          | Some d -> (self#man_of_text d)^"\n.sp\n"
          )^
          (self#man_of_author_list info.M.i_authors)^
          (self#man_of_version_opt info.M.i_version)^
          (self#man_of_since_opt info.M.i_since)^
          (self#man_of_raised_exceptions info.M.i_raised_exceptions)^
          (self#man_of_return_opt info.M.i_return_value)^
          (self#man_of_sees info.M.i_sees)^
          (self#man_of_custom info.M.i_custom)
  end

(** This class is used to create objects which can generate a simple html documentation. *)
class man =
  let re_slash = Str.regexp_string "/" in
  object (self)
    inherit info

    (** Get a file name from a complete name. *)
    method file_name name = 
      let s = Printf.sprintf "%s.%s" name !Args.man_suffix in
      Str.global_replace re_slash "slash" s

    (** Escape special sequences of characters in a string. *)
    method escape (s : string) = s

    (** Open a file for output. Add the target directory.*)
    method open_out file =
      let f = Filename.concat !Args.target_dir file in
      open_out f
      
    (** Return the groff string for a text, without correction of blanks. *)
    method private man_of_text2 t = String.concat "" (List.map self#man_of_text_element t)

    (** Return the groff string for a text, with blanks corrected. *)
    method man_of_text t =
      let s = self#man_of_text2 t in
      let s2 = Str.global_replace (Str.regexp "\n[ ]*") "\n" s in
      Str.global_replace (Str.regexp "\n\n") "\n" s2

    (** Return the given string without no newlines. *)
    method remove_newlines s =
      Str.global_replace (Str.regexp "[ ]*\n[ ]*") " " s
      
    (** Return the groff string for a text element. *)
    method man_of_text_element te =
      match te with
      | Odoc_info.Raw s -> s
      | Odoc_info.Code s -> 
          let s2 = "\n.B "^(Str.global_replace (Str.regexp "\n") "\n.B " (self#escape s))^"\n" in
          s2
      | Odoc_info.CodePre s -> 
          let s2 = "\n.B "^(Str.global_replace (Str.regexp "\n") "\n.B " (self#escape s))^"\n" in
          s2
      | Odoc_info.Verbatim s -> self#escape s
      | Odoc_info.Bold t
      | Odoc_info.Italic t
      | Odoc_info.Emphasize t
      | Odoc_info.Center t
      | Odoc_info.Left t
      | Odoc_info.Right t -> self#man_of_text2 t
      | Odoc_info.List tl ->
          (String.concat ""
             (List.map
                (fun t -> ".TP\n \"\"\n"^(self#man_of_text2 t)^"\n")
                tl
             )
          )^"\n"
      | Odoc_info.Enum tl ->
          (String.concat ""
             (List.map
                (fun t -> ".TP\n \"\"\n"^(self#man_of_text2 t)^"\n")
                tl
             )
          )^"\n"
      | Odoc_info.Newline ->
          "\n.sp\n"
      | Odoc_info.Block t ->
          "\n.sp\n"^(self#man_of_text2 t)^"\n.sp\n"
      | Odoc_info.Title (n, l_opt, t) ->
          self#man_of_text2 [Odoc_info.Code (Odoc_info.string_of_text t)]
      | Odoc_info.Latex _ ->
          (* don't care about LaTeX stuff in HTML. *)
          ""
      | Odoc_info.Link (s, t) ->
          self#man_of_text2 t
      | Odoc_info.Ref (name, _) ->
          self#man_of_text_element 
            (Odoc_info.Code (Odoc_info.use_hidden_modules name))
      | Odoc_info.Superscript t ->
          "^{"^(self#man_of_text2 t)
      | Odoc_info.Subscript t ->
          "_{"^(self#man_of_text2 t)

    (** Groff string to display code. *)
    method man_of_code s = self#man_of_text [ Code s ]

    (** Take a string and return the string where fully qualified idents 
       have been replaced by idents relative to the given module name.*)
    method relative_idents m_name s =
      let f str_t = 
        let match_s = Str.matched_string str_t in
        Odoc_info.apply_if_equal 
          Odoc_info.use_hidden_modules 
          match_s
          (Name.get_relative m_name match_s)
      in
      let s2 = Str.global_substitute
          (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)")
          f
          s
      in
      s2

    (** Groff string to display a [Types.type_expr].*)
    method man_of_type_expr m_name t =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_misc.string_of_type_expr t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "\n.B " s in
      "\n.B "^(self#relative_idents m_name s2)^"\n"

    (** Groff string to display a [Types.class_type].*)
    method man_of_class_type_expr m_name t =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_misc.string_of_class_type t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "\n.B " s in
      "\n.B "^(self#relative_idents m_name s2)^"\n"

    (** Groff string to display a [Types.type_expr list].*)
    method man_of_type_expr_list m_name sep l =
      let s = Odoc_str.string_of_type_list sep l in
      let s2 = Str.global_replace (Str.regexp "\n") "\n.B " s in
      "\n.B "^(self#relative_idents m_name s2)^"\n"

    (** Groff string to display the parameters of a type.*)
    method man_of_type_expr_param_list m_name t =
      match t.ty_parameters with
        [] -> ""
      | l ->
	  let s = Odoc_str.string_of_type_param_list t in
	  let s2 = Str.global_replace (Str.regexp "\n") "\n.B " s in
	  "\n.B "^(self#relative_idents m_name s2)^"\n"

    (** Groff string to display a [Types.module_type]. *)
    method man_of_module_type m_name t =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_misc.string_of_module_type t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "\n.B " s in
      "\n.B "^(self#relative_idents m_name s2)^"\n"

    (** Groff string code for a value. *)
    method man_of_value v =
      Odoc_info.reset_type_names () ;
      "\n.I val "^(Name.simple v.val_name)^" \n: "^
      (self#man_of_type_expr (Name.father v.val_name) v.val_type)^
      ".sp\n"^
      (self#man_of_info v.val_info)^
      "\n.sp\n"

    (** Groff string code for an exception. *)
    method man_of_exception e =
      Odoc_info.reset_type_names () ;
      "\n.I exception "^(Name.simple e.ex_name)^" \n"^
      (match e.ex_args with
        [] -> ""
      | _ -> 
          ".B of "^
          (self#man_of_type_expr_list (Name.father e.ex_name) " * " e.ex_args)
      )^
      (match e.ex_alias with
        None -> ""
      | Some ea -> " = "^
          (
           match ea.ea_ex with
             None -> ea.ea_name
           | Some e -> e.ex_name
          )
      )^
      "\n.sp\n"^
      (self#man_of_info e.ex_info)^
      "\n.sp\n"

    (** Groff string for a type. *)
    method man_of_type t =
      Odoc_info.reset_type_names () ;
      let father = Name.father t.ty_name in
      ".I type "^
      (self#man_of_type_expr_param_list father t)^
      (match t.ty_parameters with [] -> "" | _ -> ".I ")^(Name.simple t.ty_name)^" \n"^
      (match t.ty_manifest with None -> "" | Some typ -> "= "^(self#man_of_type_expr father typ))^
      (
       match t.ty_kind with
        Type_abstract -> 
          ""
      | Type_variant (l, priv) ->
          "="^(if priv then " private" else "")^"\n "^
          (String.concat ""
             (List.map 
                (fun constr ->
                  "| "^constr.vc_name^
                  (match constr.vc_args, constr.vc_text with
                    [], None -> "\n " 
                  | [], (Some t) -> "  (* "^(self#man_of_text t)^" *)\n "
                  | l, None -> 
                      "\n.B of "^(self#man_of_type_expr_list father " * " l)^" "
                  | l, (Some t) ->
                      "\n.B of "^(self#man_of_type_expr_list father " * " l)^
                      ".I \"  \"\n"^
                      "(* "^(self#man_of_text t)^" *)\n "
                  )
                )
                l
             )
          )
      | Type_record (l, priv) ->
          "= "^(if priv then "private " else "")^"{"^
          (String.concat ""
             (List.map 
                (fun r ->
                  (if r.rf_mutable then "\n\n.B mutable \n" else "\n ")^
                  r.rf_name^" : "^(self#man_of_type_expr father r.rf_type)^";"^
                  (match r.rf_text with
                    None ->
                      ""
                  | Some t ->
                      "  (* "^(self#man_of_text t)^" *) "
                  )^""
                )
                l
             )
          )^
          "\n }\n"
      )^
      "\n.sp\n"^(self#man_of_info t.ty_info)^
      "\n.sp\n"

    (** Groff string for a class attribute. *)
    method man_of_attribute a =
      ".I val "^
      (if a.att_mutable then Odoc_messages.mutab^" " else "")^
      (Name.simple a.att_value.val_name)^" : "^
      (self#man_of_type_expr (Name.father a.att_value.val_name) a.att_value.val_type)^
      "\n.sp\n"^(self#man_of_info a.att_value.val_info)^
      "\n.sp\n"

    (** Groff string for a class method. *)
    method man_of_method m =
      ".I method "^
      (if m.met_private then "private " else "")^
      (if m.met_virtual then "virtual " else "")^
      (Name.simple m.met_value.val_name)^" : "^
      (self#man_of_type_expr (Name.father m.met_value.val_name) m.met_value.val_type)^
      "\n.sp\n"^(self#man_of_info m.met_value.val_info)^
      "\n.sp\n"

    (** Groff for a list of parameters. *)
    method man_of_parameter_list m_name l =
      match l with
        [] ->
          ""
      | _ ->
          "\n.B "^Odoc_messages.parameters^": \n"^
          (String.concat ""
             (List.map
                (fun p ->
                  ".TP\n"^
                  "\""^(Parameter.complete_name p)^"\"\n"^
                  (self#man_of_type_expr m_name (Parameter.typ p))^"\n"^
                  (self#man_of_parameter_description p)^"\n"
                )
                l
             )
          )^"\n"

    (** Groff for the description of a function parameter. *)
    method man_of_parameter_description p =
      match Parameter.names p with
        [] ->
          ""
      | name :: [] ->
          (
           (* Only one name, no need for label for the description. *)
           match Parameter.desc_by_name p name with
             None -> ""
           | Some t -> "\n "^(self#man_of_text t)
          )
      | l ->
          (*  A list of names, we display those with a description. *)
          String.concat ""
            (List.map
               (fun n ->
                 match Parameter.desc_by_name p n with
                   None -> ""
                 | Some t -> (self#man_of_code (n^" : "))^(self#man_of_text t)
               )
               l
            )

    (** Groff string for a list of module parameters. *)
    method man_of_module_parameter_list m_name l =
      match l with
        [] ->
          ""
      | _ ->
          ".B \""^Odoc_messages.parameters^":\"\n"^
          (String.concat ""
             (List.map
                (fun (p, desc_opt) ->
                  ".TP\n"^
                  "\""^p.mp_name^"\"\n"^
                  (self#man_of_module_type m_name p.mp_type)^"\n"^
                  (match desc_opt with
                    None -> ""
                  | Some t -> self#man_of_text t)^
                  "\n"
                )
                l
             )
          )^"\n\n"

    (** Groff string for a class. *)
    method man_of_class c =
      let buf = Buffer.create 32 in
      let p = Printf.bprintf in
      Odoc_info.reset_type_names () ;
      let father = Name.father c.cl_name in
      p buf ".I class %s"
        (if c.cl_virtual then "virtual " else "");
      (
       match c.cl_type_parameters with
         [] -> ()
       | l -> p buf "[%s.I] " (Odoc_str.string_of_type_list ", " l)
      );
      p buf "%s : %s" 
        (Name.simple c.cl_name)
        (self#man_of_class_type_expr (Name.father c.cl_name) c.cl_type);
      p buf "\n.sp\n%s\n.sp\n" (self#man_of_info c.cl_info);
      Buffer.contents buf

    (** Groff string for a class type. *)
    method man_of_class_type ct =
      let buf = Buffer.create 32 in
      let p = Printf.bprintf in
      Odoc_info.reset_type_names () ;
      p buf ".I class type %s"
        (if ct.clt_virtual then "virtual " else "");
      (
       match ct.clt_type_parameters with
        [] -> ()
      | l -> p buf "[%s.I ] " (Odoc_str.string_of_type_list ", " l)
      );
      p buf "%s = %s" 
        (Name.simple ct.clt_name)
        (self#man_of_class_type_expr (Name.father ct.clt_name) ct.clt_type);
      p buf "\n.sp\n%s\n.sp\n" (self#man_of_info ct.clt_info);
      Buffer.contents buf

    (** Groff string for a module. *)
    method man_of_module m =
      ".I module "^(Name.simple m.m_name)^
      " : "^(self#man_of_module_type (Name.father m.m_name) m.m_type)^
      "\n.sp\n"^(self#man_of_info m.m_info)^"\n.sp\n"

    (** Groff string for a module type. *)
    method man_of_modtype mt =
      ".I module type "^(Name.simple mt.mt_name)^
      " = "^
      (match mt.mt_type with 
        None -> ""
      | Some t -> self#man_of_module_type (Name.father mt.mt_name) t
      )^
      "\n.sp\n"^(self#man_of_info mt.mt_info)^"\n.sp\n"

    (** Groff string for a module comment.*)
    method man_of_module_comment text =
      "\n.pp\n"^
      (self#man_of_text [Code ("=== "^(Odoc_misc.string_of_text text)^" ===")])^
      "\n.pp\n"

    (** Groff string for a class comment.*)
    method man_of_class_comment text =
      "\n.pp\n"^
      (self#man_of_text [Code ("=== "^(Odoc_misc.string_of_text text)^" ===")])^
      "\n.pp\n"

    (** Groff string for an included module. *)
    method man_of_included_module m_name im =
      ".I include "^
      (
       match im.im_module with
         None -> im.im_name
       | Some mmt ->
           let name = 
             match mmt with
               Mod m -> m.m_name
             | Modtype mt -> mt.mt_name
           in
           self#relative_idents m_name name
      )^
      "\n.sp\n"

    (** Generate the man page for the given class.*)
    method generate_for_class cl =
      Odoc_info.reset_type_names () ;
      let date = Unix.time () in
      let file = self#file_name cl.cl_name in
      try
        let chanout = self#open_out file in
        output_string chanout
          (".TH \""^Odoc_messages.clas^"\" "^
           cl.cl_name^" "^
           "\""^(Odoc_misc.string_of_date ~hour: false date)^"\" "^ 
           "OCamldoc "^
           "\""^(match !Args.title with Some t -> t | None -> "")^"\"\n");

	let abstract = 
	  match cl.cl_info with
	    None | Some { i_desc = None } -> "no description"
	  | Some { i_desc = Some t } ->
	      let s = Odoc_info.string_of_text (Odoc_info.first_sentence_of_text t) in
	      self#remove_newlines s
	in

        output_string chanout
          (
	   ".SH NAME\n"^
	   cl.cl_name^" \\- "^abstract^"\n"^
           ".SH "^Odoc_messages.clas^"\n"^
           Odoc_messages.clas^"   "^cl.cl_name^"\n"^
           ".SH "^Odoc_messages.documentation^"\n"^
           ".sp\n"
          );
        output_string chanout (self#man_of_class cl);

        (* parameters *)
        output_string chanout 
          (self#man_of_parameter_list "" cl.cl_parameters);
        (* a large blank *)
        output_string chanout "\n.sp\n.sp\n";

(*
        (* class inheritance *)
        self#generate_class_inheritance_info chanout cl;
*)
        (* the various elements *)
        List.iter 
          (fun element ->
            match element with
              Class_attribute a ->
                output_string chanout (self#man_of_attribute a)
            | Class_method m ->
                output_string chanout (self#man_of_method m)
            | Class_comment t ->
                output_string chanout (self#man_of_class_comment t)
          )
          (Class.class_elements cl);

        close_out chanout
      with
        Sys_error s ->
          incr Odoc_info.errors ;
          prerr_endline s

    (** Generate the man page for the given class type.*)
    method generate_for_class_type ct =
      Odoc_info.reset_type_names () ;
      let date = Unix.time () in
      let file = self#file_name ct.clt_name in
      try
        let chanout = self#open_out file in
        output_string chanout
          (".TH \""^Odoc_messages.class_type^"\" "^
           ct.clt_name^" "^
           "\""^(Odoc_misc.string_of_date ~hour: false date)^"\" "^ 
           "OCamldoc "^
           "\""^(match !Args.title with Some t -> t | None -> "")^"\"\n");

	let abstract = 
	  match ct.clt_info with
	    None | Some { i_desc = None } -> "no description"
	  | Some { i_desc = Some t } ->
	      let s = Odoc_info.string_of_text (Odoc_info.first_sentence_of_text t) in
	      self#remove_newlines s
	in

        output_string chanout
          (
	   ".SH NAME\n"^
	   ct.clt_name^" \\- "^abstract^"\n"^
           ".SH "^Odoc_messages.class_type^"\n"^
           Odoc_messages.class_type^"   "^ct.clt_name^"\n"^
           ".SH "^Odoc_messages.documentation^"\n"^
           ".sp\n"
          );
        output_string chanout (self#man_of_class_type ct);

        (* a large blank *)
        output_string chanout "\n.sp\n.sp\n";
(*
        (* class inheritance *)
        self#generate_class_inheritance_info chanout cl;
*)
        (* the various elements *)
        List.iter 
          (fun element ->
            match element with
              Class_attribute a ->
                output_string chanout (self#man_of_attribute a)
            | Class_method m ->
                output_string chanout (self#man_of_method m)
            | Class_comment t ->
                output_string chanout (self#man_of_class_comment t)
          )
          (Class.class_type_elements ct);

        close_out chanout
      with
        Sys_error s ->
          incr Odoc_info.errors ;
          prerr_endline s

    (** Generate the man file for the given module type. 
       @raise Failure if an error occurs.*)
    method generate_for_module_type mt =
      let date = Unix.time () in
      let file = self#file_name mt.mt_name in
      try
        let chanout = self#open_out file in
        output_string chanout
          (".TH \""^Odoc_messages.module_type^"\" "^
           mt.mt_name^" "^
           "\""^(Odoc_misc.string_of_date ~hour: false date)^"\" "^ 
           "OCamldoc "^
           "\""^(match !Args.title with Some t -> t | None -> "")^"\"\n");

	let abstract = 
	  match mt.mt_info with
	    None | Some { i_desc = None } -> "no description"
	  | Some { i_desc = Some t } ->
	      let s = Odoc_info.string_of_text (Odoc_info.first_sentence_of_text t) in
	      self#remove_newlines s
	in
        output_string chanout
          (
	   ".SH NAME\n"^
	   mt.mt_name^" \\- "^abstract^"\n"^
           ".SH "^Odoc_messages.module_type^"\n"^
           Odoc_messages.module_type^"   "^mt.mt_name^"\n"^
           ".SH "^Odoc_messages.documentation^"\n"^
           ".sp\n"^
           Odoc_messages.module_type^"\n"^
           ".BI \""^(Name.simple mt.mt_name)^"\"\n"^
           " = "^
           (match mt.mt_type with
             None -> ""
           | Some t -> self#man_of_module_type (Name.father mt.mt_name) t
           )^
           "\n.sp\n"^
           (self#man_of_info mt.mt_info)^"\n"^
           ".sp\n"
          );

        (* parameters for functors *)
        output_string chanout 
          (self#man_of_module_parameter_list "" (Module.module_type_parameters mt));
        (* a large blank *)
        output_string chanout "\n.sp\n.sp\n";

        (* module elements *)
        List.iter 
          (fun ele ->
            match ele with
              Element_module m ->
                output_string chanout (self#man_of_module m)
            | Element_module_type mt ->
                output_string chanout (self#man_of_modtype mt)
            | Element_included_module im ->
                output_string chanout (self#man_of_included_module mt.mt_name im)
            | Element_class c ->
                output_string chanout (self#man_of_class c)
            | Element_class_type ct ->
                output_string chanout (self#man_of_class_type ct)
            | Element_value v ->
                output_string chanout (self#man_of_value v)
            | Element_exception e ->
                output_string chanout (self#man_of_exception e)
            | Element_type t ->
                output_string chanout (self#man_of_type t)
            | Element_module_comment text ->
                output_string chanout (self#man_of_module_comment text)
          )
          (Module.module_type_elements mt);

        close_out chanout

      with
        Sys_error s ->
          incr Odoc_info.errors ;
          prerr_endline s

    (** Generate the man file for the given module. 
       @raise Failure if an error occurs.*)
    method generate_for_module m =
      let date = Unix.time () in
      let file = self#file_name m.m_name in
      try
        let chanout = self#open_out file in
        output_string chanout
          (".TH \""^Odoc_messages.modul^"\" "^
           m.m_name^" "^
           "\""^(Odoc_misc.string_of_date ~hour: false date)^"\" "^ 
           "OCamldoc "^
           "\""^(match !Args.title with Some t -> t | None -> "")^"\"\n");

	let abstract = 
	  match m.m_info with
	    None | Some { i_desc = None } -> "no description"
	  | Some { i_desc = Some t } ->
	      let s = Odoc_info.string_of_text (Odoc_info.first_sentence_of_text t) in
	      self#remove_newlines s
	in

        output_string chanout
          (
	   ".SH NAME\n"^
	   m.m_name^" \\- "^abstract^"\n"^
           ".SH "^Odoc_messages.modul^"\n"^
           Odoc_messages.modul^"   "^m.m_name^"\n"^
           ".SH "^Odoc_messages.documentation^"\n"^
           ".sp\n"^
           Odoc_messages.modul^"\n"^
           ".BI \""^(Name.simple m.m_name)^"\"\n"^
           " : "^(self#man_of_module_type (Name.father m.m_name) m.m_type)^
           "\n.sp\n"^
           (self#man_of_info m.m_info)^"\n"^
           ".sp\n"
          );

        (* parameters for functors *)
        output_string chanout 
          (self#man_of_module_parameter_list "" (Module.module_parameters m));
        (* a large blank *)
        output_string chanout "\n.sp\n.sp\n";

        (* module elements *)
        List.iter 
          (fun ele ->
            match ele with
              Element_module m ->
                output_string chanout (self#man_of_module m)
            | Element_module_type mt ->
                output_string chanout (self#man_of_modtype mt)
            | Element_included_module im ->
                output_string chanout (self#man_of_included_module m.m_name im)
            | Element_class c ->
                output_string chanout (self#man_of_class c)
            | Element_class_type ct ->
                output_string chanout (self#man_of_class_type ct)
            | Element_value v ->
                output_string chanout (self#man_of_value v)
            | Element_exception e ->
                output_string chanout (self#man_of_exception e)
            | Element_type t ->
                output_string chanout (self#man_of_type t)
            | Element_module_comment text ->
                output_string chanout (self#man_of_module_comment text)
          )
          (Module.module_elements m);

        close_out chanout

      with
        Sys_error s ->
          raise (Failure s)

    (** Create the groups of elements to generate pages for. *)
    method create_groups module_list =
      let name res_ele = 
        match res_ele with
          Res_module m -> m.m_name
        | Res_module_type mt -> mt.mt_name
        | Res_class c -> c.cl_name
        | Res_class_type ct -> ct.clt_name
        | Res_value v -> Name.simple v.val_name
        | Res_type t -> Name.simple t.ty_name
        | Res_exception e -> Name.simple e.ex_name
        | Res_attribute a -> Name.simple a.att_value.val_name
        | Res_method m -> Name.simple m.met_value.val_name
        | Res_section _ -> assert false
      in
      let all_items_pre = Odoc_info.Search.search_by_name module_list (Str.regexp ".*")  in
      let all_items = List.filter 
          (fun r -> match r with Res_section _ -> false | _ -> true)
          all_items_pre
      in
      let sorted_items = List.sort (fun e1 -> fun e2 -> compare (name e1) (name e2)) all_items in
      let rec f acc1 acc2 l = 
        match l with
          [] -> acc2 :: acc1
        | h :: q ->
            match acc2 with
              [] -> f acc1 [h] q
            | h2 :: q2 -> 
                if (name h) = (name h2) then
                  if List.mem h acc2 then
                    f acc1 acc2 q
                  else
                    f acc1 (acc2 @ [h]) q 
                else
                  f (acc2 :: acc1) [h] q
      in
      f [] [] sorted_items

    (** Generate a man page for a group of elements with the same name. 
       A group must not be empty.*)
    method generate_for_group l =
     let name = 
       Name.simple 
         (
          match List.hd l with
            Res_module m -> m.m_name
          | Res_module_type mt -> mt.mt_name
          | Res_class c -> c.cl_name
          | Res_class_type ct -> ct.clt_name
          | Res_value v -> v.val_name
          | Res_type t -> t.ty_name
          | Res_exception e -> e.ex_name
          | Res_attribute a -> a.att_value.val_name
          | Res_method m -> m.met_value.val_name
          | Res_section (s,_) -> s
         )
     in
     let date = Unix.time () in
      let file = self#file_name name in
      try
        let chanout = self#open_out file in
        output_string chanout
          (
	   ".TH \""^name^"\" "^
           "man "^
           "\""^(Odoc_misc.string_of_date ~hour: false date)^"\" "^ 
           "OCamldoc "^
           "\""^(match !Args.title with Some t -> t | None -> "")^"\"\n"^
	   ".SH NAME\n"^
	   name^" \\- all "^name^" elements\n\n"
	  );

        let f ele =
          match ele with
            Res_value v ->
              output_string chanout
                ("\n.SH "^Odoc_messages.modul^" "^(Name.father v.val_name)^"\n"^
                 (self#man_of_value v))
          | Res_type t ->
              output_string chanout
                ("\n.SH "^Odoc_messages.modul^" "^(Name.father t.ty_name)^"\n"^
                 (self#man_of_type t))
          | Res_exception e ->
              output_string chanout
                ("\n.SH "^Odoc_messages.modul^" "^(Name.father e.ex_name)^"\n"^
                 (self#man_of_exception e))
          | Res_attribute a ->
              output_string chanout
                ("\n.SH "^Odoc_messages.clas^" "^(Name.father a.att_value.val_name)^"\n"^
                 (self#man_of_attribute a))
          | Res_method m ->
              output_string chanout
                ("\n.SH "^Odoc_messages.clas^" "^(Name.father m.met_value.val_name)^"\n"^
                 (self#man_of_method m))
          | Res_class c ->
              output_string chanout
                ("\n.SH "^Odoc_messages.modul^" "^(Name.father c.cl_name)^"\n"^
                 (self#man_of_class c))
          | Res_class_type ct ->
              output_string chanout
                ("\n.SH "^Odoc_messages.modul^" "^(Name.father ct.clt_name)^"\n"^
                 (self#man_of_class_type ct))
          | _ ->
              (* normalement on ne peut pas avoir de module ici. *)
              ()
        in
        List.iter f l;
        close_out chanout
      with
        Sys_error s ->
          incr Odoc_info.errors ;
          prerr_endline s

    (** Generate all the man pages from a module list. *)
    method generate module_list =
      let sorted_module_list = Sort.list (fun m1 -> fun m2 -> m1.m_name < m2.m_name) module_list in
      let groups = self#create_groups sorted_module_list in
      let f group = 
        match group with
          [] ->
            ()
        | [Res_module m] -> self#generate_for_module m
        | [Res_module_type mt] -> self#generate_for_module_type mt
        | [Res_class cl] -> self#generate_for_class cl
        | [Res_class_type ct] -> self#generate_for_class_type ct
        | l ->
            if !Args.man_mini then 
              ()
            else
              self#generate_for_group l
      in
      List.iter f groups 
  end
