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


(** Generation of html documentation. *)

let print_DEBUG s = print_string s ; print_newline ()

open Odoc_info 
open Parameter
open Value
open Type
open Exception
open Class 
open Module


(** The functions used for naming files and html marks.*)
module Naming =
  struct
    (** The prefix for types marks. *)
    let mark_type = "TYPE"

    (** The prefix for functions marks. *)
    let mark_function = "FUN"

    (** The prefix for exceptions marks. *)
    let mark_exception = "EXCEPTION"

    (** The prefix for values marks. *)
    let mark_value = "VAL"

    (** The prefix for attributes marks. *)
    let mark_attribute = "ATT"

    (** The prefix for methods marks. *)
    let mark_method = "METHOD"

    (** The prefix for code files.. *)
    let code_prefix = "code_"

    (** The prefix for type files.. *)
    let type_prefix = "type_"

    (** Return the two html files names for the given module or class name.*)
    let html_files name =
      let html_file = name^".html" in
      let html_frame_file = name^"-frame.html" in
      (html_file, html_frame_file)

    (** Return the target for the given prefix and simple name. *)
    let target pref simple_name = pref^simple_name

    (** Return the complete link target (file#target) for the given prefix string and complete name.*)
    let complete_target pref complete_name = 
      let simple_name = Name.simple complete_name in
      let module_name = 
        let s = Name.father complete_name in
        if s = "" then simple_name else s
      in
      let (html_file, _) = html_files module_name in
      html_file^"#"^(target pref simple_name)

    (** Return the link target for the given type. *)
    let type_target t = target mark_type (Name.simple t.ty_name)

    (** Return the complete link target for the given type. *)
    let complete_type_target t = complete_target mark_type t.ty_name

    (** Return the link target for the given exception. *)
    let exception_target e = target mark_exception (Name.simple e.ex_name)

    (** Return the complete link target for the given exception. *)
    let complete_exception_target e = complete_target mark_exception e.ex_name

	
    
    (** Return the link target for the given value. *)
    let value_target v = target mark_value (Name.simple v.val_name)

    (** Return the given value name where symbols accepted in infix values
       are replaced by strings, to avoid clashes with the filesystem.*)
    let subst_infix_symbols name =
      let len = String.length name in
      let buf = Buffer.create len in
      let ch c = Buffer.add_char buf c in
      let st s = Buffer.add_string buf s in
      for i = 0 to len - 1 do
	match name.[i] with
	| '|' -> st "_pipe_"
        | '<' -> st "_lt_"
        | '>' -> st "_gt_"
        | '@' -> st "_at_"
        | '^' -> st "_exp_"
        | '&' -> st "_amp_"
        | '+' -> st "_plus_"
        | '-' -> st "_minus_"
        | '*' -> st "_star_"
        | '/' -> st "_slash_"
        | '$' -> st "_dollar_"
        | '%' -> st "_percent_"
        | '=' -> st "_equal_"
        | ':' -> st "_column_"
        | '~' -> st "_tilde_"
        | '!' -> st "_bang_"
	| c -> ch c
      done;
      Buffer.contents buf

    (** Return the complete link target for the given value. *)
    let complete_value_target v = complete_target mark_value v.val_name

    (** Return the complete filename for the code of the given value. *)
    let file_code_value_complete_target v = 
      let f = code_prefix^mark_value^(subst_infix_symbols v.val_name)^".html" in
      f

    (** Return the link target for the given attribute. *)
    let attribute_target a = target mark_attribute (Name.simple a.att_value.val_name)

    (** Return the complete link target for the given attribute. *)
    let complete_attribute_target a = complete_target mark_attribute a.att_value.val_name

    (** Return the complete filename for the code of the given attribute. *)
    let file_code_attribute_complete_target a = 
      let f = code_prefix^mark_attribute^a.att_value.val_name^".html" in
      f

    (** Return the link target for the given method. *)
    let method_target m = target mark_method (Name.simple m.met_value.val_name)

    (** Return the complete link target for the given method. *)
    let complete_method_target m = complete_target mark_method m.met_value.val_name

    (** Return the complete filename for the code of the given method. *)
    let file_code_method_complete_target m = 
      let f = code_prefix^mark_method^m.met_value.val_name^".html" in
      f

    (** Return the link target for the given label section. *)
    let label_target l = target "" l

    (** Return the complete link target for the given section label. *)
    let complete_label_target l = complete_target "" l

    (** Return the complete filename for the code of the type of the 
       given module or module type name. *)
    let file_type_module_complete_target name = 
      let f = type_prefix^name^".html" in
      f

    (** Return the complete filename for the code of the 
       given module name. *)
    let file_code_module_complete_target name = 
      let f = code_prefix^name^".html" in
      f

    (** Return the complete filename for the code of the type of the 
       given class or class type name. *)
    let file_type_class_complete_target name = 
      let f = type_prefix^name^".html" in
      f
  end

(** A class with a method to colorize a string which represents OCaml code. *)
class ocaml_code =
  object(self)
    method html_of_code ?(with_pre=true) code =
      let html_code = Odoc_ocamlhtml.html_of_code ~with_pre: with_pre code in
      html_code
  end

(** Generation of html code from text structures. *)
class text =
  object (self)
    (** We want to display colorized code. *)
    inherit ocaml_code 

    (** Escape the strings which would clash with html syntax, and
       make some replacements (double newlines replaced by <br>). *)
    method escape s = Odoc_ocamlhtml.escape_base s


    method keep_alpha_num s =
      let len = String.length s in
      let buf = Buffer.create len in
      for i = 0 to len - 1 do
        match s.[i] with
          'a'..'z' | 'A'..'Z' | '0'..'9' -> Buffer.add_char buf s.[i]
        | _ -> ()
      done;
      Buffer.contents buf

    (** Return a label created from the first sentence of a text. *)
    method label_of_text t=
      let t2 = Odoc_info.first_sentence_of_text t in
      let s = Odoc_info.string_of_text t2 in
      let s2 = self#keep_alpha_num s in
      s2

    (** Create a label for the associated title. 
       Return the label specified by the user or a label created
       from the title level and the first sentence of the title. *)
    method create_title_label (n,label_opt,t) =
      match label_opt with
        Some s -> s
      | None -> Printf.sprintf "%d_%s" n (self#label_of_text t)

    (** Return the html code corresponding to the [text] parameter. *)
    method html_of_text t = String.concat "" (List.map self#html_of_text_element t)

    (** Return the html code for the [text_element] in parameter. *)
    method html_of_text_element te =
      print_DEBUG "text::html_of_text_element";
      match te with
      | Odoc_info.Raw s -> self#html_of_Raw s
      | Odoc_info.Code s -> self#html_of_Code s
      | Odoc_info.CodePre s -> self#html_of_CodePre s
      | Odoc_info.Verbatim s -> self#html_of_Verbatim s
      | Odoc_info.Bold t -> self#html_of_Bold t
      | Odoc_info.Italic t -> self#html_of_Italic t 
      | Odoc_info.Emphasize t -> self#html_of_Emphasize t
      | Odoc_info.Center t -> self#html_of_Center t
      | Odoc_info.Left t -> self#html_of_Left t
      | Odoc_info.Right t -> self#html_of_Right t
      | Odoc_info.List tl -> self#html_of_List tl
      | Odoc_info.Enum tl -> self#html_of_Enum tl
      | Odoc_info.Newline -> self#html_of_Newline
      | Odoc_info.Block t -> self#html_of_Block t
      | Odoc_info.Title (n, l_opt, t) -> self#html_of_Title n l_opt t
      | Odoc_info.Latex s -> self#html_of_Latex s
      | Odoc_info.Link (s, t) -> self#html_of_Link s t
      | Odoc_info.Ref (name, ref_opt) -> self#html_of_Ref name ref_opt
      | Odoc_info.Superscript t -> self#html_of_Superscript t
      | Odoc_info.Subscript t -> self#html_of_Subscript t

    method html_of_Raw s = self#escape s

    method html_of_Code s =
      if !Args.colorize_code then
        self#html_of_code ~with_pre: false s
      else
        "<code class=\""^Odoc_ocamlhtml.code_class^"\">"^(self#escape s)^"</code>"

    method html_of_CodePre s =
      if !Args.colorize_code then
        "<pre></pre>"^(self#html_of_code s)^"<pre></pre>"
      else
        "<pre><code class=\""^Odoc_ocamlhtml.code_class^"\">"^(self#escape s)^"</code></pre>"

    method html_of_Verbatim s = "<pre>"^(self#escape s)^"</pre>"
    method html_of_Bold t = "<b>"^(self#html_of_text t)^"</b>"
    method html_of_Italic t = "<i>"^(self#html_of_text t)^"</i>"
    method html_of_Emphasize t = "<em>"^(self#html_of_text t)^"</em>"
    method html_of_Center t = "<center>"^(self#html_of_text t)^"</center>"
    method html_of_Left t = "<div align=left>"^(self#html_of_text t)^"</div>"
    method html_of_Right t = "<div align=right>"^(self#html_of_text t)^"</div>"

    method html_of_List tl = 
      "<ul>\n"^
      (String.concat ""
         (List.map (fun t -> "<li>"^(self#html_of_text t)^"</li>\n") tl))^
      "</ul>\n"

    method html_of_Enum tl =
      "<OL>\n"^
      (String.concat ""
         (List.map (fun t -> "<li>"^(self#html_of_text t)^"</li>\n") tl))^
      "</OL>\n"

    method html_of_Newline = "\n<p>\n"

    method html_of_Block t =
      "<blockquote>\n"^(self#html_of_text t)^"</blockquote>\n"

    method html_of_Title n label_opt t =
      let css_class = "title"^(string_of_int n) in
      let label1 = self#create_title_label (n, label_opt, t) in
      "<br>\n"^
      "<a name=\""^(Naming.label_target label1)^"\"></a>\n"^
      "<table cellpadding=5 cellspacing=5 width=\"100%\">\n"^
      "<tr class=\""^css_class^"\"><td><div align=center>\n"^
      "<span class=\""^css_class^"\">"^(self#html_of_text t)^"</span>\n"^
      "</div>\n</td>\n</tr>\n</table>\n"

    method html_of_Latex _ = ""
      (* don't care about LaTeX stuff in HTML. *)

    method html_of_Link s t =
      "<a href=\""^s^"\">"^(self#html_of_text t)^"</a>"

    method html_of_Ref name ref_opt =
      match ref_opt with
        None -> 
          self#html_of_text_element (Odoc_info.Code name)
      | Some kind ->
	  let h name = Odoc_info.Code (Odoc_info.use_hidden_modules name) in
          let (target, text) = 
            match kind with
              Odoc_info.RK_module 
            | Odoc_info.RK_module_type
            | Odoc_info.RK_class
            | Odoc_info.RK_class_type ->
                let (html_file, _) = Naming.html_files name in
                (html_file, h name)
            | Odoc_info.RK_value -> (Naming.complete_target Naming.mark_value name, h name)
            | Odoc_info.RK_type -> (Naming.complete_target Naming.mark_type name, h name)
            | Odoc_info.RK_exception -> (Naming.complete_target Naming.mark_exception name, h name)
            | Odoc_info.RK_attribute -> (Naming.complete_target Naming.mark_attribute name, h name)
            | Odoc_info.RK_method -> (Naming.complete_target Naming.mark_method name, h name)
            | Odoc_info.RK_section t -> (Naming.complete_label_target name,
					 Odoc_info.Italic [Raw (Odoc_info.string_of_text t)])
          in
          "<a href=\""^target^"\">"^
          (self#html_of_text_element text)^
	  "</a>"

    method html_of_Superscript t =
      "<sup class=\"superscript\">"^(self#html_of_text t)^"</sup>"

    method html_of_Subscript t =
      "<sub class=\"subscript\">"^(self#html_of_text t)^"</sub>"

  end

(** A class used to generate html code for info structures. *)
class virtual info =
  object (self)
    (** The list of pairs [(tag, f)] where [f] is a function taking
       the [text] associated to [tag] and returning html code. 
       Add a pair here to handle a tag.*)
    val mutable tag_functions = ([] : (string * (Odoc_info.text -> string)) list)

    (** The method used to get html code from a [text]. *)
    method virtual html_of_text : Odoc_info.text -> string

    (** Return html for an author list. *)
    method html_of_author_list l =
      match l with
        [] ->
          ""
      | _ ->
          "<b>"^Odoc_messages.authors^": </b>"^
          (String.concat ", " l)^
          "<br>\n"

    (** Return html code for the given optional version information.*)
    method html_of_version_opt v_opt =
      match v_opt with
        None -> ""
      | Some v -> "<b>"^Odoc_messages.version^": </b>"^v^"<br>\n"

    (** Return html code for the given optional since information.*)
    method html_of_since_opt s_opt =
      match s_opt with
        None -> ""
      | Some s -> "<b>"^Odoc_messages.since^"</b> "^s^"<br>\n"

    (** Return html code for the given list of raised exceptions.*)
    method html_of_raised_exceptions l =
      match l with
        [] -> ""
      | (s, t) :: [] -> "<b>"^Odoc_messages.raises^"</b> <code>"^s^"</code> "^(self#html_of_text t)^"<br>\n"
      | _ ->
          "<b>"^Odoc_messages.raises^"</b><ul>"^
          (String.concat ""
             (List.map
                (fun (ex, desc) -> "<li><code>"^ex^"</code> "^(self#html_of_text desc)^"</li>\n")
                l
             )
          )^"</ul>\n"

    (** Return html code for the given "see also" reference. *)
    method html_of_see (see_ref, t)  =
      let t_ref = 
        match see_ref with
          Odoc_info.See_url s -> [ Odoc_info.Link (s, t) ]
        | Odoc_info.See_file s -> (Odoc_info.Code s) :: (Odoc_info.Raw " ") :: t
        | Odoc_info.See_doc s -> (Odoc_info.Italic [Odoc_info.Raw s]) :: (Odoc_info.Raw " ") :: t
      in
      self#html_of_text t_ref

    (** Return html code for the given list of "see also" references.*)
    method html_of_sees l =
      match l with
        [] -> ""
      | see :: [] -> "<b>"^Odoc_messages.see_also^"</b> "^(self#html_of_see see)^"<br>\n"
      | _ ->
          "<b>"^Odoc_messages.see_also^"</b><ul>"^
          (String.concat ""
             (List.map
                (fun see -> "<li>"^(self#html_of_see see)^"</li>\n")
                l
             )
          )^"</ul>\n"

    (** Return html code for the given optional return information.*)
    method html_of_return_opt return_opt =
      match return_opt with
        None -> ""
      | Some s -> "<b>"^Odoc_messages.returns^"</b> "^(self#html_of_text s)^"<br>\n"

    (** Return html code for the given list of custom tagged texts. *)
    method html_of_custom l =
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

    (** Return html code for a description, except for the [i_params] field. *)
    method html_of_info info_opt =
      match info_opt with
        None ->
          ""
      | Some info ->
          let module M = Odoc_info in
          "<div class=\"info\">\n"^
          (match info.M.i_deprecated with
            None -> ""
          | Some d -> 
              "<span class=\"warning\">"^Odoc_messages.deprecated^"</span> "^
              (self#html_of_text d)^
              "<br>\n"
          )^
          (match info.M.i_desc with
            None -> "" 
          | Some d when d = [Odoc_info.Raw ""] -> ""
          | Some d -> (self#html_of_text d)^"<br>\n"
          )^
          (self#html_of_author_list info.M.i_authors)^
          (self#html_of_version_opt info.M.i_version)^
          (self#html_of_since_opt info.M.i_since)^
          (self#html_of_raised_exceptions info.M.i_raised_exceptions)^
          (self#html_of_return_opt info.M.i_return_value)^
          (self#html_of_sees info.M.i_sees)^
          (self#html_of_custom info.M.i_custom)^
          "</div>\n"

    (** Return html code for the first sentence of a description. 
       The titles and lists in this first sentence has been removed.*)
    method html_of_info_first_sentence info_opt =
      match info_opt with
        None -> ""
      | Some info ->
          let module M = Odoc_info in
          let dep = info.M.i_deprecated <> None in
          "<div class=\"info\">\n"^
          (if dep then "<font color=\"#CCCCCC\">" else "") ^
          (match info.M.i_desc with
            None -> "" 
          | Some d when d = [Odoc_info.Raw ""] -> ""
          | Some d -> (self#html_of_text
                         (Odoc_info.text_no_title_no_list
                            (Odoc_info.first_sentence_of_text d)))^"\n"
          )^
          (if dep then "</font>" else "") ^
          "</div>\n"

  end



let opt = Odoc_info.apply_opt

(** This class is used to create objects which can generate a simple html documentation. *)
class html =
  object (self)
    inherit text
    inherit info

    (** The default style options. *)
    val mutable default_style_options =
      ["a:visited {color : #416DFF; text-decoration : none; }" ;
        "a:link {color : #416DFF; text-decoration : none;}" ;
        "a:hover {color : Red; text-decoration : none; background-color: #5FFF88}" ;
        "a:active {color : Red; text-decoration : underline; }" ;
        ".keyword { font-weight : bold ; color : Red }" ;
        ".keywordsign { color : #C04600 }" ; 
        ".superscript { font-size : 4 }" ;
        ".subscript { font-size : 4 }" ;
        ".comment { color : Green }" ;
        ".constructor { color : Blue }" ;
        ".type { color : #5C6585 }" ;
        ".string { color : Maroon }" ;
        ".warning { color : Red ; font-weight : bold }" ;
        ".info { margin-left : 3em; margin-right : 3em }" ;
        ".code { color : #465F91 ; }" ;
        ".title1 { font-size : 20pt ; background-color : #909DFF }" ;
        ".title2 { font-size : 20pt ; background-color : #90BDFF }" ;
        ".title3 { font-size : 20pt ; background-color : #90DDFF }" ;
        ".title4 { font-size : 20pt ; background-color : #90EDFF }" ;
        ".title5 { font-size : 20pt ; background-color : #90FDFF }" ;
        ".title6 { font-size : 20pt ; background-color : #C0FFFF }" ;
	".typetable { border-style : hidden }" ;
	".indextable { border-style : hidden }" ;
	".paramstable { border-style : hidden ; padding: 5pt 5pt}" ;
        "body { background-color : White }" ;
        "tr { background-color : White }" ;
	"td.typefieldcomment { background-color : #FFFFFF }" ;
	"pre { margin-bottom: 4px }" ;
      ] 
      
    (** The style file for all pages. *)
    val mutable style_file = "style.css"

    (** The code to import the style. Initialized in [init_style]. *)
    val mutable style = ""

    (** The known types names. 
       Used to know if we must create a link to a type
       when printing a type. *)
    val mutable known_types_names = []

    (** The known class and class type names. 
       Used to know if we must create a link to a class 
       or class type or not when printing a type. *)
    val mutable known_classes_names = []

    (** The known modules and module types names. 
       Used to know if we must create a link to a type or not
       when printing a module type. *)
    val mutable known_modules_names = []

    (** The main file. *)
    val mutable index = "index.html"
    (** The file for the index of values. *)
    val mutable index_values = "index_values.html"
    (** The file for the index of types. *)
    val mutable index_types = "index_types.html"
    (** The file for the index of exceptions. *)
    val mutable index_exceptions = "index_exceptions.html"
    (** The file for the index of attributes. *)
    val mutable index_attributes = "index_attributes.html"
    (** The file for the index of methods. *)
    val mutable index_methods = "index_methods.html"
    (** The file for the index of classes. *)
    val mutable index_classes = "index_classes.html"
    (** The file for the index of class types. *)
    val mutable index_class_types = "index_class_types.html"
    (** The file for the index of modules. *)
    val mutable index_modules = "index_modules.html"
    (** The file for the index of module types. *)
    val mutable index_module_types = "index_module_types.html"


    (** The list of attributes. Filled in the [generate] method. *)
    val mutable list_attributes = []
    (** The list of methods. Filled in the [generate] method. *)
    val mutable list_methods = []
    (** The list of values. Filled in the [generate] method. *)
    val mutable list_values = []
    (** The list of exceptions. Filled in the [generate] method. *)
    val mutable list_exceptions = []
    (** The list of types. Filled in the [generate] method. *)
    val mutable list_types = []
    (** The list of modules. Filled in the [generate] method. *)
    val mutable list_modules = []
    (** The list of module types. Filled in the [generate] method. *)
    val mutable list_module_types = []
    (** The list of classes. Filled in the [generate] method. *)
    val mutable list_classes = []
    (** The list of class types. Filled in the [generate] method. *)
    val mutable list_class_types = []

    (** The header of pages. Must be prepared by the [prepare_header] method.*)
    val mutable header = fun ?(nav=None) -> fun ?(comments=[]) -> fun _ -> ""

    (** Init the style. *)
    method init_style =
      (match !Args.css_style with
        None -> 
          let default_style = String.concat "\n" default_style_options in
          (
           try
	     let file = Filename.concat !Args.target_dir style_file in
	     if Sys.file_exists file then
	       Odoc_info.verbose (Odoc_messages.file_exists_dont_generate file)
	     else
	       (
		let chanout = open_out file in
		output_string chanout default_style ;
		flush chanout ;
		close_out chanout;
		Odoc_info.verbose (Odoc_messages.file_generated file)
	       )
           with
             Sys_error s ->
               prerr_endline s ;
               incr Odoc_info.errors ;
          )
      | Some f ->
          style_file <- f 
      );
      style <- "<link rel=\"stylesheet\" href=\""^style_file^"\" type=\"text/css\">\n"

    (** Get the title given by the user *)
    method title = match !Args.title with None -> "" | Some t -> self#escape t

    (** Get the title given by the user completed with the given subtitle. *)
    method inner_title s = 
      (match self#title with "" -> "" | t -> t^" : ")^
      (self#escape s)

    (** Get the page header. *)
    method header ?nav ?comments title = header ?nav ?comments title

    (** A function to build the header of pages. *)
    method prepare_header module_list =
      let f ?(nav=None) ?(comments=[]) t  = 
        let link_if_not_empty l m url =
          match l with
            [] -> ""
          | _ -> "<link title=\""^m^"\" rel=Appendix href=\""^url^"\">\n"
        in
        "<head>\n"^
        style^
        "<link rel=\"Start\" href=\""^index^"\">\n"^
        (
         match nav with
           None -> ""
         | Some (pre_opt, post_opt, name) ->
             (match pre_opt with
               None -> ""
             | Some name -> 
                 "<link rel=\"previous\" href=\""^(fst (Naming.html_files name))^"\">\n"
             )^
             (match post_opt with
               None -> ""
             | Some name -> 
                 "<link rel=\"next\" href=\""^(fst (Naming.html_files name))^"\">\n"
             )^
             (
              let father = Name.father name in
              let href = if father = "" then index else fst (Naming.html_files father) in
              "<link rel=\"Up\" href=\""^href^"\">\n"
             )
        )^
        (link_if_not_empty list_types Odoc_messages.index_of_types index_types)^
        (link_if_not_empty list_exceptions Odoc_messages.index_of_exceptions index_exceptions)^
        (link_if_not_empty list_values Odoc_messages.index_of_values index_values)^
        (link_if_not_empty list_attributes Odoc_messages.index_of_attributes index_attributes)^
        (link_if_not_empty list_methods Odoc_messages.index_of_methods index_methods)^
        (link_if_not_empty list_classes Odoc_messages.index_of_classes index_classes)^
        (link_if_not_empty list_class_types Odoc_messages.index_of_class_types index_class_types)^
        (link_if_not_empty list_modules Odoc_messages.index_of_modules index_modules)^
        (link_if_not_empty list_module_types Odoc_messages.index_of_module_types index_module_types)^
        (String.concat "\n"
           (List.map
              (fun m -> 
                let html_file = fst (Naming.html_files m.m_name) in
                "<link title=\""^m.m_name^"\" rel=\"Chapter\" href=\""^html_file^"\">"
              )
              module_list
           )
        )^
        (self#html_sections_links comments)^
        "<title>"^
        t^
        "</title>\n</head>\n"
      in
      header <- f

    (** Build the html code for the link tags in the header, defining section and
       subsections for the titles found in the given comments.*)
    method html_sections_links comments =
      let titles = List.flatten (List.map Odoc_info.get_titles_in_text comments) in
      let levels = 
        let rec iter acc l =
          match l with
            [] -> acc
          | (n,_,_) :: q ->
              if List.mem n acc 
              then iter acc q
              else iter (n::acc) q
        in
        iter [] titles
      in
      let sorted_levels = List.sort compare levels in
      let (section_level, subsection_level) =
        match sorted_levels with
          [] -> (None, None)
        | [n] -> (Some n, None)
        | n :: m :: _ -> (Some n, Some m)
      in
      let titles_per_level level_opt =
        match level_opt with
          None -> []
        | Some n -> List.filter (fun (m,_,_) -> m = n) titles
      in
      let section_titles = titles_per_level section_level in
      let subsection_titles = titles_per_level subsection_level in
      let create_lines s_rel titles =
        List.map
          (fun (n,lopt,t) -> 
            let s = Odoc_info.string_of_text t in
            let label = self#create_title_label (n,lopt,t) in
            Printf.sprintf "<link title=\"%s\" rel=\"%s\" href=\"#%s\">\n" s s_rel label)
          titles
      in
      let section_lines = create_lines "Section" section_titles in
      let subsection_lines = create_lines "Subsection" subsection_titles in
      String.concat "" (section_lines @ subsection_lines)

    (** Html code for navigation bar. 
       @param pre optional name for optional previous module/class 
       @param post optional name for optional next module/class
       @param name name of current module/class *)
    method navbar pre post name =
      "<div class=\"navbar\">"^
      (match pre with
        None -> ""
      | Some name -> 
          "<a href=\""^(fst (Naming.html_files name))^"\">"^Odoc_messages.previous^"</a>\n"
      )^
      "&nbsp;"^
      (
       let father = Name.father name in
       let href = if father = "" then index else fst (Naming.html_files father) in
       "<a href=\""^href^"\">"^Odoc_messages.up^"</a>\n"
      )^
      "&nbsp;"^
      (match post with
        None -> ""
      | Some name -> 
          "<a href=\""^(fst (Naming.html_files name))^"\">"^Odoc_messages.next^"</a>\n"
      )^
      "</div>\n"

    (** Return html code with the given string in the keyword style.*)
    method keyword s = 
      "<span class=\"keyword\">"^s^"</span>"

    (** Return html code with the given string in the constructor style. *)
    method constructor s = "<span class=\"constructor\">"^s^"</span>"

    (** Output the given ocaml code to the given file name. *)
    method private output_code in_title file code =
      try
        let chanout = open_out file in
        let html_code = self#html_of_code code in
        output_string chanout ("<html>"^(self#header (self#inner_title in_title))^"<body>\n");
        output_string chanout html_code;
        output_string chanout "</body></html>";
        close_out chanout
      with
        Sys_error s -> 
          incr Odoc_info.errors ;
          prerr_endline s

    (** Take a string and return the string where fully qualified 
       type (or class or class type) idents 
       have been replaced by links to the type referenced by the ident.*)
    method create_fully_qualified_idents_links m_name s =
      let f str_t = 
        let match_s = Str.matched_string str_t in
        let rel = Name.get_relative m_name match_s in
        let s_final = Odoc_info.apply_if_equal 
            Odoc_info.use_hidden_modules 
            match_s
            rel
        in
        if List.mem match_s known_types_names then
           "<a href=\""^(Naming.complete_target Naming.mark_type match_s)^"\">"^
           s_final^
           "</a>"
        else
          if List.mem match_s known_classes_names then
            let (html_file, _) = Naming.html_files match_s in
            "<a href=\""^html_file^"\">"^s_final^"</a>"
          else
            s_final
      in
      let s2 = Str.global_substitute
          (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)")
          f
          s
      in
      s2

    (** Take a string and return the string where fully qualified module idents 
       have been replaced by links to the module referenced by the ident.*)
    method create_fully_qualified_module_idents_links m_name s =
      let f str_t = 
        let match_s = Str.matched_string str_t in
        if List.mem match_s known_modules_names then
          let (html_file, _) = Naming.html_files match_s in
          "<a href=\""^html_file^"\">"^(Name.get_relative m_name match_s)^"</a>"
        else
          match_s
      in
      let s2 = Str.global_substitute
          (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([A-Z][a-zA-Z_'0-9]*\\)")
          f
          s
      in
      s2

    (** Return html code to display a [Types.type_expr]. *)
    method html_of_type_expr m_name t =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_info.string_of_type_expr t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "<br>       " s in
      Printf.sprintf 
	"<code class=\"type\">%s</code>"
	(self#create_fully_qualified_idents_links m_name s2)

    (** Return html code to display a [Types.class_type].*)
    method html_of_class_type_expr m_name t =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_info.string_of_class_type t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "<br>       " s in
      "<code class=\"type\">"^(self#create_fully_qualified_idents_links m_name s2)^"</code>"

    (** Return html code to display a [Types.type_expr list]. *)
    method html_of_type_expr_list m_name sep l =
      print_DEBUG "html#html_of_type_expr_list";
      let s = Odoc_info.string_of_type_list sep l in
      print_DEBUG "html#html_of_type_expr_list: 1";
      let s2 = Str.global_replace (Str.regexp "\n") "<br>       " s in
      print_DEBUG "html#html_of_type_expr_list: 2";
      "<code class=\"type\">"^(self#create_fully_qualified_idents_links m_name s2)^"</code>"

    (** Return html code to display a [Types.type_expr list] as type parameters
       of a class of class type. *)
    method html_of_class_type_param_expr_list m_name l =
      let s = Odoc_info.string_of_class_type_param_list l in
      let s2 = Str.global_replace (Str.regexp "\n") "<br>       " s in
      "<code class=\"type\">"^(self#create_fully_qualified_idents_links m_name s2)^"</code>"

    (** Return html code to display a list of type parameters for the given type.*)
    method html_of_type_expr_param_list m_name t =
      let s = Odoc_info.string_of_type_param_list t in
      let s2 = Str.global_replace (Str.regexp "\n") "<br>       " s in
      "<code class=\"type\">"^(self#create_fully_qualified_idents_links m_name s2)^"</code>"

    (** Return html code to display a [Types.module_type]. *)
    method html_of_module_type m_name t =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_info.string_of_module_type t))
      in
      let s2 = Str.global_replace (Str.regexp "\n") "<br>       " s in
      "<code class=\"type\">"^(self#create_fully_qualified_module_idents_links m_name s2)^"</code>"
        
    (** Generate a file containing the module type in the given file name. *)
    method output_module_type in_title file mtyp =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_info.string_of_module_type ~complete: true mtyp))
      in
      self#output_code in_title file s

    (** Generate a file containing the class type in the given file name. *)
    method output_class_type in_title file ctyp =
      let s = String.concat "\n"
          (Str.split (Str.regexp "\n") (Odoc_info.string_of_class_type ~complete: true ctyp))
      in
      self#output_code in_title file s


    (** Return html code for a value. *)
    method html_of_value v =
      Odoc_info.reset_type_names ();
      "<pre>"^(self#keyword "val")^" "^
      (* html mark *)
      "<a name=\""^(Naming.value_target v)^"\"></a>"^
      (match v.val_code with 
        None -> Name.simple v.val_name
      | Some c -> 
          let file = Naming.file_code_value_complete_target v in
          self#output_code v.val_name (Filename.concat !Args.target_dir file) c;
          "<a href=\""^file^"\">"^(Name.simple v.val_name)^"</a>"
      )^" : "^
      (self#html_of_type_expr (Name.father v.val_name) v.val_type)^"</pre>"^
      (self#html_of_info v.val_info)^
      (if !Args.with_parameter_list then
        self#html_of_parameter_list (Name.father v.val_name) v.val_parameters
      else
        self#html_of_described_parameter_list (Name.father v.val_name) v.val_parameters
      )

    (** Return html code for an exception. *)
    method html_of_exception e =
      Odoc_info.reset_type_names ();
      "<pre>"^(self#keyword "exception")^" "^
      (* html mark *)
      "<a name=\""^(Naming.exception_target e)^"\"></a>"^
      (Name.simple e.ex_name)^
      (match e.ex_args with
        [] -> ""
      | _ -> 
          " "^(self#keyword "of")^" "^
          (self#html_of_type_expr_list (Name.father e.ex_name) " * " e.ex_args)
      )^
      (match e.ex_alias with
        None -> ""
      | Some ea -> " = "^
          (
           match ea.ea_ex with
             None -> ea.ea_name
           | Some e -> "<a href=\""^(Naming.complete_exception_target e)^"\">"^e.ex_name^"</a>"
          )
      )^
      "</pre>\n"^
      (self#html_of_info e.ex_info)

    (** Return html code for a type. *)
    method html_of_type t =
      Odoc_info.reset_type_names ();
      let father = Name.father t.ty_name in
      (match t.ty_manifest, t.ty_kind with 
	None, Type_abstract -> "<pre>" 
      |	None, Type_variant _ 
      |	None, Type_record _ -> "<br><code>" 
      | Some _, Type_abstract -> "<pre>"
      | Some _, Type_variant _
      |	Some _, Type_record _ -> "<pre>"
      )^
      (self#keyword "type")^" "^
      (* html mark *)
      "<a name=\""^(Naming.type_target t)^"\"></a>"^
      (self#html_of_type_expr_param_list father t)^
      (match t.ty_parameters with [] -> "" | _ -> " ")^
      (Name.simple t.ty_name)^" "^
      (match t.ty_manifest with None -> "" | Some typ -> "= "^(self#html_of_type_expr father typ)^" ")^
      (match t.ty_kind with
        Type_abstract -> "</pre>"
      | Type_variant (l, priv) ->
          "= "^(if priv then "private" else "")^
	  (match t.ty_manifest with None -> "</code>" | Some _ -> "</pre>")^
          "<table class=\"typetable\">\n"^ 
          (String.concat "\n"
             (List.map 
                (fun constr ->
                  "<tr>\n"^
                  "<td align=\"left\" valign=\"top\" >\n"^
                  "<code>"^
                  (self#keyword "|")^
                  "</code></td>\n"^
                  "<td align=\"left\" valign=\"top\" >\n"^
                  "<code>"^
                  (self#constructor constr.vc_name)^
                  (match constr.vc_args with
                    [] -> "" 
                  | l -> 
                      " "^(self#keyword "of")^" "^
                      (self#html_of_type_expr_list father " * " l)
                  )^
                  "</code></td>\n"^
                  (match constr.vc_text with
                    None -> ""
                  | Some t ->
                      "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >"^
                      "<code>"^
                      "(*"^
                      "</code></td>"^
                      "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >"^
                      "<code>"^
                      (self#html_of_text t)^
                      "</code></td>"^
                      "<td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >"^
                      "<code>"^
                      "*)"^
                      "</code></td>"
                  )^
                  "\n</tr>"
                )
                l
             )
          )^
          "</table>\n"

      | Type_record (l, priv) ->
          "= "^(if priv then "private " else "")^"{"^
	  (match t.ty_manifest with None -> "</code>" | Some _ -> "</pre>")^
          "<table class=\"typetable\">\n"^ 
          (String.concat "\n"
             (List.map 
                (fun r ->
                  "<tr>\n"^
                  "<td align=\"left\" valign=\"top\" >\n"^
                  "<code>&nbsp;&nbsp;</code>"^
                  "</td>\n"^
                  "<td align=\"left\" valign=\"top\" >\n"^
                  "<code>"^(if r.rf_mutable then self#keyword "mutable&nbsp;" else "")^
                  r.rf_name^"&nbsp;: "^(self#html_of_type_expr father r.rf_type)^";"^
                  "</code></td>\n"^
                  (match r.rf_text with
                    None -> ""
                  | Some t ->
                      "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >"^
                      "<code>"^
                      "(*"^
                      "</code></td>"^
                      "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >"^
                      "<code>"^
                      (self#html_of_text t)^
                      "</code></td>"^
                      "<td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >"^
                      "<code>"^
                      "*)"^
                      "</code></td>"
                  )^
                  "\n</tr>"
                )
                l
             )
          )^
          "</table>\n"^
          "}\n"
      )^"\n"^
      (self#html_of_info t.ty_info)^
      "\n"

    (** Return html code for a class attribute. *)
    method html_of_attribute a =
      let module_name = Name.father (Name.father a.att_value.val_name) in
      "<pre>"^(self#keyword "val")^" "^
      (* html mark *)
      "<a name=\""^(Naming.attribute_target a)^"\"></a>"^
      (if a.att_mutable then (self#keyword Odoc_messages.mutab)^" " else "")^
      (match a.att_value.val_code with 
        None -> Name.simple a.att_value.val_name
      | Some c -> 
          let file = Naming.file_code_attribute_complete_target a in
          self#output_code a.att_value.val_name (Filename.concat !Args.target_dir file) c;
          "<a href=\""^file^"\">"^(Name.simple a.att_value.val_name)^"</a>"
      )^" : "^
      (self#html_of_type_expr module_name  a.att_value.val_type)^"</pre>"^
      (self#html_of_info a.att_value.val_info)

    (** Return html code for a class method. *)
    method html_of_method m =
      let module_name = Name.father (Name.father m.met_value.val_name) in
      "<pre>"^(self#keyword "method")^" "^
      (* html mark *)
      "<a name=\""^(Naming.method_target m)^"\"></a>"^
      (if m.met_private then (self#keyword "private")^" " else "")^
      (if m.met_virtual then (self#keyword "virtual")^" " else "")^
      (match m.met_value.val_code with 
        None -> Name.simple m.met_value.val_name
      | Some c -> 
          let file = Naming.file_code_method_complete_target m in
          self#output_code m.met_value.val_name (Filename.concat !Args.target_dir file) c;
          "<a href=\""^file^"\">"^(Name.simple m.met_value.val_name)^"</a>"
      )^" : "^
      (self#html_of_type_expr module_name m.met_value.val_type)^"</pre>"^
      (self#html_of_info m.met_value.val_info)^
      (if !Args.with_parameter_list then
        self#html_of_parameter_list module_name m.met_value.val_parameters
      else
        self#html_of_described_parameter_list module_name m.met_value.val_parameters
      )

    (** Return html code for the description of a function parameter. *)
    method html_of_parameter_description p =
      match Parameter.names p with
        [] ->
          ""
      | name :: [] ->
          (
           (* Only one name, no need for label for the description. *)
           match Parameter.desc_by_name p name with
             None -> ""
           | Some t -> self#html_of_text t
          )
      | l ->
          (*  A list of names, we display those with a description. *)
          let l2 = List.filter (fun n -> (Parameter.desc_by_name p n) <> None) l in
          String.concat "<br>\n"
            (List.map
               (fun n ->
                 match Parameter.desc_by_name p n with
                   None -> ""
                 | Some t -> "<code>"^n^"</code> : "^(self#html_of_text t)
               )
               l2
            )

    (** Return html code for a list of parameters. *)
    method html_of_parameter_list m_name l =
      match l with
        [] ->
          ""
      | _ ->
          "<div class=\"info\">"^
          "<table border=\"0\" cellpadding=\"3\" width=\"100%\">\n"^
          "<tr>\n"^
          "<td align=\"left\" valign=\"top\" width=\"1%\"><b>"^Odoc_messages.parameters^": </b></td>\n"^
          "<td>\n"^
          "<table class=\"paramstable\">\n"^ 
             (*border=\"0\" cellpadding=\"5\" cellspacing=\"0\">\n"^*)
          (String.concat ""
             (List.map
                (fun p ->
                  "<tr>\n"^
                  "<td align=\"center\" valign=\"top\" width=\"15%\" class=\"code\">\n"^
                  (match Parameter.complete_name p with
                    "" -> "?"
                  | s -> s
                  )^"</td>\n"^
                  "<td align=\"center\" valign=\"top\">:</td>\n"^
                  "<td>"^(self#html_of_type_expr m_name (Parameter.typ p))^"<br>\n"^
                  (self#html_of_parameter_description p)^"\n"^
                  "</tr>\n"
                )
                l
             )
          )^"</table>\n"^
          "</td>\n"^
          "</tr>\n"^
          "</table></div>\n"

    (** Return html code for the parameters which have a name and description. *)
    method html_of_described_parameter_list m_name l =
      (* get the params which have a name, and at least one name described. *)
      let l2 = List.filter 
          (fun p -> 
            List.exists 
              (fun n -> (Parameter.desc_by_name p n) <> None)
              (Parameter.names p))
          l
      in
      let f p =
        "<div class=\"info\"><code class=\"code\">"^(Parameter.complete_name p)^"</code> : "^
        (self#html_of_parameter_description p)^"</div>\n"
      in
      match l2 with
        [] -> ""
      | _ -> "<br>"^(String.concat "" (List.map f l2))

    (** Return html code for a list of module parameters. *)
    method html_of_module_parameter_list m_name l =
      match l with
        [] ->
          ""
      | _ ->
          "<table border=\"0\" cellpadding=\"3\" width=\"100%\">\n"^
          "<tr>\n"^
          "<td align=\"left\" valign=\"top\" width=\"1%\"><b>"^Odoc_messages.parameters^": </b></td>\n"^
          "<td>\n"^
          "<table class=\"paramstable\">\n"^
             (*border=\"0\" cellpadding=\"5\" cellspacing=\"0\">\n"^*)
          (String.concat ""
             (List.map
                (fun (p, desc_opt) ->
                  "<tr>\n"^
                  "<td align=\"center\" valign=\"top\" width=\"15%\">\n"^
                  "<code>"^p.mp_name^"</code></td>\n"^
                  "<td align=\"center\" valign=\"top\">:</td>\n"^
                  "<td>"^(self#html_of_module_type m_name p.mp_type)^"\n"^
                  (match desc_opt with
                    None -> ""
                  | Some t -> "<br>"^(self#html_of_text t))^
                  "\n"^
                  "</tr>\n"
                )
                l
             )
          )^"</table>\n"^
          "</td>\n"^
          "</tr>\n"^
          "</table>\n"

    (** Return html code for a module. *)
    method html_of_module ?(info=true) ?(complete=true) ?(with_link=true) m =
      let (html_file, _) = Naming.html_files m.m_name in
      let father = Name.father m.m_name in
      let buf = Buffer.create 32 in
      let p = Printf.bprintf in
      p buf "<pre>%s " (self#keyword "module");
      (
       if with_link then
         p buf "<a href=\"%s\">%s</a>" html_file (Name.simple m.m_name)
       else
         p buf "%s" (Name.simple m.m_name)
      );
      p buf ": %s</pre>" (self#html_of_module_type father m.m_type);
      if info then
        p buf "%s" ((if complete then self#html_of_info else self#html_of_info_first_sentence) m.m_info)
      else
        ();
      Buffer.contents buf

    (** Return html code for a module type. *)
    method html_of_modtype ?(info=true) ?(complete=true) ?(with_link=true) mt =
      let (html_file, _) = Naming.html_files mt.mt_name in
      let father = Name.father mt.mt_name in
      let buf = Buffer.create 32 in
      let p = Printf.bprintf in
      p buf "<pre>%s " (self#keyword "module type");
      (
       if with_link then
         p buf "<a href=\"%s\">%s</a>" html_file (Name.simple mt.mt_name)
         else
         p buf "%s" (Name.simple mt.mt_name)
      );
      (match mt.mt_type with
        None -> ()
      | Some mtyp -> p buf " = %s" (self#html_of_module_type father mtyp)
      );
      Buffer.add_string buf "</pre>";
      if info then
        p buf "%s" ((if complete then self#html_of_info else self#html_of_info_first_sentence) mt.mt_info)
      else
        ();
      Buffer.contents buf

    (** Return html code for an included module. *)
    method html_of_included_module im =
      "<pre>"^(self#keyword "include")^" "^
      (
       match im.im_module with
         None ->
           im.im_name
       | Some mmt ->
           let (file, name) = 
             match mmt with
               Mod m -> 
                 let (html_file, _) = Naming.html_files m.m_name in
                 (html_file, m.m_name)
             | Modtype mt ->
                 let (html_file, _) = Naming.html_files mt.mt_name in
                 (html_file, mt.mt_name)
           in
           "<a href=\""^file^"\">"^(Name.simple name)^"</a>"
      )^
      "</pre>\n"^
      (self#html_of_info im.im_info)

    (** Return html code for a class. *)
    method html_of_class ?(complete=true) ?(with_link=true) c =
      let father = Name.father c.cl_name in
      Odoc_info.reset_type_names ();
      let buf = Buffer.create 32 in
      let (html_file, _) = Naming.html_files c.cl_name in
      let p = Printf.bprintf in
      p buf "<pre>%s " (self#keyword "class");
      (* we add a html tag, the same as for a type so we can 
         go directly here when the class name is used as a type name *)
      p buf "<a name=\"%s\"></a>"
        (Naming.type_target 
           { ty_name = c.cl_name ;
             ty_info = None ; ty_parameters = [] ;
             ty_kind = Type_abstract ; ty_manifest = None ; 
             ty_loc = Odoc_info.dummy_loc ; 
	     ty_code = None ;
	   }
	);
      print_DEBUG "html#html_of_class : virtual or not" ;
      if c.cl_virtual then p buf "%s " (self#keyword "virtual") else ();
      (
       match c.cl_type_parameters with
         [] -> ()
       | l -> 
           p buf "%s "
             (self#html_of_class_type_param_expr_list father l)
      );
      print_DEBUG "html#html_of_class : with link or not" ;
      (
       if with_link then
         p buf "<a href=\"%s\">%s</a>" html_file (Name.simple c.cl_name)
       else
         p buf "%s" (Name.simple c.cl_name)
      );

      Buffer.add_string buf " : " ;
      Buffer.add_string buf (self#html_of_class_type_expr father c.cl_type);
      Buffer.add_string buf "</pre>" ;
      print_DEBUG "html#html_of_class : info" ;
      Buffer.add_string buf 
        ((if complete then self#html_of_info else self#html_of_info_first_sentence) c.cl_info);
      Buffer.contents buf

    (** Return html code for a class type. *)
    method html_of_class_type ?(complete=true) ?(with_link=true) ct =
      Odoc_info.reset_type_names ();
      let father = Name.father ct.clt_name in
      let buf = Buffer.create 32 in
      let p = Printf.bprintf in
      let (html_file, _) = Naming.html_files ct.clt_name in
      p buf "<pre>%s " (self#keyword "class type");
      (* we add a html tag, the same as for a type so we can 
         go directly here when the class type name is used as a type name *)
      p buf "<a name=\"%s\"></a>"
        (Naming.type_target 
           { ty_name = ct.clt_name ;
             ty_info = None ; ty_parameters = [] ;
             ty_kind = Type_abstract ; ty_manifest = None ;
             ty_loc = Odoc_info.dummy_loc ; 
	     ty_code = None ;
	   }
	);
      if ct.clt_virtual then p buf "%s "(self#keyword "virtual") else ();
      (
       match ct.clt_type_parameters with
        [] -> ()
      | l -> p buf "%s " (self#html_of_class_type_param_expr_list father l)
      );

      if with_link then
        p buf "<a href=\"%s\">%s</a>" html_file (Name.simple ct.clt_name)
      else
        p buf "%s" (Name.simple ct.clt_name);

      Buffer.add_string buf " = ";
      Buffer.add_string buf (self#html_of_class_type_expr father ct.clt_type);
      Buffer.add_string buf "</pre>";
      Buffer.add_string buf ((if complete then self#html_of_info else self#html_of_info_first_sentence) ct.clt_info);

      Buffer.contents buf

    (** Return html code to represent a dag, represented as in Odoc_dag2html. *)
    method html_of_dag dag =
      let f n =
        let (name, cct_opt) = n.Odoc_dag2html.valu in
        (* if we have a c_opt = Some class then we take its information
           because we are sure the name is complete. *)
        let (name2, html_file) =
          match cct_opt with
            None -> (name, fst (Naming.html_files name))
          | Some (Cl c) -> (c.cl_name, fst (Naming.html_files c.cl_name))
          | Some (Cltype (ct, _)) -> (ct.clt_name, fst (Naming.html_files ct.clt_name))
        in
        let new_v =
          "<table border=1>\n<tr><td>"^
          "<a href=\""^html_file^"\">"^name2^"</a>"^
          "</td></tr>\n</table>\n"
        in      
        { n with Odoc_dag2html.valu = new_v }
      in
      let a = Array.map f dag.Odoc_dag2html.dag in
      Odoc_dag2html.html_of_dag { Odoc_dag2html.dag = a }

    (** Return html code for a module comment.*)
    method html_of_module_comment text =
      "<br>\n"^(self#html_of_text text)^"<br><br>\n"

    (** Return html code for a class comment.*)
    method html_of_class_comment text =
      (* Add some style if there is no style for the first part of the text. *)
      let text2 =
        match text with
        | (Odoc_info.Raw s) :: q -> 
            (Odoc_info.Title (2, None, [Odoc_info.Raw s])) :: q
        | _ -> text
      in
      self#html_of_text text2

    (** Generate html code for the given list of inherited classes.*)
    method generate_inheritance_info chanout inher_l =
      let f inh =
        match inh.ic_class with
          None -> (* we can't make the link. *)
            (Odoc_info.Code inh.ic_name) ::
            (match inh.ic_text with
              None -> []
            | Some t -> (Odoc_info.Raw "    ") :: t)
        | Some cct ->
            (* we can create the link. *)
            let real_name = (* even if it should be the same *)
              match cct with
                Cl c -> c.cl_name
              | Cltype (ct, _) -> ct.clt_name
            in
            let (class_file, _) = Naming.html_files real_name in
            (Odoc_info.Link (class_file, [Odoc_info.Code real_name])) ::
            (match inh.ic_text with
              None -> []
            | Some t -> (Odoc_info.Raw "    ") :: t)
      in
      let text = [
        Odoc_info.Bold [Odoc_info.Raw Odoc_messages.inherits] ;
        Odoc_info.List (List.map f inher_l)
      ] 
      in
      let html = self#html_of_text text in
      output_string chanout html

    (** Generate html code for the inherited classes of the given class. *)
    method generate_class_inheritance_info chanout cl =
      let rec iter_kind k = 
        match k with
          Class_structure ([], _) ->
            ()
        | Class_structure (l, _) ->
            self#generate_inheritance_info chanout l
        | Class_constraint (k, ct) ->
            iter_kind k
        | Class_apply _
        | Class_constr _ ->
            ()
      in
      iter_kind cl.cl_kind

    (** Generate html code for the inherited classes of the given class type. *)
    method generate_class_type_inheritance_info chanout clt =
      match clt.clt_kind with
        Class_signature ([], _) ->
          ()
      | Class_signature (l, _) ->
          self#generate_inheritance_info chanout l
      | Class_type _ ->
          ()

    (** A method to create index files. *)
    method generate_elements_index :
        'a.
        'a list ->
          ('a -> Odoc_info.Name.t) ->
            ('a -> Odoc_info.info option) -> 
              ('a -> string) -> string -> string -> unit =
    fun elements name info target title simple_file ->
      try
        let chanout = open_out (Filename.concat !Args.target_dir simple_file) in
        output_string chanout 
          (
           "<html>\n"^
           (self#header (self#inner_title title)) ^
           "<body>\n"^
            "<center><h1>"^title^"</h1></center>\n");
        
        let sorted_elements = List.sort 
            (fun e1 -> fun e2 -> compare (Name.simple (name e1)) (Name.simple (name e2)))
            elements
        in
        let groups = Odoc_info.create_index_lists sorted_elements (fun e -> Name.simple (name e)) in
        let f_ele e =
          let simple_name = Name.simple (name e) in
          let father_name = Name.father (name e) in
          output_string chanout
            ("<tr><td><a href=\""^(target e)^"\">"^simple_name^"</a> "^
             (if simple_name <> father_name && father_name <> "" then 
               "["^"<a href=\""^(fst (Naming.html_files father_name))^"\">"^father_name^"</a>]"
             else
               ""
             )^
             "</td>\n"^
             "<td>"^(self#html_of_info_first_sentence (info e))^"</td></tr>\n"
            )
        in
        let f_group l =
          match l with
            [] -> ()
          | e :: _ ->
              let s = 
                match (Char.uppercase (Name.simple (name e)).[0]) with
                  'A'..'Z' as c -> String.make 1 c
                | _ -> ""
              in
              output_string chanout ("<tr><td align=\"left\"><br>"^s^"</td></tr>\n");
              List.iter f_ele l
        in
        output_string chanout "<table>\n";
        List.iter f_group groups ;
        output_string chanout "</table><br>\n" ;
        output_string chanout "</body>\n</html>";
        close_out chanout
      with
        Sys_error s ->
          raise (Failure s)

    (** A method to generate a list of module/class files. *)
    method generate_elements :
        'a. ('a option -> 'a option -> 'a -> unit) -> 'a list -> unit =
      fun f_generate l ->
        let rec iter pre_opt = function
            [] -> ()
          | ele :: [] -> f_generate pre_opt None ele
          | ele1 :: ele2 :: q -> 
              f_generate pre_opt (Some ele2) ele1 ;
              iter (Some ele1) (ele2 :: q)
        in
        iter None l

    (** Generate the code of the html page for the given class.*)
    method generate_for_class pre post cl =
      Odoc_info.reset_type_names ();
      let (html_file, _) = Naming.html_files cl.cl_name in
      let type_file = Naming.file_type_class_complete_target cl.cl_name in
      try
        let chanout = open_out (Filename.concat !Args.target_dir html_file) in
        let pre_name = opt (fun c -> c.cl_name) pre in
        let post_name = opt (fun c -> c.cl_name) post in
        output_string chanout
          ("<html>\n"^
           (self#header 
              ~nav: (Some (pre_name, post_name, cl.cl_name))
              ~comments: (Class.class_comments cl)
              (self#inner_title cl.cl_name)
           )^
           "<body>\n"^
           (self#navbar pre_name post_name cl.cl_name)^
           "<center><h1>"^Odoc_messages.clas^" "^
           (if cl.cl_virtual then "virtual " else "")^
           "<a href=\""^type_file^"\">"^cl.cl_name^"</a>"^
           "</h1></center>\n"^
           "<br>\n"^
           (self#html_of_class ~with_link: false cl)
          );
        (* parameters *)
        output_string chanout 
          (self#html_of_described_parameter_list (Name.father cl.cl_name) cl.cl_parameters);
        (* class inheritance *)
        self#generate_class_inheritance_info chanout cl;
        (* a horizontal line *)
        output_string chanout "<hr width=\"100%\">\n";
        (* the various elements *)
        List.iter 
          (fun element ->
            match element with
              Class_attribute a ->
                output_string chanout (self#html_of_attribute a)
            | Class_method m ->
                output_string chanout (self#html_of_method m)
            | Class_comment t ->
                output_string chanout (self#html_of_class_comment t)
          )
          (Class.class_elements ~trans:false cl);
        output_string chanout "</body></html>";
        close_out chanout;

        (* generate the file with the complete class type *)
        self#output_class_type 
          cl.cl_name
          (Filename.concat !Args.target_dir type_file)
          cl.cl_type
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the code of the html page for the given class type.*)
    method generate_for_class_type pre post clt =
      Odoc_info.reset_type_names ();
      let (html_file, _) = Naming.html_files clt.clt_name in
      let type_file = Naming.file_type_class_complete_target clt.clt_name in
      try
        let chanout = open_out (Filename.concat !Args.target_dir html_file) in
        let pre_name = opt (fun ct -> ct.clt_name) pre in
        let post_name = opt (fun ct -> ct.clt_name) post in
        output_string chanout
          ("<html>\n"^
           (self#header 
              ~nav: (Some (pre_name, post_name, clt.clt_name))
              ~comments: (Class.class_type_comments clt)
              (self#inner_title clt.clt_name)
           )^
           "<body>\n"^
           (self#navbar pre_name post_name clt.clt_name)^
           "<center><h1>"^Odoc_messages.class_type^" "^
           (if clt.clt_virtual then "virtual " else "")^
           "<a href=\""^type_file^"\">"^clt.clt_name^"</a>"^
           "</h1></center>\n"^
           "<br>\n"^
           (self#html_of_class_type ~with_link: false clt)
          );
        (* class inheritance *)
        self#generate_class_type_inheritance_info chanout clt;
        (* a horizontal line *)
        output_string chanout "<hr width=\"100%\">\n";
        (* the various elements *)
        List.iter 
          (fun element ->
            match element with
              Class_attribute a ->
                output_string chanout (self#html_of_attribute a)
            | Class_method m ->
                output_string chanout (self#html_of_method m)
            | Class_comment t ->
                output_string chanout (self#html_of_class_comment t)
          )
          (Class.class_type_elements ~trans: false clt);
        output_string chanout "</body></html>";
        close_out chanout;

        (* generate the file with the complete class type *)
        self#output_class_type 
          clt.clt_name
          (Filename.concat !Args.target_dir type_file)
          clt.clt_type
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the html file for the given module type. 
       @raise Failure if an error occurs.*)
    method generate_for_module_type pre post mt =
      try
        let (html_file, _) = Naming.html_files mt.mt_name in
        let type_file = Naming.file_type_module_complete_target mt.mt_name in
        let chanout = open_out (Filename.concat !Args.target_dir html_file) in
        let pre_name = opt (fun mt -> mt.mt_name) pre in
        let post_name = opt (fun mt -> mt.mt_name) post in
        output_string chanout
          ("<html>\n"^
           (self#header
              ~nav: (Some (pre_name, post_name, mt.mt_name))
              ~comments: (Module.module_type_comments mt)
              (self#inner_title mt.mt_name)
           )^
           "<body>\n"^
           (self#navbar pre_name post_name mt.mt_name)^
           "<center><h1>"^Odoc_messages.module_type^
           " "^
           (match mt.mt_type with
             Some _ -> "<a href=\""^type_file^"\">"^mt.mt_name^"</a>"
           | None-> mt.mt_name
           )^
           "</h1></center>\n"^
           "<br>\n"^
           (self#html_of_modtype ~with_link: false mt)
          );
        (* parameters for functors *)
        output_string chanout (self#html_of_module_parameter_list "" (Module.module_type_parameters mt));
        (* a horizontal line *)
        output_string chanout "<hr width=\"100%\">\n";
        (* module elements *)
        List.iter 
          (fun ele ->
            match ele with
              Element_module m ->
                output_string chanout (self#html_of_module ~complete: false m)
            | Element_module_type mt ->
                output_string chanout (self#html_of_modtype ~complete: false mt)
            | Element_included_module im ->
                output_string chanout (self#html_of_included_module im)
            | Element_class c ->
                output_string chanout (self#html_of_class ~complete: false c)
            | Element_class_type ct ->
                output_string chanout (self#html_of_class_type ~complete: false ct)
            | Element_value v ->
                output_string chanout (self#html_of_value v)
            | Element_exception e ->
                output_string chanout (self#html_of_exception e)
            | Element_type t ->
                output_string chanout (self#html_of_type t)
            | Element_module_comment text ->
                output_string chanout (self#html_of_module_comment text)
          )
          (Module.module_type_elements mt);

        output_string chanout "</body></html>";       
        close_out chanout;

        (* generate html files for submodules *)
        self#generate_elements self#generate_for_module (Module.module_type_modules mt);
        (* generate html files for module types *)
        self#generate_elements self#generate_for_module_type (Module.module_type_module_types mt);
        (* generate html files for classes *)
        self#generate_elements self#generate_for_class (Module.module_type_classes mt);
        (* generate html files for class types *)
        self#generate_elements self#generate_for_class_type (Module.module_type_class_types mt);

        (* generate the file with the complete module type *)
        (
         match mt.mt_type with
           None -> ()
         | Some mty -> self#output_module_type 
               mt.mt_name
               (Filename.concat !Args.target_dir type_file) 
               mty
        )
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the html file for the given module. 
       @raise Failure if an error occurs.*)
    method generate_for_module pre post modu =
      try
        Odoc_info.verbose ("Generate for module "^modu.m_name);
        let (html_file, _) = Naming.html_files modu.m_name in
        let type_file = Naming.file_type_module_complete_target modu.m_name in
        let code_file = Naming.file_code_module_complete_target modu.m_name in
        let chanout = open_out (Filename.concat !Args.target_dir html_file) in
        let pre_name = opt (fun m -> m.m_name) pre in
        let post_name = opt (fun m -> m.m_name) post in
        output_string chanout
          ("<html>\n"^
           (self#header 
              ~nav: (Some (pre_name, post_name, modu.m_name))
              ~comments: (Module.module_comments modu)
              (self#inner_title modu.m_name)
           ) ^
           "<body>\n"^
           (self#navbar pre_name post_name modu.m_name)^
           "<center><h1>"^(if Module.module_is_functor modu then Odoc_messages.functo else Odoc_messages.modul)^
           " "^
           "<a href=\""^type_file^"\">"^modu.m_name^"</a>"^
	   (
	    match modu.m_code with
	      None -> ""
	    | Some _ -> Printf.sprintf " (<a href=\"%s\">.ml</a>)" code_file
	   )^
           "</h1></center>\n"^
           "<br>\n"^
           (self#html_of_module ~with_link: false modu)
          );
        (* parameters for functors *)
        output_string chanout (self#html_of_module_parameter_list "" (Module.module_parameters modu));
        (* a horizontal line *)
        output_string chanout "<hr width=\"100%\">\n";
        (* module elements *)
        List.iter 
          (fun ele ->
            print_DEBUG "html#generate_for_module : ele ->";
            match ele with
              Element_module m ->
                output_string chanout (self#html_of_module ~complete: false m)
            | Element_module_type mt ->
                output_string chanout (self#html_of_modtype ~complete: false mt)
            | Element_included_module im ->
                output_string chanout (self#html_of_included_module im)
            | Element_class c ->
                output_string chanout (self#html_of_class ~complete: false c)
            | Element_class_type ct ->
                output_string chanout (self#html_of_class_type ~complete: false ct)
            | Element_value v ->
                output_string chanout (self#html_of_value v)
            | Element_exception e ->
                output_string chanout (self#html_of_exception e)
            | Element_type t ->
                output_string chanout (self#html_of_type t)
            | Element_module_comment text ->
                output_string chanout (self#html_of_module_comment text)
          )
          (Module.module_elements modu);

        output_string chanout "</body></html>";
        close_out chanout;

        (* generate html files for submodules *)
        self#generate_elements  self#generate_for_module (Module.module_modules modu);
        (* generate html files for module types *)
        self#generate_elements  self#generate_for_module_type (Module.module_module_types modu);
        (* generate html files for classes *)
        self#generate_elements  self#generate_for_class (Module.module_classes modu);
        (* generate html files for class types *)
        self#generate_elements  self#generate_for_class_type (Module.module_class_types modu);
        
        (* generate the file with the complete module type *)
        self#output_module_type 
          modu.m_name
          (Filename.concat !Args.target_dir type_file)
          modu.m_type;

	match modu.m_code with
	  None -> ()
	| Some code ->
	    self#output_code
	      modu.m_name
	      (Filename.concat !Args.target_dir code_file)
	      code
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the [index.html] file corresponding to the given module list.
       @raise Failure if an error occurs.*)
    method generate_index module_list =
      try
        let title = match !Args.title with None -> "" | Some t -> self#escape t in
        let index_if_not_empty l url m =
          match l with
            [] -> ""
          | _ -> "<a href=\""^url^"\">"^m^"</a><br>\n"
        in
        let chanout = open_out (Filename.concat !Args.target_dir index) in
        output_string chanout 
          (
           "<html>\n"^
           (self#header self#title) ^
           "<body>\n"^
           "<center><h1>"^title^"</h1></center>\n"^
           (index_if_not_empty list_types index_types Odoc_messages.index_of_types)^
           (index_if_not_empty list_exceptions index_exceptions Odoc_messages.index_of_exceptions)^
           (index_if_not_empty list_values index_values Odoc_messages.index_of_values)^
           (index_if_not_empty list_attributes index_attributes Odoc_messages.index_of_attributes)^
           (index_if_not_empty list_methods index_methods Odoc_messages.index_of_methods)^
           (index_if_not_empty list_classes index_classes Odoc_messages.index_of_classes)^
           (index_if_not_empty list_class_types index_class_types Odoc_messages.index_of_class_types)^
           (index_if_not_empty list_modules index_modules Odoc_messages.index_of_modules)^
           (index_if_not_empty list_module_types index_module_types Odoc_messages.index_of_module_types)^
           "<br>\n"^
           "<table class=\"indextable\">\n"^
           (String.concat ""
              (List.map
                 (fun m ->
                   let (html, _) = Naming.html_files m.m_name in
                   "<tr><td><a href=\""^html^"\">"^m.m_name^"</a></td>"^
                   "<td>"^(self#html_of_info_first_sentence m.m_info)^"</td></tr>\n")
                 module_list
              )
           )^
           "</table>\n"^
           "</body>\n"^
           "</html>"
          );
        close_out chanout
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the values index in the file [index_values.html]. *)
    method generate_values_index module_list =
      self#generate_elements_index 
        list_values
        (fun v -> v.val_name) 
        (fun v -> v.val_info)
        Naming.complete_value_target
        Odoc_messages.index_of_values
        index_values

    (** Generate the exceptions index in the file [index_exceptions.html]. *)
    method generate_exceptions_index module_list =
      self#generate_elements_index 
        list_exceptions
        (fun e -> e.ex_name) 
        (fun e -> e.ex_info)
        Naming.complete_exception_target
        Odoc_messages.index_of_exceptions
        index_exceptions

    (** Generate the types index in the file [index_types.html]. *)
    method generate_types_index module_list =
      self#generate_elements_index 
        list_types
        (fun t -> t.ty_name) 
        (fun t -> t.ty_info)
        Naming.complete_type_target
        Odoc_messages.index_of_types
        index_types

    (** Generate the attributes index in the file [index_attributes.html]. *)
    method generate_attributes_index module_list =
      self#generate_elements_index 
        list_attributes
        (fun a -> a.att_value.val_name) 
        (fun a -> a.att_value.val_info)
        Naming.complete_attribute_target
        Odoc_messages.index_of_attributes
        index_attributes

    (** Generate the methods index in the file [index_methods.html]. *)
    method generate_methods_index module_list =
      self#generate_elements_index 
        list_methods
        (fun m -> m.met_value.val_name) 
        (fun m -> m.met_value.val_info)
        Naming.complete_method_target
        Odoc_messages.index_of_methods
        index_methods

    (** Generate the classes index in the file [index_classes.html]. *)
    method generate_classes_index module_list =
      self#generate_elements_index
        list_classes
        (fun c -> c.cl_name) 
        (fun c -> c.cl_info)
        (fun c -> fst (Naming.html_files c.cl_name))
        Odoc_messages.index_of_classes
        index_classes

    (** Generate the class types index in the file [index_class_types.html]. *)
    method generate_class_types_index module_list =
      self#generate_elements_index
        list_class_types
        (fun ct -> ct.clt_name) 
        (fun ct -> ct.clt_info)
        (fun ct -> fst (Naming.html_files ct.clt_name))
        Odoc_messages.index_of_class_types
        index_class_types

    (** Generate the modules index in the file [index_modules.html]. *)
    method generate_modules_index module_list =
      self#generate_elements_index
        list_modules
        (fun m -> m.m_name) 
        (fun m -> m.m_info)
        (fun m -> fst (Naming.html_files m.m_name))
        Odoc_messages.index_of_modules
        index_modules

    (** Generate the module types index in the file [index_module_types.html]. *)
    method generate_module_types_index module_list =
      let module_types = Odoc_info.Search.module_types module_list in
      self#generate_elements_index
        list_module_types
        (fun mt -> mt.mt_name) 
        (fun mt -> mt.mt_info)
        (fun mt -> fst (Naming.html_files mt.mt_name))
        Odoc_messages.index_of_module_types
        index_module_types

    (** Generate all the html files from a module list. The main
       file is [index.html]. *)
    method generate module_list =
      (* init the style *)
      self#init_style ;
      (* init the lists of elements *)
      list_values <- Odoc_info.Search.values module_list ;
      list_exceptions <- Odoc_info.Search.exceptions module_list ;
      list_types <- Odoc_info.Search.types module_list ;
      list_attributes <- Odoc_info.Search.attributes module_list ;
      list_methods <- Odoc_info.Search.methods module_list ;
      list_classes <- Odoc_info.Search.classes module_list ;
      list_class_types <- Odoc_info.Search.class_types module_list ;
      list_modules <- Odoc_info.Search.modules module_list ;
      list_module_types <- Odoc_info.Search.module_types module_list ;
      
      (* prepare the page header *)
      self#prepare_header module_list ;
      (* Get the names of all known types. *)
      let types = Odoc_info.Search.types module_list in
      let type_names = List.map (fun t -> t.ty_name) types in
      known_types_names <- type_names ;
      (* Get the names of all class and class types. *)
      let classes = Odoc_info.Search.classes module_list in
      let class_types = Odoc_info.Search.class_types module_list in
      let class_names = List.map (fun c -> c.cl_name) classes in
      let class_type_names = List.map (fun ct -> ct.clt_name) class_types in
      known_classes_names <- class_names @ class_type_names ;
      (* Get the names of all known modules and module types. *)
      let module_types = Odoc_info.Search.module_types module_list in
      let modules = Odoc_info.Search.modules module_list in
      let module_type_names = List.map (fun mt -> mt.mt_name) module_types in
      let module_names = List.map (fun m -> m.m_name) modules in
      known_modules_names <- module_type_names @ module_names ;
      (* generate html for each module *)
      if not !Args.index_only then 
        self#generate_elements self#generate_for_module module_list ;

      try
        self#generate_index module_list;
        self#generate_values_index module_list ;
        self#generate_exceptions_index module_list ;
        self#generate_types_index module_list ;
        self#generate_attributes_index module_list ;
        self#generate_methods_index module_list ;
        self#generate_classes_index module_list ;
        self#generate_class_types_index module_list ;
        self#generate_modules_index module_list ;
        self#generate_module_types_index module_list ;
      with
        Failure s ->
          prerr_endline s ;
          incr Odoc_info.errors

    initializer
      Odoc_ocamlhtml.html_of_comment := 
        (fun s -> self#html_of_text (Odoc_text.Texter.text_of_string s))
  end


                             
