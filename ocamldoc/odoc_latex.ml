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


(** Generation of LaTeX documentation. *)

let print_DEBUG s = print_string s ; print_newline ()

open Odoc_info 
open Parameter
open Value
open Type
open Exception
open Class 
open Module


(** Generation of LaTeX code from text structures. *)
class text =
  object (self)
    (** Return latex code to make a sectionning according to the given level,
       and with the given latex code. *)
    method section_style level s =
      try 
	let sec = List.assoc level !Odoc_args.latex_titles in
	"\\"^sec^"{"^s^"}\n"
      with Not_found -> s

    (** Associations of strings to subsitute in latex code. *)
    val mutable subst_strings = [
      ("MAXENCE"^"ZZZ", "\\$");
      ("MAXENCE"^"YYY", "\\&");
      ("MAXENCE"^"XXX", "{\\textbackslash}") ;
      ("à", "\\`a") ;
      ("â", "\\^a") ;
      ("é", "\\'e") ;
      ("è", "\\`e") ;
      ("ê", "\\^e") ;
      ("ë", "\\\"e") ;
      ("ç", "\\c{c}") ;
      ("ô", "\\^o") ;
      ("ö", "\\\"o") ;
      ("î", "\\^i") ;
      ("ï", "\\\"i") ;
      ("ù", "\\`u") ;
      ("û", "\\^u") ;
      ("%", "\\%") ;
      ("_", "\\_");
      ("\\.\\.\\.", "$\\ldots$");
      ("~", "\\~{}");
      ("#", "\\verb`#`");
      ("}", "\\}");
      ("{", "\\{");
      ("&", "\\&");
      (">", "$>$");
      ("<", "$<$");
      ("=", "$=$");
      (">=", "$\\geq$");
      ("<=", "$\\leq$");
      ("->", "$\\rightarrow$") ;
      ("<-", "$\\leftarrow$");
      ("|", "\\textbar ");
      ("\\^", "\\textasciicircum ") ;
      ("\\.\\.\\.", "$\\ldots$");
      ("\\\\", "MAXENCE"^"XXX") ;
      ("&", "MAXENCE"^"YYY") ;
      ("\\$", "MAXENCE"^"ZZZ")
     ] 

    val mutable subst_strings_simple =    
      [ 
	("MAXENCE"^"XXX", "{\\textbackslash}") ;
	"}", "\\}" ;
	"{", "\\{" ;
	("\\\\", "MAXENCE"^"XXX") ;
      ] 

    val mutable subst_strings_code = [
      ("MAXENCE"^"ZZZ", "\\$");
      ("MAXENCE"^"YYY", "\\&");
      ("MAXENCE"^"XXX", "{\\textbackslash}") ;
      ("%", "\\%") ;
      ("_", "\\_");
      ("~", "\\~{}");
      ("#", "\\verb`#`");
      ("}", "\\}");
      ("{", "\\{");
      ("&", "\\&");
      ("\\^", "\\textasciicircum ") ;
      ("&", "MAXENCE"^"YYY") ;
      ("\\$", "MAXENCE"^"ZZZ") ;
      ("\\\\", "MAXENCE"^"XXX") ;
     ] 

    method subst l s =
      List.fold_right
	(fun (s, s2) -> fun acc -> Str.global_replace (Str.regexp s) s2 acc)
	l
	s

    (** Escape the strings which would clash with LaTeX syntax. *)
    method escape s = self#subst subst_strings s

    (** Escape the ['\'], ['{'] and ['}'] characters. *)
    method escape_simple s = self#subst subst_strings_simple s

    (** Escape some characters for the code style. *)
    method escape_code s = self#subst subst_strings_code s
	
    (** Make a correct latex label from a name. *)
    method label ?(no_=true) name =
      let s = 
	if no_ then 
	  Str.global_replace (Str.regexp_string "_") "" name 
	else
	  name
      in
      List.fold_right
       (fun (s, s2) -> fun acc -> Str.global_replace (Str.regexp_string s) s2 acc)
	[ 
	  "@", "\"@" ;
	  "!", "\"!" ;
	  "|", "\"|" ;
	] 
	s

    (** Return latex code for the label of a name. *)
    method make_label name = 
      "\\label{"^(self#label name)^"}"

    (** Return latex code for the ref to a name. *)
    method make_ref name = 
      "\\ref{"^(self#label name)^"}"

    (** Return the LaTeX code corresponding to the [text] parameter.*)
    method latex_of_text t = String.concat "" (List.map self#latex_of_text_element t)
	
    (** Return the LaTeX code for the [text_element] in parameter. *)
    method latex_of_text_element te =
      match te with
      |	Odoc_info.Raw s -> self#latex_of_Raw s
      |	Odoc_info.Code s -> self#latex_of_Code s
      |	Odoc_info.CodePre s -> self#latex_of_CodePre s
      |	Odoc_info.Verbatim s -> self#latex_of_Verbatim s
      |	Odoc_info.Bold t -> self#latex_of_Bold t
      |	Odoc_info.Italic t -> self#latex_of_Italic t 
      |	Odoc_info.Emphasize t -> self#latex_of_Emphasize t
      |	Odoc_info.Center t -> self#latex_of_Center t
      |	Odoc_info.Left t -> self#latex_of_Left t
      |	Odoc_info.Right t -> self#latex_of_Right t
      |	Odoc_info.List tl -> self#latex_of_List tl
      |	Odoc_info.Enum tl -> self#latex_of_Enum tl
      |	Odoc_info.Newline -> self#latex_of_Newline
      |	Odoc_info.Block t -> self#latex_of_Block t
      |	Odoc_info.Title (n, l_opt, t) -> self#latex_of_Title n l_opt t
      |	Odoc_info.Latex s -> self#latex_of_Latex s
      |	Odoc_info.Link (s, t) -> self#latex_of_Link s t
      |	Odoc_info.Ref (name, ref_opt) -> self#latex_of_Ref name ref_opt
      |	Odoc_info.Superscript t -> self#latex_of_Superscript t
      |	Odoc_info.Subscript t -> self#latex_of_Subscript t
	    
    method latex_of_Raw s = self#escape s

    method latex_of_Code s = 
      let s2 = self#escape_code s in
      let s3 = Str.global_replace (Str.regexp "\n") ("\\\\\n") s2 in
      "{\\tt{"^s3^"}}"

    method latex_of_CodePre s =
      "\n\\begin{ocamldoccode}\n"^(self#escape_simple s)^"\n\\end{ocamldoccode}\n"

    method latex_of_Verbatim s = "\\begin{verbatim}"^s^"\\end{verbatim}"

    method latex_of_Bold t =
      let s = self#latex_of_text t in
      "{\\bf "^s^"}"

    method latex_of_Italic t = 
      let s = self#latex_of_text t in
      "{\\it "^s^"}"

    method latex_of_Emphasize t =
      let s = self#latex_of_text t in
      "{\\em "^s^"}"

    method latex_of_Center t =
      let s = self#latex_of_text t in
      "\\begin{center}\n"^s^"\\end{center}\n"

    method latex_of_Left t =
      let s = self#latex_of_text t in
      "\\begin{flushleft}\n"^s^"\\end{flushleft}\n"

    method latex_of_Right t =
      let s = self#latex_of_text t in
      "\\begin{flushright}\n"^s^"\\end{flushright}\n"

    method latex_of_List tl =
      "\\begin{itemize}"^
      (String.concat ""
	 (List.map (fun t -> "\\item "^(self#latex_of_text t)^"\n") tl))^
      "\\end{itemize}\n"

    method latex_of_Enum tl =
      "\\begin{enumerate}"^
      (String.concat ""
	 (List.map (fun t -> "\\item "^(self#latex_of_text t)^"\n") tl))^
      "\\end{enumerate}\n"

    method latex_of_Newline = "\n\n"

    method latex_of_Block t =
      let s = self#latex_of_text t in
      "\\begin{ocamldocdescription}\n"^s^"\n\\end{ocamldocdescription}\n"

    method latex_of_Title n label_opt t =
      let s_title = self#latex_of_text t in
      let s_title2 = self#section_style n s_title in
      s_title2^
      (match label_opt with
	None -> ""
      |	Some l -> self#make_label l)

    method latex_of_Latex s = s

    method latex_of_Link s t =
      let s1 = "\\url{"^s^"} " in
      let s2 = self#latex_of_text t in
      s1^s2

    method latex_of_Ref name ref_opt =
      match ref_opt with
	None -> 
	  self#latex_of_text_element 
	    (Odoc_info.Code (Odoc_info.use_hidden_modules name))
      |	Some kind when kind = RK_section -> 
	  self#latex_of_text_element (Latex ("["^(self#make_ref (Name.simple name))^"]"))
      |	Some kind ->
	  let target = 
	    match kind with
	      Odoc_info.RK_module 
	    | Odoc_info.RK_module_type
	    | Odoc_info.RK_class
	    | Odoc_info.RK_class_type
	    | Odoc_info.RK_value
	    | Odoc_info.RK_type
	    | Odoc_info.RK_exception
	    | Odoc_info.RK_attribute
	    | Odoc_info.RK_method -> name
	    | Odoc_info.RK_section -> assert false
	  in
	  (self#latex_of_text 
	     [
	       Odoc_info.Code (Odoc_info.use_hidden_modules name) ;
	       Latex ("["^(self#make_ref name)^"]")
	     ] 
	  )

    method latex_of_Superscript t = "$^{"^(self#latex_of_text t)^"}$"

    method latex_of_Subscript t = "$_{"^(self#latex_of_text t)^"}$"

  end

(** A class used to generate LaTeX code for info structures. *)
class virtual info =
  object (self)
    (** The method used to get LaTeX code from a [text]. *)
    method virtual latex_of_text : Odoc_info.text -> string

    (** The method used to get a [text] from an optionel info structure. *)
    method virtual text_of_info : ?block: bool -> Odoc_info.info option -> Odoc_info.text 

    (** Return LaTeX code for a description, except for the [i_params] field. *)
    method latex_of_info info_opt = 
      self#latex_of_text 
	(self#text_of_info ~block: false info_opt)
  end

(** This class is used to create objects which can generate a simple LaTeX documentation. *)
class latex =
  object (self)
    inherit text
    inherit Odoc_to_text.to_text as to_text
    inherit info

    (** Get the first sentence and the rest of a description,
       from an optional [info] structure. The first sentence
       can be empty if it would not appear right in a title.
    *)
    method first_and_rest_of_info i_opt =
      match i_opt with
	None -> ([], [])
      | Some i -> 
	    match i.Odoc_info.i_desc with
	      None -> ([], self#text_of_info ~block: true i_opt)
	    | Some t -> 
		let (first,_) = Odoc_info.first_sentence_and_rest_of_text t in
		let (_, rest) = Odoc_info.first_sentence_and_rest_of_text (self#text_of_info ~block: false i_opt) in
		(first, rest)

    (** Return LaTeX code for a value. *)
    method latex_of_value v = 
      Odoc_info.reset_type_names () ;
      self#latex_of_text 
	((Latex (self#make_label v.val_name)) :: 
	 (to_text#text_of_value v))

    (** Return LaTeX code for a class attribute. *)
    method latex_of_attribute a =
      self#latex_of_text 
	((Latex (self#make_label a.att_value.val_name)) :: 
	 (to_text#text_of_attribute a))

    (** Return LaTeX code for a class method. *)
    method latex_of_method m = 
      self#latex_of_text
	((Latex (self#make_label m.met_value.val_name)) :: 
	 (to_text#text_of_method m))

    (** Return LaTeX code for a type. *)
    method latex_of_type t =
      let s_name = Name.simple t.ty_name in
      let text = 
	Odoc_info.reset_type_names () ;
	let mod_name = Name.father t.ty_name in
	let s_type1 = 
	  Format.fprintf Format.str_formatter 
	    "@[<hov 2>type ";
	  match t.ty_parameters with
	    [] -> Format.flush_str_formatter ()
	  | [p] -> self#normal_type mod_name p
	  | l -> 
	      Format.fprintf Format.str_formatter "(" ;
	      let s = self#normal_type_list mod_name ", " l in
	      s^")"
	in
	Format.fprintf Format.str_formatter 
	  ("@[<hov 2>%s %s")
	  s_type1
	  s_name;
	let s_type2 = 
	  match t.ty_manifest with
	    None -> Format.flush_str_formatter ()
	  | Some typ -> 
	      Format.fprintf Format.str_formatter " = ";
	      self#normal_type mod_name typ
	in
	let s_type3 = 
	  Format.fprintf Format.str_formatter 
	    ("%s %s")
	    s_type2
	    (match t.ty_kind with
	      Type_abstract -> ""
	    | Type_variant _ -> "="
	    | Type_record _ -> "= {" ) ;
	  Format.flush_str_formatter ()
	in
	
	let defs = 
	  match t.ty_kind with
	    Type_abstract -> []
	  | Type_variant l ->
	      (List.flatten
	       (List.map
		  (fun constr ->
		    let s_cons = 
		      Format.fprintf Format.str_formatter 
			"@[<hov 6>  | %s"
			constr.vc_name;
		      match constr.vc_args with
			[] -> Format.flush_str_formatter ()
		      | l -> 
			  Format.fprintf Format.str_formatter " %s@ " "of";
			  self#normal_type_list mod_name " * " l
		    in
		    [ CodePre s_cons ] @
		    (match constr.vc_text with
		      None -> []
		    | Some t -> 
			[ Latex
			    ("\\begin{ocamldoccomment}\n"^
			     (self#latex_of_text t)^
			     "\n\\end{ocamldoccomment}\n")
			] 
		    )
		  )
		  l
	       )
	      )
	  | Type_record l ->
	      (List.flatten
		 (List.map
		    (fun r ->
		      let s_field = 
			Format.fprintf Format.str_formatter 
			  "@[<hov 6>  %s%s :@ "
			  (if r.rf_mutable then "mutable " else "")
			  r.rf_name;
			(self#normal_type mod_name r.rf_type)^" ;"
		      in
		      [ CodePre s_field ] @
		      (match r.rf_text with
			None -> []
		      | Some t -> 
			  [ Latex
			      ("\\begin{ocamldoccomment}\n"^
			       (self#latex_of_text t)^
			       "\n\\end{ocamldoccomment}\n")
			  ] 
		      )
		    )
		    l
		 )
	      ) @
	      [ CodePre "}" ]
	in
	let defs2 = (CodePre s_type3) :: defs in
	let rec iter = function
	    [] -> []
	  | [e] -> [e]
	  | (CodePre s1) :: (CodePre s2) :: q ->
	      iter ((CodePre (s1^"\n"^s2)) :: q)
	  | e :: q ->
	      e :: (iter q)
	in
	(iter defs2) @
	[Latex ("\\index{"^(self#label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")] @
	(self#text_of_info t.ty_info)
      in
      self#latex_of_text 
	((Latex (self#make_label t.ty_name)) :: text)

    (** Return LaTeX code for an exception. *)
    method latex_of_exception e =
      Odoc_info.reset_type_names () ;
      self#latex_of_text 
	((Latex (self#make_label e.ex_name)) :: 
	 (to_text#text_of_exception e))

    (** Return the LaTeX code for the given module. *)
    method latex_of_module ?(with_link=true) m =
      let t = 
	[Code "module "] @
	[Code (Name.simple m.m_name)] @
	(self#text_of_module_kind m.m_kind) @
	(
	 if with_link 
	 then [Odoc_info.Latex ("\\\n["^(self#make_ref m.m_name)^"]")]
	 else [] 
	)
      in
      self#latex_of_text t

    (** Return the LaTeX code for the given module type. *)
    method latex_of_module_type ?(with_link=true) mt =
      let t = 
	[Code "module type "] @
	[Code (Name.simple mt.mt_name)] @
	(match mt.mt_kind with
	  None -> []
	| Some k -> self#text_of_module_type_kind k
	) @
	(
	 if with_link 
	 then [Odoc_info.Latex ("\\\n["^(self#make_ref mt.mt_name)^"]")]
	 else [] 
	)
      in
      self#latex_of_text t

    (** Return the LaTeX code for the given included module. *)
    method latex_of_included_module im =
      (self#latex_of_text [ Code "include module " ; 
			    Code 
			      (match im.im_module with
				None -> im.im_name
			      |	Some (Mod m) -> m.m_name
			      |	Some (Modtype mt) -> mt.mt_name)
			  ] )

    (** Return a well-formatted code string for the given [class_kind].
       This method uses [Format.str_formatter].*)
    method pre_of_class_kind father acc ?(with_def_syntax=true) ckind =
      let p = Format.fprintf in
      let f = Format.str_formatter in
      p f "%s%s" acc (if with_def_syntax then " = " else "");
      match ckind with
	Class_structure _ -> 
	  p f "%s" Odoc_messages.object_end ;
	  Format.flush_str_formatter ()

      |	Class_apply capp ->
	  p f "%s" 
	    (match capp.capp_class with
	      None -> capp.capp_name
	    | Some cl -> cl.cl_name
	    );
	  List.iter 
	    (fun s -> p f " (%s)" s)
	    capp.capp_params_code;
	  Format.flush_str_formatter ()
	    
      |	Class_constr cco ->
	  (match cco.cco_type_parameters with
	    [] -> ()
	  | l -> 
	      p f "[";
	      let s = self#normal_type_list father ", " l in
	      p f "%s] " s
	  );
	  p f "%s"
	    (match cco.cco_class with
	      None -> cco.cco_name
	    | Some (Cl cl) -> cl.cl_name
	    | Some (Cltype (clt, _)) -> clt.clt_name
	    );
	  Format.flush_str_formatter ()

      |	Class_constraint (ck, ctk) ->
	  p f "(" ;
	  let s = self#pre_of_class_kind father 
	      (Format.flush_str_formatter ())
	      ~with_def_syntax: false ck
	  in
	  p f "%s : " s;
	  let s2 = self#pre_of_class_type_kind father 
	      (Format.flush_str_formatter ())
	      ctk
	  in
	  p f "%s)" s2 ;
	  Format.flush_str_formatter ()

    (** Return well-formatted string for the given [class_type_kind].
       This method uses [Format.str_formatter].*)
    method pre_of_class_type_kind father acc ?def_syntax ctkind =
      let p = Format.fprintf in
      let f = Format.str_formatter in
      p f "%s%s" acc 
	(match def_syntax with
	  None -> ""
	| Some s -> " "^s^" ");
      match ctkind with
	Class_type cta -> 
	  (
	   match cta.cta_type_parameters with
	     [] -> ()
	   | l -> 
	       p f "[" ;
	       let s = self#normal_type_list father ", " l in
	       p f "%s] " s
	  );
	  p f "%s" 
	    (
	     match cta.cta_class with
	       None -> cta.cta_name
	     | Some (Cltype (clt, _)) -> clt.clt_name
	     | Some (Cl cl) -> cl.cl_name
	    );
	  Format.flush_str_formatter ()

      |	Class_signature _ ->
	  p f "%s" Odoc_messages.object_end ;
	  Format.flush_str_formatter ()


    (** Return the LaTeX code for the given class. *)
    method latex_of_class ?(with_link=true) c =
      Odoc_info.reset_type_names () ;
      let father = Name.father c.cl_name in
      let t = 
	let s = 
	  Format.fprintf Format.str_formatter "class %s" 
	    (if c.cl_virtual then "virtual " else "");
	  (
	   match c.cl_type_parameters with
	     [] -> ()
	   | l -> 
	       Format.fprintf Format.str_formatter "[" ;
	       let s1 = self#normal_type_list father ", " l in
	       Format.fprintf Format.str_formatter "%s] " s1
	  );
	  Format.fprintf Format.str_formatter "%s%s"
	    (Name.simple c.cl_name)
	    (match c.cl_parameters with [] -> "" | _ -> " ...");
	  Format.flush_str_formatter ()
	in
	(CodePre (self#pre_of_class_kind father s c.cl_kind)) ::
	(
	 if with_link 
	 then [Odoc_info.Latex (" ["^(self#make_ref c.cl_name)^"]")]
	 else [] 
	) 
      in
      self#latex_of_text t

    (** Return the LaTeX code for the given class type. *)
    method latex_of_class_type ?(with_link=true) ct =
      Odoc_info.reset_type_names () ;
      let father = Name.father ct.clt_name in
      let t = 
	let s = 
	  Format.fprintf Format.str_formatter "class type %s" 
	    (if ct.clt_virtual then "virtual " else "");
	  (
	   match ct.clt_type_parameters with
	     [] -> ()
	   | l -> 
	       Format.fprintf Format.str_formatter "[" ;
	       let s1 = self#normal_type_list father ", " l in
	       Format.fprintf Format.str_formatter "%s] " s1
	  );
	  Format.fprintf Format.str_formatter "%s" (Name.simple ct.clt_name);
	  Format.flush_str_formatter ()
	in
	(CodePre (self#pre_of_class_type_kind father s ~def_syntax: "=" ct.clt_kind)) ::
	(
	 if with_link 
	 then [Odoc_info.Latex (" ["^(self#make_ref ct.clt_name)^"]")]
	 else [] 
	) 
      in
      self#latex_of_text t

    (** Return the LaTeX code for the given class element. *)
    method latex_of_class_element class_name class_ele =
      (self#latex_of_text [Newline])^
      (
       match class_ele with
	 Class_attribute att -> self#latex_of_attribute att
       | Class_method met -> self#latex_of_method met
       | Class_comment t ->
	   match t with
	   | [] -> ""
	   | (Title (_,_,_)) :: _ -> self#latex_of_text t
	   | _ -> self#latex_of_text [ Title ((Name.depth class_name) + 2, None, t) ]
      )

    (** Return the LaTeX code for the given module element. *)
    method latex_of_module_element module_name module_ele =
      (self#latex_of_text [Newline])^
      (
       match module_ele with
	 Element_module m -> self#latex_of_module m
       | Element_module_type mt -> self#latex_of_module_type mt
       | Element_included_module im -> self#latex_of_included_module im
       | Element_class c -> self#latex_of_class c
       | Element_class_type ct -> self#latex_of_class_type ct
       | Element_value v -> self#latex_of_value v
       | Element_exception e -> self#latex_of_exception e
       | Element_type t -> self#latex_of_type t
       | Element_module_comment t -> self#latex_of_text t
      )

    (** Generate the LaTeX code for the given list of inherited classes.*)
    method generate_inheritance_info chanout inher_l =
      let f inh =
	match inh.ic_class with
	  None -> (* we can't make the reference *)
	    (Odoc_info.Code inh.ic_name) ::
	    (match inh.ic_text with
	      None -> []
	    | Some t -> Newline :: t
	    )
	| Some _ ->
	    (* we can create the reference *)
	    (Odoc_info.Code inh.ic_name) ::
	    (Odoc_info.Latex (" ["^(self#make_ref inh.ic_name)^"]")) :: 
	    (match inh.ic_text with
	      None -> []
	    | Some t -> Newline :: t
	    )
      in
      let text = [
	Odoc_info.Bold [Odoc_info.Raw Odoc_messages.inherits ];
	Odoc_info.List (List.map f inher_l)
      ] 
      in
      let s = self#latex_of_text text in
      output_string chanout s

    (** Generate the LaTeX code for the inherited classes of the given class. *)
    method generate_class_inheritance_info chanout cl =
      let rec iter_kind k = 
	match k with
	  Class_structure ([], _) ->
	    ()
	| Class_structure (l, _) ->
	    self#generate_inheritance_info chanout l
	| Class_constraint (k, _) ->
	    iter_kind k
	| Class_apply _
	| Class_constr _ ->
	    ()
      in
      iter_kind cl.cl_kind

    (** Generate the LaTeX code for the inherited classes of the given class type. *)
    method generate_class_type_inheritance_info chanout clt =
      match clt.clt_kind with
	Class_signature ([], _) ->
	  ()
      |	Class_signature (l, _) ->
	  self#generate_inheritance_info chanout l
      |	Class_type _ ->
	  ()

    (** Generate the LaTeX code for the given class, in the given out channel. *)
    method generate_for_class chanout c =
      Odoc_info.reset_type_names () ;
      let depth = Name.depth c.cl_name in
      let (first_t, rest_t) = self#first_and_rest_of_info c.cl_info in
      let text = [ Title (depth, None, [ Raw (Odoc_messages.clas^" ") ; Code c.cl_name ] @
			  (match first_t with 
			    [] -> []
			  | t -> (Raw " : ") :: t)) ;
		   Latex (self#make_label c.cl_name) ;
		 ] 
      in
      output_string chanout (self#latex_of_text text);
      output_string chanout ((self#latex_of_class ~with_link: false c)^"\n\n") ;
      let s_name = Name.simple c.cl_name in
      output_string chanout
	(self#latex_of_text [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")]);
      output_string chanout (self#latex_of_text rest_t) ;
      (* parameters *)
      output_string chanout 
	(self#latex_of_text (self#text_of_parameter_list (Name.father c.cl_name) c.cl_parameters));
       
      output_string chanout (self#latex_of_text [ Newline ] );
      output_string chanout ("\\vspace{0.5cm}\n\n");
      self#generate_class_inheritance_info chanout c;

      List.iter 
	(fun ele -> output_string chanout ((self#latex_of_class_element c.cl_name ele)^"\\vspace{0.1cm}\n\n"))
	(Class.class_elements ~trans: false c)

    (** Generate the LaTeX code for the given class type, in the given out channel. *)
    method generate_for_class_type chanout ct =
      Odoc_info.reset_type_names () ;
      let depth = Name.depth ct.clt_name in
      let (first_t, rest_t) = self#first_and_rest_of_info ct.clt_info in
      let text = [ Title (depth, None, [ Raw (Odoc_messages.class_type^" ") ; Code ct.clt_name ] @
			  (match first_t with 
			    [] -> []
			  | t -> (Raw " : ") :: t)) ;
		   Latex (self#make_label ct.clt_name) ;
		 ] 
      in

      output_string chanout (self#latex_of_text text);
      output_string chanout ((self#latex_of_class_type ~with_link: false ct)^"\n\n") ;
      let s_name = Name.simple ct.clt_name in
      output_string chanout
	(self#latex_of_text [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")]);
      output_string chanout ((self#latex_of_text rest_t)) ;
      output_string chanout (self#latex_of_text [ Newline]) ;
      output_string chanout ("\\vspace{0.5cm}\n\n");
      self#generate_class_type_inheritance_info chanout ct;

      List.iter 
	(fun ele -> output_string chanout ((self#latex_of_class_element ct.clt_name ele)^"\\vspace{0.1cm}\n\n"))
	(Class.class_type_elements ~trans: false ct)

    (** Generate the LaTeX code for the given module type, in the given out channel. *)
    method generate_for_module_type chanout mt =
      let depth = Name.depth mt.mt_name in
      let (first_t, rest_t) = self#first_and_rest_of_info mt.mt_info in
      let text = [ Title (depth,  None,
			  [ Raw (Odoc_messages.module_type^" ") ; Code mt.mt_name ] @
			  (match first_t with 
			    [] -> []
			  | t -> (Raw " : ") :: t)) ;
		   Latex (self#make_label mt.mt_name) ;
		 ] 
      in
      output_string chanout (self#latex_of_text text);
      if depth > 1 then
	output_string chanout ((self#latex_of_module_type ~with_link: false mt)^"\n\n");
      let s_name = Name.simple mt.mt_name in
      output_string chanout
	(self#latex_of_text [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")]);
      output_string chanout (self#latex_of_text rest_t) ;
      (* parameters *)
      output_string chanout 
	(self#latex_of_text 
	   (self#text_of_module_parameter_list
	      (Module.module_type_parameters mt)));

      output_string chanout (self#latex_of_text [ Newline ] );
      output_string chanout ("\\vspace{0.5cm}\n\n");
      List.iter 
	(fun ele -> output_string chanout ((self#latex_of_module_element mt.mt_name ele)^"\\vspace{0.1cm}\n\n"))
	(Module.module_type_elements ~trans: false mt);
      (* create sub parts for modules, module types, classes and class types *)
      let rec iter ele =
	match ele with
	  Element_module m -> self#generate_for_module chanout m
	| Element_module_type mt -> self#generate_for_module_type chanout mt
	| Element_class c -> self#generate_for_class chanout c
	| Element_class_type ct -> self#generate_for_class_type chanout ct
	| _ -> ()
      in
      List.iter iter (Module.module_type_elements ~trans: false mt)

    (** Generate the LaTeX code for the given module, in the given out channel. *)
    method generate_for_module chanout m =
      let depth = Name.depth m.m_name in
      let (first_t, rest_t) = self#first_and_rest_of_info m.m_info in
      let text = [ Title (depth, None,
			  [ Raw (Odoc_messages.modul^" ") ; Code m.m_name ] @
			  (match first_t with 
			    [] -> []
			  | t -> (Raw " : ") :: t)) ;
		   Latex (self#make_label m.m_name) ;
		 ] 
      in
      output_string chanout (self#latex_of_text text);
      if depth > 1 then
	output_string chanout ((self#latex_of_module ~with_link: false m)^"\n\n");
      let s_name = Name.simple m.m_name in
      output_string chanout
	(self#latex_of_text [Latex ("\\index{"^(self#label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")]);
      output_string chanout (self#latex_of_text rest_t) ;
      (* parameters *)
      output_string chanout 
	(self#latex_of_text 
	   (self#text_of_module_parameter_list
	      (Module.module_parameters m)));

      output_string chanout (self#latex_of_text [ Newline ]) ;
      output_string chanout ("\\vspace{0.5cm}\n\n");
      List.iter 
	(fun ele -> output_string chanout ((self#latex_of_module_element m.m_name ele)^"\\vspace{0.1cm}\n\n"))
	(Module.module_elements ~trans: false m);
      (* create sub parts for modules, module types, classes and class types *)
      let rec iter ele =
	match ele with
	  Element_module m -> self#generate_for_module chanout m
	| Element_module_type mt -> self#generate_for_module_type chanout mt
	| Element_class c -> self#generate_for_class chanout c
	| Element_class_type ct -> self#generate_for_class_type chanout ct
	| _ -> ()
      in
      List.iter iter (Module.module_elements ~trans: false m)

    (** Return the header of the TeX document. *)
    method latex_header =
      "\\documentclass[11pt]{article} \n"^
      "\\usepackage[latin1]{inputenc} \n"^
      "\\usepackage[T1]{fontenc} \n"^
      "\\usepackage{fullpage} \n"^
      "\\usepackage{url} \n"^
      "\\usepackage{ocamldoc}\n"^
      (
       match !Odoc_args.title with 
	 None -> ""
       | Some s -> "\\title{"^(self#escape s)^"}\n"
      )^
      "\\begin{document}\n"^
      (match !Odoc_args.title with None -> "" | Some _ -> "\\maketitle\n")^
      (if !Odoc_args.with_toc then "\\tableofcontents\n" else "")

    (** Generate the [doc.tex] LaTeX file from a module list. *)
    method generate module_list =
      if !Odoc_args.separate_files then
	(
	 let f m =
	   try
	     let chanout = 
	       open_out ((Filename.concat !Odoc_args.target_dir (Name.simple m.m_name))^".tex")
	     in
	     self#generate_for_module chanout m ;
	     close_out chanout
	   with
	     Failure s
	   | Sys_error s ->
	       prerr_endline s ;
	       incr Odoc_info.errors 
	 in
	 List.iter f module_list
	);
      
      try
	let chanout = open_out (Filename.concat !Odoc_args.target_dir "doc.tex") in
	let _ = if !Odoc_args.with_header then output_string chanout self#latex_header else () in
	List.iter 
	  (fun m -> if !Odoc_args.separate_files then
	    output_string chanout ("\\input{"^((Name.simple m.m_name))^".tex}\n")
	  else
	    self#generate_for_module chanout m
	  ) 
	  module_list ;
	let _ = if !Odoc_args.with_trailer then output_string chanout "\\end{document}" else () in
	close_out chanout
      with
	Failure s
      | Sys_error s ->
	  prerr_endline s ;
	  incr Odoc_info.errors 
  end
