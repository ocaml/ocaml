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

(** Generation of LaTeX documentation. *)

let print_DEBUG s = print_string s ; print_newline ()

open Odoc_info 
open Parameter
open Value
open Type
open Exception
open Class 
open Module

let new_buf () = Buffer.create 1024
let new_fmt () =
  let b = new_buf () in
  let fmt = Format.formatter_of_buffer b in
  (fmt,
   fun () -> 
    Format.pp_print_flush fmt ();
    let s = Buffer.contents b in
    Buffer.reset b;
    s
  )

let bp = Printf.bprintf
let bs = Buffer.add_string

let print_concat b sep f =
  let rec iter = function
      [] -> ()
    | [c] -> f c
    | c :: q ->
	f c;
	bs b sep;
	iter q
  in
  iter

(** Generation of LaTeX code from text structures. *)
class text =
  object (self)
    (** Return latex code to make a sectionning according to the given level,
       and with the given latex code. *)
    method section_style level s =
      try 
        let sec = List.assoc level !Args.latex_titles in
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
    (* The following characters are forbidden in LaTeX \index:
       \ { } $ & # ^ _ % ~ ! " @ | (" to close the double quote)
       The following characters are forbidden in LaTeX \label:
       \ { } $ & # ^ _ % ~
       So we will use characters not forbidden in \index if no_ = true.
    *)
    method label ?(no_=true) name =
      let len = String.length name in
      let buf = Buffer.create len in
      for i = 0 to len - 1 do
        let (s_no_, s) =
	  match name.[i] with
          '_' -> ("-underscore", "_")
        | '~' -> ("-tilde", "~")
	| '%' -> ("-percent", "%")
        | '@' -> ("-at", "\"@")
        | '!' -> ("-bang", "\"!")
        | '|' -> ("-pipe", "\"|")
	| '<' -> ("-lt", "<")
        | '>' -> ("-gt", ">")
        | '^' -> ("-exp", "^")
        | '&' -> ("-ampersand", "&")
        | '+' -> ("-plus", "+")
        | '-' -> ("-minus", "-")
        | '*' -> ("-star", "*")
        | '/' -> ("-slash", "/")
        | '$' -> ("-dollar", "$")
        | '=' -> ("-equal", "=")
        | ':' -> ("-colon", ":")
        | c -> (String.make 1 c, String.make 1 c)
	in
	Buffer.add_string buf (if no_ then s_no_ else s)
      done;
      Buffer.contents buf

    (** Make a correct label from a value name. *)
    method value_label ?no_ name = !Args.latex_value_prefix^(self#label ?no_ name)

    (** Make a correct label from an attribute name. *)
    method attribute_label ?no_ name = !Args.latex_attribute_prefix^(self#label ?no_ name)

    (** Make a correct label from a method name. *)
    method method_label ?no_ name = !Args.latex_method_prefix^(self#label ?no_ name)

    (** Make a correct label from a class name. *)
    method class_label ?no_ name = !Args.latex_class_prefix^(self#label ?no_ name)

    (** Make a correct label from a class type name. *)
    method class_type_label ?no_ name = !Args.latex_class_type_prefix^(self#label ?no_ name)

    (** Make a correct label from a module name. *)
    method module_label ?no_ name = !Args.latex_module_prefix^(self#label ?no_ name)

    (** Make a correct label from a module type name. *)
    method module_type_label ?no_ name = !Args.latex_module_type_prefix^(self#label ?no_ name)

    (** Make a correct label from an exception name. *)
    method exception_label ?no_ name = !Args.latex_exception_prefix^(self#label ?no_ name)

    (** Make a correct label from a type name. *)
    method type_label ?no_ name = !Args.latex_type_prefix^(self#label ?no_ name)

    (** Return latex code for the label of a given label. *)
    method make_label label = "\\label{"^label^"}"

    (** Return latex code for the ref to a given label. *)
    method make_ref label = "\\ref{"^label^"}"

    (** Return the LaTeX code corresponding to the [text] parameter.*)
    method latex_of_text b t = 
      List.iter (self#latex_of_text_element b) t
        
    (** Return the LaTeX code for the [text_element] in parameter. *)
    method latex_of_text_element b te =
      match te with
      | Odoc_info.Raw s -> self#latex_of_Raw b s
      | Odoc_info.Code s -> self#latex_of_Code b s
      | Odoc_info.CodePre s -> self#latex_of_CodePre b s
      | Odoc_info.Verbatim s -> self#latex_of_Verbatim b s
      | Odoc_info.Bold t -> self#latex_of_Bold b t
      | Odoc_info.Italic t -> self#latex_of_Italic b t 
      | Odoc_info.Emphasize t -> self#latex_of_Emphasize b t
      | Odoc_info.Center t -> self#latex_of_Center b t
      | Odoc_info.Left t -> self#latex_of_Left b t
      | Odoc_info.Right t -> self#latex_of_Right b t
      | Odoc_info.List tl -> self#latex_of_List b tl
      | Odoc_info.Enum tl -> self#latex_of_Enum b tl
      | Odoc_info.Newline -> self#latex_of_Newline b
      | Odoc_info.Block t -> self#latex_of_Block b t
      | Odoc_info.Title (n, l_opt, t) -> self#latex_of_Title b n l_opt t
      | Odoc_info.Latex s -> self#latex_of_Latex b s
      | Odoc_info.Link (s, t) -> self#latex_of_Link b s t
      | Odoc_info.Ref (name, ref_opt) -> self#latex_of_Ref b name ref_opt
      | Odoc_info.Superscript t -> self#latex_of_Superscript b t
      | Odoc_info.Subscript t -> self#latex_of_Subscript b t
            
    method latex_of_Raw b s = 
      bs b (self#escape s)

    method latex_of_Code b s = 
      let s2 = self#escape_code s in
      let s3 = Str.global_replace (Str.regexp "\n") ("\\\\\n") s2 in
      bs b ("{\\tt{"^s3^"}}")

    method latex_of_CodePre b s =
      bs b "\\begin{ocamldoccode}\n";
      bs b (self#escape_simple s);
      bs b "\n\\end{ocamldoccode}\n"

    method latex_of_Verbatim b s = 
      bs b "\\begin{verbatim}";
      bs b s;
      bs b "\\end{verbatim}"

    method latex_of_Bold b t =
      bs b "{\\bf ";
      self#latex_of_text b t;
      bs b "}"

    method latex_of_Italic b t = 
      bs b "{\\it ";
      self#latex_of_text b t;
      bs b "}"

    method latex_of_Emphasize b t =
      bs b "{\\em ";
      self#latex_of_text b t;
      bs b "}"

    method latex_of_Center b t =
      bs b "\\begin{center}\n";
      self#latex_of_text b t;
      bs b "\\end{center}\n"

    method latex_of_Left b t =
      bs b "\\begin{flushleft}\n";
      self#latex_of_text b t;
      bs b "\\end{flushleft}\n"

    method latex_of_Right b t =
      bs b "\\begin{flushright}\n";
      self#latex_of_text b t;
      bs b "\\end{flushright}\n"

    method latex_of_List b tl =
      bs b "\\begin{itemize}\n";
      List.iter 
	(fun t -> 
	  bs b "\\item ";
	  self#latex_of_text b t;
	  bs b "\n"
	) 
	tl;
      bs b "\\end{itemize}\n"

    method latex_of_Enum b tl =
      bs b "\\begin{enumerate}\n";
      List.iter 
	(fun t -> 
	  bs b "\\item ";
	  self#latex_of_text b t;
	  bs b "\n"
	) 
	tl;
      bs b "\\end{enumerate}\n"

    method latex_of_Newline b = bs b "\n\n"

    method latex_of_Block b t =
      bs b "\\begin{ocamldocdescription}\n";
      self#latex_of_text b t;
      bs b "\n\\end{ocamldocdescription}\n"

    method latex_of_Title b n label_opt t =
      let b2 = new_buf () in
      self#latex_of_text b2 t;
      let s_title2 = self#section_style n (Buffer.contents b2) in
      bs b s_title2;
      (
       match label_opt with
         None -> ()
       | Some l ->
	   bs b (self#make_label (self#label ~no_: false l))
      )

    method latex_of_Latex b s = bs b s

    method latex_of_Link b s t =
      self#latex_of_text b t ;
      bs b "[\\url{";
      bs b s ;
      bs b "}]"

    method latex_of_Ref b name ref_opt =
      match ref_opt with
        None -> 
          self#latex_of_text_element b
            (Odoc_info.Code (Odoc_info.use_hidden_modules name))
      | Some (RK_section _) -> 
          self#latex_of_text_element b
            (Latex ("["^(self#make_ref (self#label ~no_:false (Name.simple name)))^"]"))
      | Some kind ->
          let f_label = 
            match kind with
              Odoc_info.RK_module -> self#module_label
            | Odoc_info.RK_module_type -> self#module_type_label
            | Odoc_info.RK_class -> self#class_label
            | Odoc_info.RK_class_type -> self#class_type_label
            | Odoc_info.RK_value -> self#value_label
            | Odoc_info.RK_type -> self#type_label
            | Odoc_info.RK_exception -> self#exception_label
            | Odoc_info.RK_attribute -> self#attribute_label
            | Odoc_info.RK_method -> self#method_label
            | Odoc_info.RK_section _ -> assert false
          in
          self#latex_of_text b
            [
              Odoc_info.Code (Odoc_info.use_hidden_modules name) ;
              Latex ("["^(self#make_ref (f_label name))^"]")
            ] 

    method latex_of_Superscript b t = 
      bs b "$^{";
      self#latex_of_text b t;
      bs b "}$"

    method latex_of_Subscript b t = 
      bs b "$_{";
      self#latex_of_text b t;
      bs b "}$"

  end

(** A class used to generate LaTeX code for info structures. *)
class virtual info =
  object (self)
    (** The method used to get LaTeX code from a [text]. *)
    method virtual latex_of_text : Buffer.t -> Odoc_info.text -> unit

    (** The method used to get a [text] from an optionel info structure. *)
    method virtual text_of_info : ?block: bool -> Odoc_info.info option -> Odoc_info.text 

    (** Return LaTeX code for a description, except for the [i_params] field. *)
    method latex_of_info b info_opt = 
      self#latex_of_text b 
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
       In the first sentence, the titles and lists has been removed,
       since it is used in LaTeX titles and would make LaTeX complain
       if we has two nested \section commands.
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
                (Odoc_info.text_no_title_no_list first, rest)

    (** Return LaTeX code for a value. *)
    method latex_of_value b v = 
      Odoc_info.reset_type_names () ;
      self#latex_of_text b
        ((Latex (self#make_label (self#value_label v.val_name))) :: 
         (to_text#text_of_value v))

    (** Return LaTeX code for a class attribute. *)
    method latex_of_attribute b a =
      self#latex_of_text b
        ((Latex (self#make_label (self#attribute_label a.att_value.val_name))) :: 
         (to_text#text_of_attribute a))

    (** Return LaTeX code for a class method. *)
    method latex_of_method b m = 
      self#latex_of_text b
        ((Latex (self#make_label (self#method_label m.met_value.val_name))) :: 
         (to_text#text_of_method m))

    (** Return LaTeX code for the parameters of a type. *)
    method latex_of_type_params b m_name t =
      let print_one (p, co, cn) =
	bs b (Odoc_info.string_of_variance t (co,cn));
	bs b (self#normal_type m_name p)
      in
      match t.ty_parameters with
        [] -> ()
      | [(p,co,cn)] -> print_one (p, co, cn)
      | l -> 
	  bs b "(";
	  print_concat b ", " print_one t.ty_parameters;
	  bs b ")"

    (** TODO: regarder si les format servent ici.
       Return LaTeX code for a type. *)
    method latex_of_type b t =
      let s_name = Name.simple t.ty_name in
      let btmp = new_buf () in
      let text = 
	let (fmt, flush) = new_fmt () in
        Odoc_info.reset_type_names () ;
        let mod_name = Name.father t.ty_name in
        Format.fprintf fmt "@[<hov 2>type ";
	let s_param_types =
	  self#latex_of_type_params btmp mod_name t;
	  (match t.ty_parameters with [] -> () | _ -> bs btmp " ");
	  let s = Buffer.contents btmp in
	  Buffer.reset btmp;
	  s
	in
        Format.fprintf fmt "%s %s" s_param_types s_name;
	(
         match t.ty_manifest with
           None -> ()
         | Some typ -> 
             Format.fprintf fmt " = %s"
	       (self#normal_type mod_name typ)
	);
        let s_type3 = 
          Format.fprintf fmt
            " %s"
            (
	     match t.ty_kind with
               Type_abstract -> ""
             | Type_variant (_, priv) -> "="^(if priv then " private" else "")
             | Type_record (_, priv) -> "= "^(if priv then "private " else "")^"{" 
	    ) ;
          flush ()
        in
        
        let defs = 
          match t.ty_kind with
            Type_abstract -> []
          | Type_variant (l, _) ->
              (List.flatten
               (List.map
                  (fun constr ->
                    let s_cons = 
                      Format.fprintf fmt
                        "@[<hov 6>  | %s"
                        constr.vc_name;
                      (
		       match constr.vc_args with
                         [] -> ()
                       | l -> 
                           Format.fprintf fmt " %s@ %s" 
			     "of"
                             (self#normal_type_list ~par: false mod_name " * " l)
		      );
		      flush ()
                    in
                    [ CodePre s_cons ] @
                    (match constr.vc_text with
                      None -> []
                    | Some t -> 
			let s = 
			  bs btmp "\\begin{ocamldoccomment}\n";
			  self#latex_of_text btmp t;
			  bs btmp "\n\\end{ocamldoccomment}\n";
			  let s = Buffer.contents btmp in
			  Buffer.reset btmp;
			  s
			in
                        [ Latex s]
                    )
                  )
                  l
               )
              )
          | Type_record (l, _) ->
              (List.flatten
                 (List.map
                    (fun r ->
                      let s_field = 
                        Format.fprintf fmt
                          "@[<hov 6>  %s%s :@ %s ;"
                          (if r.rf_mutable then "mutable " else "")
                          r.rf_name
                          (self#normal_type mod_name r.rf_type);
			flush ()
                      in
                      [ CodePre s_field ] @
                      (match r.rf_text with
                        None -> []
                      | Some t -> 
                          let s = 
			  bs btmp "\\begin{ocamldoccomment}\n";
			  self#latex_of_text btmp t;
			  bs btmp "\n\\end{ocamldoccomment}\n";
			  let s = Buffer.contents btmp in
			  Buffer.reset btmp;
			  s
			in
                        [ Latex s]
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
        [Latex ("\\index{"^(self#type_label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")] @
        (self#text_of_info t.ty_info)
      in
      self#latex_of_text b
        ((Latex (self#make_label (self#type_label t.ty_name))) :: text)

    (** Return LaTeX code for an exception. *)
    method latex_of_exception b e =
      Odoc_info.reset_type_names () ;
      self#latex_of_text b
        ((Latex (self#make_label (self#exception_label e.ex_name))) :: 
         (to_text#text_of_exception e))

    (** Return the LaTeX code for the given module. 
       @param for_detail indicate if we must print the type ([false]) or just ["sig"] ([true]).*)
    method latex_of_module b ?(for_detail=false) ?(with_link=true) m =
      let father = Name.father m.m_name in
      let b2 = new_buf () in
      let t = 
        bs b2 "module ";
	bs b2 (Name.simple m.m_name);
        bs b2 " : ";
        bs b2 
	  (
	   if for_detail 
	   then "sig" 
	   else (self#normal_module_type father m.m_type)
	  );
          
        (CodePre (Buffer.contents b2)) ::
        (
         if with_link 
         then [Odoc_info.Latex ("\\\n["^(self#make_ref (self#module_label m.m_name))^"]")]
         else [] 
        )
      in
      self#latex_of_text b t

    (** Return the LaTeX code for the given module type. 
       @param for_detail indicate if we must print the type ([false]) or just ["sig"] ([true]).*)
    method latex_of_module_type b ?(for_detail=false) ?(with_link=true) mt =
      let b2 = new_buf () in
      let father = Name.father mt.mt_name in
      let t = 
        bs b2 "module type ";
	bs b2 (Name.simple mt.mt_name);
        (match mt.mt_type with
          None -> ()
        | Some mtyp -> 
            bs b2 " = ";
            bs b2 
	      (
	       if for_detail 
	       then "sig" 
	       else (self#normal_module_type father mtyp)
	      )
        );

        (CodePre (Buffer.contents b2)) ::
        (
         if with_link 
         then [Odoc_info.Latex ("\\\n["^(self#make_ref (self#module_type_label mt.mt_name))^"]")]
         else [] 
        )
      in
      self#latex_of_text b t

    (** Return the LaTeX code for the given included module. *)
    method latex_of_included_module b im =
      self#latex_of_text b
	((Code "include ") ::
         (Code 
            (match im.im_module with
              None -> im.im_name
            | Some (Mod m) -> m.m_name
            | Some (Modtype mt) -> mt.mt_name)
	 ) ::
	 (self#text_of_info im.im_info)
        )

    (** Return the LaTeX code for the given class. 
       @param for_detail indicate if we must print the type ([false]) or just ["object"] ([true]).*)
    method latex_of_class b ?(for_detail=false) ?(with_link=true) c =
      Odoc_info.reset_type_names () ;
      let b2 = new_buf () in
      let father = Name.father c.cl_name in
      let t = 
        bs b2 "class ";
	if c.cl_virtual then bs b2 "virtual " ;
        (
         match c.cl_type_parameters with
           [] -> ()
         | l -> 
             let s1 = self#normal_class_type_param_list father l in
             bs b2 s1;
	     bs b2 " "
        );
	bs b2 (Name.simple c.cl_name);
        bs b2 " : " ;
        bs b2
	  (
	   if for_detail then
	     "object"
	   else
	     self#normal_class_type father c.cl_type
	  );

        (CodePre (Buffer.contents b2)) ::
        (
         if with_link 
         then [Odoc_info.Latex (" ["^(self#make_ref (self#class_label c.cl_name))^"]")]
         else [] 
        ) 
      in
      self#latex_of_text b t

    (** Return the LaTeX code for the given class type. 
       @param for_detail indicate if we must print the type ([false]) or just ["object"] ([true]).*)
    method latex_of_class_type b ?(for_detail=false) ?(with_link=true) ct =
      Odoc_info.reset_type_names () ;
      let b2 = new_buf () in
      let father = Name.father ct.clt_name in
      let t = 
        bs b2 "class type ";
        if ct.clt_virtual then bs b2 "virtual " ;
        (
         match ct.clt_type_parameters with
           [] -> ()
         | l -> 
             let s1 = self#normal_class_type_param_list father l in
	     bs b2 s1;
	     bs b2 " "
        );
        bs b2 (Name.simple ct.clt_name);
	bs b2 " = " ;
	bs b2	
          (if for_detail then
	    "object"
	  else
	    self#normal_class_type father ct.clt_type
	  );

        (CodePre (Buffer.contents b2)) ::
        (
         if with_link 
         then [Odoc_info.Latex (" ["^(self#make_ref (self#class_type_label ct.clt_name))^"]")]
         else [] 
        ) 
      in
      self#latex_of_text b t

    (** Return the LaTeX code for the given class element. *)
    method latex_of_class_element b class_name class_ele =
      self#latex_of_text b [Newline];
      match class_ele with
        Class_attribute att -> self#latex_of_attribute b att
      | Class_method met -> self#latex_of_method b met
      | Class_comment t ->
          match t with
          | [] -> ()
          | (Title (_,_,_)) :: _ -> self#latex_of_text b t
          | _ -> self#latex_of_text b [ Title ((Name.depth class_name) + 2, None, t) ]

    (** Return the LaTeX code for the given module element. *)
    method latex_of_module_element b module_name module_ele =
      self#latex_of_text b [Newline];
      match module_ele with
        Element_module m -> self#latex_of_module b m
      | Element_module_type mt -> self#latex_of_module_type b mt
      | Element_included_module im -> self#latex_of_included_module b im
      | Element_class c -> self#latex_of_class b c
      | Element_class_type ct -> self#latex_of_class_type b ct
      | Element_value v -> self#latex_of_value b v
      | Element_exception e -> self#latex_of_exception b e
      | Element_type t -> self#latex_of_type b t
      | Element_module_comment t -> self#latex_of_text b t

    (** Generate the LaTeX code for the given list of inherited classes.*)
    method generate_inheritance_info b inher_l =
      let f inh =
        match inh.ic_class with
          None -> (* we can't make the reference *)
            (Odoc_info.Code inh.ic_name) ::
            (match inh.ic_text with
              None -> []
            | Some t -> Newline :: t
            )
        | Some cct ->
            let label = 
              match cct with
                Cl _ -> self#class_label inh.ic_name
              | Cltype _ -> self#class_type_label inh.ic_name
            in
            (* we can create the reference *)
            (Odoc_info.Code inh.ic_name) ::
            (Odoc_info.Latex (" ["^(self#make_ref label)^"]")) :: 
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
      self#latex_of_text b text 

    (** Generate the LaTeX code for the inherited classes of the given class. *)
    method generate_class_inheritance_info b cl =
      let rec iter_kind k = 
        match k with
          Class_structure ([], _) ->
            ()
        | Class_structure (l, _) ->
            self#generate_inheritance_info b l
        | Class_constraint (k, _) ->
            iter_kind k
        | Class_apply _
        | Class_constr _ ->
            ()
      in
      iter_kind cl.cl_kind

    (** Generate the LaTeX code for the inherited classes of the given class type. *)
    method generate_class_type_inheritance_info b clt =
      match clt.clt_kind with
        Class_signature ([], _) ->
          ()
      | Class_signature (l, _) ->
          self#generate_inheritance_info b l
      | Class_type _ ->
          ()

    (** Generate the LaTeX code for the given class, in the given buffer. *)
    method generate_for_class b c =
      Odoc_info.reset_type_names () ;
      let depth = Name.depth c.cl_name in
      let (first_t, rest_t) = self#first_and_rest_of_info c.cl_info in
      let text = [ Title (depth, None, [ Raw (Odoc_messages.clas^" ") ; Code c.cl_name ] @
                          (match first_t with 
                            [] -> []
                          | t -> (Raw " : ") :: t)) ;
                   Latex (self#make_label (self#class_label c.cl_name)) ;
                 ] 
      in
      self#latex_of_text b text;
      self#latex_of_class b ~for_detail: true ~with_link: false c;
      bs b "\n\n" ;
      let s_name = Name.simple c.cl_name in
      self#latex_of_text b
	[Latex ("\\index{"^(self#class_label s_name)^"@\\verb`"^(self#label ~no_:false s_name)^"`}\n")];
      self#latex_of_text b rest_t;

      (* parameters *)
      self#latex_of_text b (self#text_of_parameter_list (Name.father c.cl_name) c.cl_parameters);
       
      self#latex_of_text b [ Newline ];
      bs b "\\ocamldocvspace{0.5cm}\n\n";
      self#generate_class_inheritance_info b c;

      List.iter 
        (fun ele -> 
	  self#latex_of_class_element b c.cl_name ele;
	  bs b "\n\n"
	)
        (Class.class_elements ~trans: false c);

      self#latex_of_text b [ CodePre "end"]

    (** Generate the LaTeX code for the given class type, in the given buffer. *)
    method generate_for_class_type b ct =
      Odoc_info.reset_type_names () ;
      let depth = Name.depth ct.clt_name in
      let (first_t, rest_t) = self#first_and_rest_of_info ct.clt_info in
      let text = [ Title (depth, None, [ Raw (Odoc_messages.class_type^" ") ; Code ct.clt_name ] @
                          (match first_t with 
                            [] -> []
                          | t -> (Raw " : ") :: t)) ;
                   Latex (self#make_label (self#class_type_label ct.clt_name)) ;
                 ] 
      in

      self#latex_of_text b text;
      self#latex_of_class_type b ~for_detail: true ~with_link: false ct;
      bs b "\n\n" ;
      let s_name = Name.simple ct.clt_name in
      self#latex_of_text b 
	[Latex ("\\index{"^(self#class_type_label s_name)^"@\\verb`"^
		(self#label ~no_:false s_name)^"`}\n"
	       )
	];
      self#latex_of_text b rest_t;
      self#latex_of_text b [Newline];
      bs b "\\ocamldocvspace{0.5cm}\n\n";
      self#generate_class_type_inheritance_info b ct;

      List.iter 
        (fun ele -> 
	  self#latex_of_class_element b ct.clt_name ele;
	  bs b "\n\n"
	)
        (Class.class_type_elements ~trans: false ct);

      self#latex_of_text b [ CodePre "end"]

    (** Generate the LaTeX code for the given module type, in the given buffer. *)
    method generate_for_module_type b mt =
      let depth = Name.depth mt.mt_name in
      let (first_t, rest_t) = self#first_and_rest_of_info mt.mt_info in
      let text = [ Title (depth,  None,
                          [ Raw (Odoc_messages.module_type^" ") ; Code mt.mt_name ] @
                          (match first_t with 
                            [] -> []
                          | t -> (Raw " : ") :: t)) ;
                   Latex (self#make_label (self#module_type_label mt.mt_name)) ;
                 ] 
      in
      self#latex_of_text b text;
      if depth > 1 then
        (
	 self#latex_of_module_type b ~for_detail: true ~with_link: false mt;
	 bs b "\n\n"
	);
      let s_name = Name.simple mt.mt_name in
      self#latex_of_text b 
	[Latex ("\\index{"^(self#module_type_label s_name)^"@\\verb`"^
		(self#label ~no_:false s_name)^"`}\n"
	       )
	];
      self#latex_of_text b rest_t ;
      (* parameters *)
      self#latex_of_text b
        (self#text_of_module_parameter_list
           (Module.module_type_parameters mt));

      self#latex_of_text b [ Newline ];
      bs b "\\ocamldocvspace{0.5cm}\n\n";
      List.iter 
        (fun ele -> 
	  self#latex_of_module_element b mt.mt_name ele;
	  bs b "\n\n"
	)
        (Module.module_type_elements ~trans: false mt);

      if depth > 1 then 
	self#latex_of_text b [ CodePre "end"];

      (* create sub parts for modules, module types, classes and class types *)
      let rec iter ele =
        match ele with
          Element_module m -> self#generate_for_module b m
        | Element_module_type mt -> self#generate_for_module_type b mt
        | Element_class c -> self#generate_for_class b c
        | Element_class_type ct -> self#generate_for_class_type b ct
        | _ -> ()
      in
      List.iter iter (Module.module_type_elements ~trans: false mt)

    (** Generate the LaTeX code for the given module, in the given buffer. *)
    method generate_for_module b m =
      let depth = Name.depth m.m_name in
      let (first_t, rest_t) = self#first_and_rest_of_info m.m_info in
      let text = [ Title (depth, None,
                          [ Raw (Odoc_messages.modul^" ") ; Code m.m_name ] @
                          (match first_t with 
                            [] -> []
                          | t -> (Raw " : ") :: t)) ;
                   Latex (self#make_label (self#module_label m.m_name)) ;
                 ] 
      in
      self#latex_of_text b text;
      if depth > 1 then
        (
	 self#latex_of_module b ~for_detail:true ~with_link: false m;
	 bs b "\n\n"
	);
      let s_name = Name.simple m.m_name in
      self#latex_of_text b 
	[Latex ("\\index{"^(self#module_label s_name)^"@\\verb`"^
		(self#label ~no_:false s_name)^"`}\n"
	       )
	];
      self#latex_of_text b rest_t ;
      (* parameters *)
      self#latex_of_text b
        (self#text_of_module_parameter_list
           (Module.module_parameters m));

      self#latex_of_text b [ Newline ] ;
      bs b "\\ocamldocvspace{0.5cm}\n\n";
      List.iter 
        (fun ele -> 
	  self#latex_of_module_element b m.m_name ele;
	  bs b "\n\n"
	)
        (Module.module_elements ~trans: false m);

      if depth > 1 then 
	self#latex_of_text b [ CodePre "end"];

      (* create sub parts for modules, module types, classes and class types *)
      let rec iter ele =
        match ele with
          Element_module m -> self#generate_for_module b m
        | Element_module_type mt -> self#generate_for_module_type b mt
        | Element_class c -> self#generate_for_class b c
        | Element_class_type ct -> self#generate_for_class_type b ct
        | _ -> ()
      in
      List.iter iter (Module.module_elements ~trans: false m)

    (** Return the header of the TeX document. *)
    method latex_header b =
      bs b "\\documentclass[11pt]{article} \n";
      bs b "\\usepackage[latin1]{inputenc} \n";
      bs b "\\usepackage[T1]{fontenc} \n";
      bs b "\\usepackage{fullpage} \n";
      bs b "\\usepackage{url} \n";
      bs b "\\usepackage{ocamldoc}\n";
      (
       match !Args.title with 
         None -> ()
       | Some s -> 
	   bs b "\\title{";
	   bs b (self#escape s);
	   bs b "}\n"
      );
      bs b "\\begin{document}\n";
      (match !Args.title with 
	None -> () | 
	Some _ -> bs b "\\maketitle\n"
      );
      if !Args.with_toc then bs b "\\tableofcontents\n";
      (
       let info = Odoc_info.apply_opt
	   Odoc_info.info_of_comment_file !Odoc_info.Args.intro_file 
       in
       (match info with None -> () | Some _ -> bs b "\\vspace{0.2cm}");
       self#latex_of_info b info;
       (match info with None -> () | Some _ -> bs b "\n\n")
      )
	

    (** Generate the LaTeX style file, if it does not exists. *)
    method generate_style_file =
      try
	let dir = Filename.dirname !Args.out_file in
	let file = Filename.concat dir "ocamldoc.sty" in
	if Sys.file_exists file then
	  Odoc_info.verbose (Odoc_messages.file_exists_dont_generate file)
	else
	  (
	   let chanout = open_out file in
	   output_string chanout Odoc_latex_style.content ;
	   flush chanout ;
	   close_out chanout;
	   Odoc_info.verbose (Odoc_messages.file_generated file)
	  )
      with
        Sys_error s ->
          prerr_endline s ;
          incr Odoc_info.errors ;

    (** Generate the LaTeX file from a module list, in the {!Odoc_info.Args.out_file} file. *)
    method generate module_list =
      self#generate_style_file ;
      if !Args.separate_files then
        (
         let f m =
           try
             let chanout = 
               open_out ((Filename.concat !Args.target_dir (Name.simple m.m_name))^".tex")
             in
	     let b = new_buf () in
             self#generate_for_module b m ;
	     Buffer.output_buffer chanout b;
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
        let chanout = open_out !Args.out_file in
	let b = new_buf () in
        if !Args.with_header then self#latex_header b;
        List.iter 
          (fun m -> 
	    if !Args.separate_files then
              bs b ("\\input{"^((Name.simple m.m_name))^".tex}\n")
            else
              self#generate_for_module b m
          ) 
          module_list ;
        if !Args.with_trailer then bs b "\\end{document}";
	Buffer.output_buffer chanout b;
        close_out chanout
      with
        Failure s
      | Sys_error s ->
          prerr_endline s ;
          incr Odoc_info.errors 
  end

(* eof $Id$ *)
