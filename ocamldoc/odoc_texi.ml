(***********************************************************************)
(*                           OCamldoc                                  *)
(*                                                                     *)
(*      Olivier Andrieu, basé sur du code de Maxence Guesdon           *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(***********************************************************************)

(* $Id$ *)

(** Generation of Texinfo documentation. *)

open Odoc_info 
open Parameter
open Value
open Type
open Exception
open Class 
open Module

(** {2 Command-line options} *)

let with_index = Odoc_args.with_index
let esc_8bits = Odoc_args.esc_8bits

(** {2 Some small helper functions} *)

let puts_nl chan s = 
  output_string chan s ;
  output_char chan '\n'
let puts chan s = 
  output_string chan s
let nl chan = 
  output_char chan '\n'

let is = function
  | None -> false
  | Some _ -> true

let pad_to n s = 
  let len = String.length s in
  if len < n
  then
    let s' = String.make n ' ' in
    String.blit s 0 s' 0 len ; s'
  else s

let indent nb_sp s = 
  let c = ref 0 in
  let len = pred (String.length s) in
  for i = 0 to len do if s.[i] = '\n' then incr c done ;
  let s' = String.make (succ len + (succ !c) * nb_sp ) ' ' in
  c := nb_sp ;
  for i = 0 to len do 
    s'.[!c] <- s.[i] ; 
    if s.[i] = '\n' then c := !c + nb_sp ;
    incr c
  done ;
  s'

type subparts = [ 
  | `Module of Odoc_info.Module.t_module
  | `Module_type of Odoc_info.Module.t_module_type
  | `Class of Odoc_info.Class.t_class
  | `Class_type of Odoc_info.Class.t_class_type
  | `Blank
  | `Comment of string
  | `Texi of string
  | `Index of string
] list

let nothing = Verbatim ""

let module_subparts = 
  let rec iter acc = function
    | [] -> List.rev acc
    (* skip aliases *)	  
    | Element_module { m_kind = Module_alias _ } :: n -> 
	iter acc n
    | Element_module_type { mt_kind = Some (Module_type_alias _) } :: n -> 
	iter acc n
    (* keep modules, module types, classes and class types *)
    | Element_module m :: n -> 
	iter (`Module m :: acc) n 
    | Element_module_type mt :: n -> 
	iter (`Module_type mt :: acc) n
    | Element_class c :: n -> 
	iter (`Class c :: acc) n
    | Element_class_type ct :: n -> 
	iter (`Class_type ct :: acc) n
    (* forget the rest *)
    | _ :: n -> iter acc n
  in
  iter []

type indices = [
  | `Type       
  | `Exception  
  | `Value      
  | `Class_att  
  | `Method     
  | `Class      
  | `Class_type 
  | `Module     
  | `Module_type
]

let indices = function
  | `Type        -> "ty"
  | `Exception   -> "ex"
  | `Value       -> "va"
  | `Class_att   -> "ca"
  | `Method      -> "me"
  | `Class       -> "cl"
  | `Class_type  -> "ct"
  | `Module      -> "mo"
  | `Module_type -> "mt"

let indices_names = [
  "Types", "ty" ;
  "Exceptions", "ex" ;
  "Values", "va" ;
  "Class attributes", "ca" ;
  "Methods", "me" ;
  "Classes", "cl" ;
  "Class types", "ct" ;
  "Modules", "mo" ;
  "Module types", "mt" ; ]



(** Module for generating various Texinfo things (menus, xrefs, ...) *) 
module Texi = 
struct
  (** Associations of strings to subsitute in Texinfo code. *)
  let subst_strings = [
    (Str.regexp "@", "@@") ;
    (Str.regexp "{", "@{") ;
    (Str.regexp "}", "@}") ;
    (Str.regexp "\\.\\.\\.", "@dots{}") ;
  ] @
    (if !esc_8bits 
    then [
    (Str.regexp "à", "@`a") ;
    (Str.regexp "â", "@^a") ;
    (Str.regexp "é", "@'e") ;
    (Str.regexp "è", "@`e") ; 
    (Str.regexp "ê", "@^e") ;
    (Str.regexp "ë", "@\"e") ;
    (Str.regexp "ç", "@,{c}") ;
    (Str.regexp "ô", "@^o") ;
    (Str.regexp "ö", "@\"o") ;
    (Str.regexp "î", "@^i") ;
    (Str.regexp "ï", "@\"i") ;
    (Str.regexp "ù", "@`u") ;
    (Str.regexp "û", "@^u") ;
    (Str.regexp "æ", "@ae{}" ) ;
    (Str.regexp "Æ", "@AE{}" ) ;
    (Str.regexp "ß", "@ss{}" ) ;
    (Str.regexp "©", "@copyright{}" ) ;
    ]
    else [])

  (** Escape the strings which would clash with Texinfo syntax. *)
  let escape s = 
    List.fold_left
      (fun acc (p, r) -> Str.global_replace p r acc)
      s subst_strings

  (** Removes dots (no good for a node name). *)
  let fix_nodename s = 
    Str.global_replace (Str.regexp "\\.") "/" (escape s)

  (** Generates a Texinfo menu. *)
  let generate_menu chan subpart_list = 
    if subpart_list <> []
    then begin
      let menu_line part_qual name = 
	let sname = Name.simple name in
	if sname = name
	then (
	  puts chan (pad_to 35 
		       ("* " ^ sname ^ ":: ")) ;
	  puts_nl chan part_qual )
	else (
	  puts chan (pad_to 35 
		       ("* " ^ sname ^ ": " ^ (fix_nodename name) ^ ". " )) ;
	  puts_nl chan part_qual )
      in
      puts_nl chan "@menu" ;
      List.iter
	(function
	| `Module { m_name = name } -> 
	    menu_line Odoc_messages.modul name
	| `Module_type { mt_name = name } ->
	    menu_line Odoc_messages.module_type name
	| `Class { cl_name = name } ->
	    menu_line Odoc_messages.clas name
	| `Class_type { clt_name = name } ->
	    menu_line Odoc_messages.class_type name 
	| `Blank -> nl chan 
	| `Comment c -> puts_nl chan (escape c)
	| `Texi t -> puts_nl chan t
	| `Index ind -> Printf.fprintf chan "* %s::\n" ind)
      subpart_list ;
    puts_nl chan "@end menu"
  end

  let xref ?xname name =
    "@xref{" ^ (fix_nodename name) ^ 
    (match xname with | None -> "" | Some s -> "," ^ s) ^ 
    "}."

  let ifinfo s = 
    String.concat "\n"
      [ "@ifinfo" ; s ; "@end ifinfo" ; "" ]
end





(** {2 Generation of Texinfo code from text structures} *)
class text = 
  object(self)

  (** Associations between a title number and functions to get texinfo code. *)
    val titles = [
      1, "@chapter " ;
      2, "@section " ;
      3, "@subsection " ;
      4, "@subsubsection " ;
    ]

    val fallback_title = 
      "@unnumberedsubsubsec "

    val headings = [
      1, "@majorheading " ;
      2, "@heading " ;
      3, "@subheading " ;
      4, "@subsubheading " ;
    ] 
  
    val fallback_heading = 
      "@subsubheading " 

    method escape = 
      Texi.escape 

    method label ?(no_ : bool option) (_ : string) = 
      failwith "gni" ; ""

    (** Return the Texinfo code corresponding to the [text] parameter.*)
    method texi_of_text t =
      String.concat ""
	(List.map self#texi_of_text_element t)
      
    (** Return the Texinfo code for the [text_element] in parameter. *)
    method texi_of_text_element = function
      | Verbatim s | Latex s -> self#texi_of_Verbatim s
      | Raw s -> self#texi_of_Raw s
      | Code s -> self#texi_of_Code s
      | CodePre s -> self#texi_of_CodePre s
      | Bold t -> self#texi_of_Bold t
      | Italic t -> self#texi_of_Italic t
      | Emphasize t -> self#texi_of_Emphasize t
      | Center t -> self#texi_of_Center t
      | Left t -> self#texi_of_Left t
      | Right t -> self#texi_of_Right t
      | List tl -> self#texi_of_List tl
      | Enum tl -> self#texi_of_Enum tl
      | Newline -> self#texi_of_Newline
      | Block t -> self#texi_of_Block t
      | Title (n, _, t) -> self#texi_of_Title n t
      | Link (s, t) -> self#texi_of_Link s t
      | Ref (name, kind) ->self#texi_of_Ref name kind
      | Superscript t -> self#texi_of_Superscript t
      | Subscript t -> self#texi_of_Subscript t

    method texi_of_Verbatim s = s
    method texi_of_Raw s = self#escape s
    method texi_of_Code s = "@code{" ^ (self#escape s) ^ "}"
    method texi_of_CodePre s = 
      String.concat "\n"
	[ "" ;  "@example" ; self#escape s ; "@end example" ; "" ]
    method texi_of_Bold t = "@strong{" ^ (self#texi_of_text t) ^ "}"
    method texi_of_Italic t = "@i{" ^ (self#texi_of_text t) ^ "}"
    method texi_of_Emphasize t = "@emph{" ^ (self#texi_of_text t) ^ "}"
    method texi_of_Center t = 
      let sl = Str.split (Str.regexp "\n") (self#texi_of_text t) in
      String.concat ""
	((List.map (fun s -> "\n@center "^s) sl) @ [ "\n" ])
    method texi_of_Left t =
      String.concat "\n" 
	[ "" ; "@flushleft" ; self#texi_of_text t ; "@end flushleft" ; "" ]
    method texi_of_Right t = 
      String.concat "\n" 
	[ "" ; "@flushright" ; self#texi_of_text t ; "@end flushright"; "" ]
    method texi_of_List tl = 
      String.concat "\n"
	( [ "" ; "@itemize" ] @ 
	  (List.map (fun t -> "@item\n" ^ (self#texi_of_text t)) tl) @
	  [ "@end itemize"; "" ] )
    method texi_of_Enum tl = 
      String.concat "\n"
	( [ "" ; "@enumerate" ] @ 
	  (List.map (fun t -> "@item\n" ^ (self#texi_of_text t)) tl) @
	  [ "@end enumerate"; "" ] )
    method texi_of_Newline = "\n"
    method texi_of_Block t =
      String.concat "\n"
	[ "@format" ; self#texi_of_text t ; "@end format" ; "" ]
    method texi_of_Title n t =
      let t_begin = 
	try List.assoc n titles 
	with Not_found -> fallback_title in
      t_begin ^ (self#texi_of_text t) ^ "\n"
    method texi_of_Link s t =
      String.concat ""
	[ "@uref{" ; s ;  "," ; self#texi_of_text t ; "}" ]
    method texi_of_Ref name kind =
      let xname = 
	match kind with
	| Some RK_module -> 
	    Odoc_messages.modul ^ " " ^ (Name.simple name)
	| Some RK_module_type -> 
	    Odoc_messages.module_type ^ " " ^ (Name.simple name)
	| Some RK_class -> 
	    Odoc_messages.clas ^ " " ^ (Name.simple name)
	| Some RK_class_type -> 
	    Odoc_messages.class_type ^ " " ^ (Name.simple name)
	| _ -> ""
      in
      if xname = "" then self#escape name else Texi.xref ~xname name
    method texi_of_Superscript t =
      "^@{" ^ (self#texi_of_text t) ^ "@}"
    method texi_of_Subscript t =
      "_@{" ^ (self#texi_of_text t) ^ "@}"

    method heading n t =
      let f = 
	try List.assoc n headings
	with Not_found -> fallback_heading
      in
      f ^ (self#texi_of_text t) ^ "\n"

    method fixedblock t = 
      Block ( ( Verbatim "@t{" :: t ) @ [ Verbatim "}" ] )

  end



(** This class is used to create objects which can generate a simple
    Texinfo documentation. *)
class texi =
  object (self)
    inherit text as to_texi
    inherit Odoc_to_text.to_text as to_text

    (** quelques machins utiles qui se trouvent bien ici *)

    val maxdepth = 4

    val bullet = Verbatim " @bullet{} "
    val minus  = Verbatim " @minus{} "
    val linebreak =  Verbatim "@*\n"

    method node depth name = 
      if depth <= maxdepth 
      then Verbatim ("@node " ^ (Texi.fix_nodename name) ^ ",\n")
      else nothing

    method index (ind : indices) ent = 
      Verbatim 
	(if !with_index
	then (String.concat "" 
		[ "@" ; indices ind ; "index " ; 
		  Texi.escape (Name.simple ent) ; "\n" ])
	else "")
      

    (** grosse bidouille infame *)
    method private fix_linebreaks t = 
      let re = Str.regexp "\n[ \t]*" in
      List.map 
	(function
	  | Newline -> Raw "\n"
	  | Raw s -> Raw (Str.global_replace re "\n" s)
	  | List tel | Enum tel -> List (List.map self#fix_linebreaks tel)
	  | te -> te) t

    (** autre bidouille du meme acabit. Elles ont tellement honte
        qu'elle se cachent. *)
    method private soft_fix_linebreaks ind t = 
      let re = Str.regexp "\n[ \t]*" in
      let rep = String.make (succ ind) ' ' in
      rep.[0] <- '\n' ;
      List.map
	(function
	  | Raw s -> Raw (Str.global_replace re rep s)
	  | te -> te) t

    method text_of_desc = function
      | None -> []
      | Some [ Raw "" ] -> []
      | Some t -> (self#fix_linebreaks t) @ [ Newline ]

    (** ça je l'ai rajouté *)
    method text_of_sees_opt = function
      |	[] -> []
      |	(See_url s, t) :: n ->
	  [ Bold [ Raw Odoc_messages.see_also ] ;
	    Raw " " ; Link (s, t) ; Newline ] @ (self#text_of_sees_opt n)
      |	(See_file s, t) :: n
      |	(See_doc s, t) :: n ->
	  [ Bold [ Raw Odoc_messages.see_also ] ;
	    Raw " " ; Raw s ] @ t @ [ Newline ] @ (self#text_of_sees_opt n)

    (** ça aussi je l'ai rajouté *)
    method text_of_params = function
      |	[] -> []
      |	(s, t) :: n ->
	  [ Bold [ Raw Odoc_messages.parameters ] ;
	    Raw " " ; Raw s ; Raw ": " ] @ t @
	  [ Newline ] @ (self#text_of_params n)

    (** plouf plouf *)
    method text_of_return_opt = function
      | None -> []
      | Some t -> 
	  (Bold [ Raw Odoc_messages.returns ]) 
	  :: (Raw " ") :: t @ [ Newline ]

    (** redéfinition de text_of_info de Odoc_to_text.to_text
        dans le seul but d'introduire la précédente bidouille 
        @raise Exit pas d'exceptions, aucunes ! *)
    method text_of_info ?(block=false) = function
      | None -> []
      | Some info -> 
	  let t = 
	    List.flatten [
	    ( match info.i_deprecated with
	    | None -> []
	    | Some t -> 
		(Raw (Odoc_messages.deprecated ^ " ")) :: 
		(self#fix_linebreaks t) 
		@ [ Newline ; Newline ] ) ;
	    self#text_of_desc info.i_desc ;
	    self#text_of_author_list info.i_authors ;
	    self#text_of_version_opt info.i_version ;
	    self#text_of_sees_opt info.i_sees ;
	    self#text_of_since_opt info.i_since ;
	    self#text_of_params info.i_params ;
	    self#text_of_raised_exceptions info.i_raised_exceptions ;
	    self#text_of_return_opt info.i_return_value ;
	  ] in
	  if block 
	  then [ Block t ] 
	  else (t @ [ Newline ] )

    method texi_of_info i =
      self#texi_of_text (self#text_of_info i)

    method text_el_of_type_expr m_name typ = 
      Raw (indent 5
	     (self#relative_idents m_name 
		(Odoc_info.string_of_type_expr typ)))

    (** je l'ai redéfini pcq to_text mettait un [Code] 
        et moi je veux un [Raw] sinon c'est moche *)
    method text_of_short_type_expr m_name typ =
      [ Raw (self#normal_type m_name typ) ]

    (** Return Texinfo code for a value. *)
    method texi_of_value v = 
      Odoc_info.reset_type_names () ;
      let t = [ self#fixedblock 
		  [ Newline ; minus ; 
		    Raw ("val " ^ (Name.simple v.val_name) ^ " :\n") ; 
		    self#text_el_of_type_expr 
		      (Name.father v.val_name) v.val_type ] ;
		self#index `Value v.val_name ; Newline  ] @
		(self#text_of_info v.val_info) in
      self#texi_of_text t


    (** Return Texinfo code for a class attribute. *)
    method texi_of_attribute a =
      Odoc_info.reset_type_names () ;
      let t = [ self#fixedblock
		  [ Newline ; minus ; 
		    Raw "val " ;
		    Raw (if a.att_mutable then "mutable " else "") ;
		    Raw (Name.simple a.att_value.val_name) ;
		    Raw " :\n" ; 
		    self#text_el_of_type_expr 
		      (Name.father a.att_value.val_name) 
		      a.att_value.val_type ] ;
		self#index `Class_att a.att_value.val_name  ; Newline ] @
	(self#text_of_info a.att_value.val_info) in
      self#texi_of_text t


    (** Return Texinfo code for a class method. *)
    method texi_of_method m = 
      Odoc_info.reset_type_names () ;
      let t = [ self#fixedblock
		  [ Newline ; minus ; Raw "method " ;
		    Raw (if m.met_private then "private " else "") ;
		    Raw (if m.met_virtual then "virtual " else "") ;
		    Raw (Name.simple m.met_value.val_name) ;
		    Raw " :\n" ; 
		    self#text_el_of_type_expr 
		      (Name.father m.met_value.val_name)
		      m.met_value.val_type ] ;
		self#index `Method m.met_value.val_name ; Newline ] @
	(self#text_of_info m.met_value.val_info) in
      self#texi_of_text t


    method string_of_type_parameter = function
      |	[] -> ""
      |	[ tp ] -> (Odoc_info.string_of_type_expr tp) ^ " "
      |	l -> "(" ^ (String.concat ", "
		      (List.map Odoc_info.string_of_type_expr l)) ^ ") "

    method string_of_type_args = function
      | [] -> ""
      | args -> " of " ^ (Odoc_info.string_of_type_list " * " args)

    (** Return Texinfo code for a type. *)
    method texi_of_type ty = 
      Odoc_info.reset_type_names () ;
      let t = 
	[ self#fixedblock ( 
	  [ Newline ; minus ; Raw "type " ;
	    Raw (self#string_of_type_parameter ty.ty_parameters) ;
	    Raw (Name.simple ty.ty_name) ] @
	  ( match ty.ty_manifest with
	  | None -> [] 
	  | Some typ -> 
	      (Raw " = ") :: (self#text_of_short_type_expr 
				(Name.father ty.ty_name) typ) ) @
	  ( match ty.ty_kind with
	  | Type_abstract -> [ Newline ]
	  | Type_variant l ->
	      (Raw " =\n") ::
	      (List.flatten 
		 (List.map 
		    (fun constr ->
		      (Raw ("  | " ^ constr.vc_name)) ::
		      (Raw (self#string_of_type_args constr.vc_args)) ::
		      (match constr.vc_text with
		      | None -> [ Newline ]
		      | Some t -> 
			  ((Raw (indent 5 "\n(* ")) :: (self#soft_fix_linebreaks 8 t)) @ 
			  [ Raw " *)" ; Newline ]
		      ) ) l ) )
	  | Type_record l ->
	      (Raw " = {\n") ::
	      (List.flatten 
		 (List.map 
		    (fun r -> 
		      [ Raw ("  " ^ r.rf_name ^ " : ") ] @
		      (self#text_of_short_type_expr 
			 (Name.father r.rf_name)
			 r.rf_type) @ 
		      [ Raw " ;" ] @
		      (match r.rf_text with
		      | None -> [ Newline ]
 		      | Some t -> 
			  ((Raw (indent 5 "\n(* ")) :: (self#soft_fix_linebreaks 8 t)) @ 
			  [ Raw " *)" ; Newline ] ) ) 
		    l ) )
	      @  [ Raw " }" ] ) ) ;
	  self#index `Type ty.ty_name ; Newline ] @
	(self#text_of_info ty.ty_info) in
      self#texi_of_text t

    (** Return Texinfo code for an exception. *)
    method texi_of_exception e = 
      Odoc_info.reset_type_names () ;
      let t = 
	[ self#fixedblock
	    ( [ Newline ; minus ; Raw "exception " ; 
		Raw (Name.simple e.ex_name) ;
		Raw (self#string_of_type_args e.ex_args) ] @
	      (match e.ex_alias with
	      | None -> []
	      | Some ea -> [ Raw " = " ; Raw
			       ( match ea.ea_ex with
			       | None -> ea.ea_name
			       | Some e -> e.ex_name ) ; ]
	      ) ) ;
	  self#index `Exception e.ex_name ; Newline ] @
	(self#text_of_info e.ex_info) in
      self#texi_of_text t


    (** Return the Texinfo code for the given module. *)
    method texi_of_module m =
      let is_alias = function
	| { m_kind = Module_alias _ } -> true
	| _ -> false in
      let is_alias_there = function
	| { m_kind = Module_alias { ma_module = None } } -> false
	| _ -> true in
      let resolve_alias_name = function
	| { m_kind = Module_alias { ma_name = name } } -> name
	| { m_name = name } -> name in
      let t = 
	[ [ self#fixedblock 
	      [ Newline ; minus ; Raw "module " ; 
		Raw (Name.simple m.m_name) ;
		Raw (if is_alias m 
		then " = " ^ (resolve_alias_name m) 
		else "" ) ] ] ;
	  ( if is_alias_there m
	  then [ Ref (resolve_alias_name m, Some RK_module) ; 
		 Newline ; ]
	  else [] ) ;
	  ( if is_alias m 
	  then [ self#index `Module m.m_name ; Newline ]
	  else [ Newline ] ) ;
	  self#text_of_info m.m_info ]
      in
      self#texi_of_text (List.flatten t)

    (** Return the Texinfo code for the given module type. *)
    method texi_of_module_type mt =
      let is_alias = function
	| { mt_kind = Some (Module_type_alias _) } -> true
	| _ -> false in
      let is_alias_there = function
	| { mt_kind = Some (Module_type_alias { mta_module = None }) } -> false
	| _ -> true in
      let resolve_alias_name = function
	| { mt_kind = Some (Module_type_alias { mta_name = name }) } -> name
	| { mt_name = name } -> name in
      let t = 
	[ [ self#fixedblock 
	      [ Newline ; minus ; Raw "module type" ; 
		Raw (Name.simple mt.mt_name) ;
		Raw (if is_alias mt
		then " = " ^ (resolve_alias_name mt) 
		else "" ) ] ] ;
	  ( if is_alias_there mt
	  then [ Ref (resolve_alias_name mt, Some RK_module_type) ; 
		 Newline ; ]
	  else [] ) ;
	  ( if is_alias mt
	  then [ self#index `Module_type mt.mt_name ; Newline ]
	  else [ Newline ] ) ;
	  self#text_of_info mt.mt_info ]
      in
      self#texi_of_text (List.flatten t)

    (** Return the Texinfo code for the given included module. *)
    method texi_of_included_module im =
      let t = [ self#fixedblock
		  ( Newline :: minus :: (Raw "include module ") ::
		    ( match im.im_module with
		    | None -> 
			[ Raw im.im_name ]
		    | Some (Mod { m_name = name }) -> 
			[ Raw name ; Raw "\n     " ; 
			  Ref (name, Some RK_module) ]
		    | Some (Modtype { mt_name = name }) ->
			[ Raw name ; Raw "\n     " ; 
			  Ref (name, Some RK_module_type) ]
		     ) ) ] in
      self#texi_of_text t

    (** Return the Texinfo code for the given class. *)
    method texi_of_class c =
      Odoc_info.reset_type_names () ;
      let t = [ self#fixedblock 
		  [ Newline ; minus ; Raw "class " ; 
		    Raw (Name.simple c.cl_name) ] ; 
		Ref (c.cl_name, Some RK_class) ; Newline ;
		Newline ] @ (self#text_of_info c.cl_info) in
      self#texi_of_text t

    (** Return the Texinfo code for the given class type. *)
    method texi_of_class_type ct =
      Odoc_info.reset_type_names () ;
      let t = [ self#fixedblock 
		  [ Newline ; minus ; Raw "class type " ; 
		    Raw (Name.simple ct.clt_name) ] ; 
		Ref (ct.clt_name, Some RK_class_type) ; Newline ;
		Newline ] @ (self#text_of_info ct.clt_info) in
      self#texi_of_text t

    (** Return the Texinfo code for the given class element. *)
    method texi_of_class_element class_name class_ele =
      match class_ele with
      | Class_attribute att -> self#texi_of_attribute att
      | Class_method met -> self#texi_of_method met
      | Class_comment t -> self#texi_of_text t

    (** Return the Texinfo code for the given module element. *)
    method texi_of_module_element module_name module_ele =
      (match module_ele with
      | Element_module m -> self#texi_of_module m
      | Element_module_type mt -> self#texi_of_module_type mt
      | Element_included_module im -> self#texi_of_included_module im
      | Element_class c -> self#texi_of_class c
      | Element_class_type ct -> self#texi_of_class_type ct
      | Element_value v -> self#texi_of_value v
      | Element_exception e -> self#texi_of_exception e
      | Element_type t -> self#texi_of_type t
      | Element_module_comment t -> self#texi_of_text (t @ [Newline])
      )

    (** Generate the Texinfo code for the given list of inherited classes.*)
    method generate_inheritance_info chanout inher_l =
      let f inh =
	match inh.ic_class with
	| None -> (* we can't make the reference *)
	    (Code inh.ic_name) ::
	    (match inh.ic_text with
	    | None -> []
	    | Some t -> Newline :: t)
	| Some cct -> (* we can create the reference *)
	    let kind = 
	      match cct with
	      | Cl _ -> Some RK_class 
	      | Cltype _ -> Some RK_class_type in
	    (Code inh.ic_name) ::
	    (Ref (inh.ic_name, kind)) ::
	    ( match inh.ic_text with
	    | None -> []
	    | Some t -> Newline :: t)
      in
      let text = [
	Bold [ Raw Odoc_messages.inherits ] ;
	List (List.map f inher_l) ; Newline ] 
      in
      puts chanout (self#texi_of_text text)



    (** Generate the Texinfo code for the inherited classes 
       of the given class. *)
    method generate_class_inheritance_info chanout cl =
      let rec iter_kind = function
	| Class_structure ([], _) -> ()
	| Class_structure (l, _) ->
	    self#generate_inheritance_info chanout l
	| Class_constraint (k, _) -> iter_kind k
	| Class_apply _
	| Class_constr _ -> ()
      in
      iter_kind cl.cl_kind



    (** Generate the Texinfo code for the inherited classes 
       of the given class type. *)
    method generate_class_type_inheritance_info chanout clt =
      match clt.clt_kind with
      |	Class_signature ([], _) ->
	  ()
      |	Class_signature (l, _) ->
	  self#generate_inheritance_info chanout l
      |	Class_type _ ->
	  ()

    (** Generate the Texinfo code for the given class, 
       in the given out channel. *)
    method generate_for_class chanout c =
      Odoc_info.reset_type_names () ;
      let depth = Name.depth c.cl_name in
      let title = [ 
	self#node depth c.cl_name ;
	Title (depth, None, [ Raw (Odoc_messages.clas ^ " ") ;
				    Code c.cl_name ]) ; 
	self#index `Class c.cl_name ] in
      puts chanout (self#texi_of_text title) ;

      if is c.cl_info
      then begin
	let descr = [ Title (succ depth, None,
			     [ Raw Odoc_messages.description ]) ] in
	puts chanout (self#texi_of_text descr) ;
	puts chanout (self#texi_of_info c.cl_info) 
      end ;
      
      let intf = [ Title (succ depth, None, 
			  [ Raw Odoc_messages.interface]) ] in
      puts chanout (self#texi_of_text intf);
      self#generate_class_inheritance_info chanout c ;
      List.iter
	(fun ele -> puts chanout
	    (self#texi_of_class_element c.cl_name ele))
	(Class.class_elements ~trans:false c)


    (** Generate the Texinfo code for the given class type, 
       in the given out channel. *)
    method generate_for_class_type chanout ct =
      Odoc_info.reset_type_names () ;
      let depth = Name.depth ct.clt_name in
      let title = [ 
	self#node depth ct.clt_name ;
	Title (depth, None, [ Raw (Odoc_messages.class_type ^ " ") ; 
				    Code ct.clt_name ]) ; 
	self#index `Class_type ct.clt_name ] in
      puts chanout (self#texi_of_text title) ;
      
      if is ct.clt_info
      then begin
	let descr = [ Title (succ depth, None,
			     [ Raw Odoc_messages.description ]) ] in
	puts chanout (self#texi_of_text descr) ;
	puts chanout (self#texi_of_info ct.clt_info)
      end ;

      let intf = [ Title (succ depth, None, 
			  [ Raw Odoc_messages.interface ]) ] in
      puts chanout (self#texi_of_text intf) ;
      self#generate_class_type_inheritance_info chanout ct;
      List.iter 
	(fun ele -> puts chanout
	    (self#texi_of_class_element ct.clt_name ele))
	(Class.class_type_elements ~trans:false ct)



    (** Generate the Texinfo code for the given module type, 
       in the given out channel. *)
    method generate_for_module_type chanout mt =
      let depth = Name.depth mt.mt_name in
      let title = [ 
	self#node depth mt.mt_name ;
	Title (depth, None, [ Raw (Odoc_messages.module_type ^ " ") ; 
			      Code mt.mt_name ]) ; 
	self#index `Module_type mt.mt_name ; Newline ] in
      puts chanout (self#texi_of_text title) ;
      
      if is mt.mt_info
      then begin
	let descr = [ Title (succ depth, None,
			     [ Raw Odoc_messages.description ]) ] in
	puts chanout (self#texi_of_text descr) ;
	puts chanout (self#texi_of_info mt.mt_info)
      end ;

      let mt_ele = Module.module_type_elements ~trans:false mt in
      let subparts = module_subparts mt_ele in
      if depth < maxdepth && subparts <> []
      then begin
	let menu = Texi.ifinfo 
	    ( self#heading (succ depth) [ Raw "Subparts" ]) in
	puts chanout menu ;
	Texi.generate_menu chanout (subparts :> subparts)
      end ;

      let intf = [ Title (succ depth, None, 
			  [ Raw Odoc_messages.interface ]) ] in
      puts chanout (self#texi_of_text intf) ;
      List.iter
	(fun ele -> puts chanout
	    (self#texi_of_module_element mt.mt_name ele))
	mt_ele ;

      (* create sub parts for modules, module types, classes and class types *)
      List.iter
	(function
	  | `Module m -> self#generate_for_module chanout m
	  | `Module_type mt -> self#generate_for_module_type chanout mt
	  | `Class c -> self#generate_for_class chanout c
	  | `Class_type ct -> self#generate_for_class_type chanout ct)
	subparts


    (** Generate the Texinfo code for the given module, 
       in the given out channel. *)
    method generate_for_module chanout m =
      let depth = Name.depth m.m_name in
      let title = [ 
	self#node depth m.m_name ;
	Title (depth, None, [ Raw (Odoc_messages.modul ^ " ") ; 
			      Code m.m_name ]) ; 
	self#index `Module m.m_name ; Newline ] in
      puts chanout (self#texi_of_text title) ;
      
      if is m.m_info
      then begin
	let descr = [ Title (succ depth, None,
			     [ Raw Odoc_messages.description ]) ] in
	puts chanout (self#texi_of_text descr) ;
	puts chanout (self#texi_of_info m.m_info)
      end ;
      
      let m_ele = Module.module_elements ~trans:false m in
      let subparts = module_subparts m_ele in
      if depth < maxdepth && subparts <> []
      then begin
	let menu = Texi.ifinfo 
	    ( self#heading (succ depth) [ Raw "Subparts" ]) in
	puts chanout menu ;
	Texi.generate_menu chanout (subparts :> subparts)
      end ;

      let intf = [ Title (succ depth, None, 
			  [ Raw Odoc_messages.interface]) ] in
      puts chanout (self#texi_of_text intf) ;

      List.iter
	(fun ele -> puts chanout
	    (self#texi_of_module_element m.m_name ele))
	m_ele ;

      (* create sub nodes for modules, module types, classes and class types *)
      List.iter
	(function
	  | `Module m -> self#generate_for_module chanout m
	  | `Module_type mt -> self#generate_for_module_type chanout mt
	  | `Class c -> self#generate_for_class chanout c
	  | `Class_type ct -> self#generate_for_class_type chanout ct )
	subparts



    (** Return the header of the TeX document. *)
    method generate_texi_header chan m_list =
      let title, filename = 
       match !Odoc_args.title with 
       | None -> ("", "doc.info")
       | Some s -> 
	   let s' = self#escape s in 
	   (s', s' ^ ".info")
      in
      (* write a standard Texinfo header *)
      List.iter
	(puts_nl chan)
	(List.flatten 
	   [ [ "\\input texinfo   @c -*-texinfo-*-" ;
	       "@c %**start of header" ;
	       "@setfilename " ^ filename ;
	       "@settitle " ^ title ;
	       "@c %**end of header" ; ] ;
	     
	     (if !with_index then
	       List.map 
		 (fun (_, shortname) ->
		   "@defcodeindex " ^ shortname)
		 indices_names
	     else []) ;

	     [ "@ifinfo" ;
	       "This file was generated by Ocamldoc using the Texinfo generator." ;
	       "@end ifinfo" ;
	       
	       "@c no titlepage." ;

	       "@node Top, , , (dir)" ;
	       "@top "^ title ; ]
	   ] ) ;
      if title <> ""
      then begin
	puts_nl chan "@ifinfo" ; 
	puts_nl chan ("Documentation for " ^ title) ; 
	puts_nl chan "@end ifinfo" 
      end 
      else puts_nl chan "@c no title given" ;
      
      (* write a top menu *)
      Texi.generate_menu chan 
	((List.map (fun m -> `Module m) m_list) @
	 (if !with_index then
	   [ `Blank ; `Comment "Indices :" ] @
	     (List.map
		(fun (longname, _) -> `Index (longname ^ " index"))
		indices_names )
	 else [] ))
      


    method generate_texi_trailer chan = 
      nl chan ; 
      if !with_index
      then 
	List.iter (puts_nl chan)
	  (List.flatten
	     (List.map 
		(fun (longname, shortname) ->
		  [ "@node " ^ longname ^ " index," ;
		    "@unnumbered " ^ longname ^ " index" ;
		    "@printindex " ^ shortname ; ])
		indices_names )) ;
      if !Odoc_args.with_toc 
      then puts_nl chan "@contents" ;
      puts_nl chan "@bye"



    (** Generate the Texinfo file from a module list, in the {!Odoc_args.out_file} file. *)
    method generate module_list =
      try
	let chanout = open_out 
	    (Filename.concat !Odoc_args.target_dir !Odoc_args.out_file) in
	if !Odoc_args.with_header 
	then self#generate_texi_header chanout module_list ;
	List.iter 
	  (fun modu ->
 	    Odoc_info.verbose ("Generate for module " ^ modu.m_name) ;
	    self#generate_for_module chanout modu) 
	  module_list ;
	if !Odoc_args.with_trailer 
	then self#generate_texi_trailer chanout ;
	close_out chanout
      with
      |	Failure s
      |	Sys_error s ->
	  prerr_endline s ;
	  incr Odoc_info.errors 
  end

