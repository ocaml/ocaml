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


(** Types for the information collected in comments. *)

(** The differents kinds of element references. *)
type ref_kind = 
    RK_module
  | RK_module_type
  | RK_class
  | RK_class_type
  | RK_value
  | RK_type
  | RK_exception
  | RK_attribute
  | RK_method
  | RK_section

type text_element = 
  | Raw of string (** Raw text. *)
  | Code of string (** The string is source code. *)
  | CodePre of string (** The string is pre-formatted source code. *)
  | Verbatim of string (** String 'as is'. *)
  | Bold of text (** Text in bold style. *)
  | Italic of text (** Text in italic. *)
  | Emphasize of text (** Emphasized text. *)
  | Center of text (** Centered text. *)
  | Left of text (** Left alignment. *)
  | Right of text (** Right alignment. *)
  | List of text list (** A list. *)
  | Enum of text list (** An enumerated list. *)
  | Newline   (** To force a line break. *)
  | Block of text (** Like html's block quote. *)
  | Title of int * string option * text
              (** Style number, optional label, and text. *)
  | Latex of string (** A string for latex. *)
  | Link of string * text (** A reference string and the link text. *)
  | Ref of string * ref_kind option
       (** A reference to an element. Complete name and kind. *)
  | Superscript of text (** Superscripts. *)
  | Subscript of text (** Subscripts. *)

(** [text] is a list of text_elements. The order matters. *)
and text = text_element list

(** The different forms of references in \@see tags. *)
type see_ref = 
    See_url of string
  | See_file of string
  | See_doc of string

(** The information in a \@see tag. *)
type see = see_ref * text

(** Parameter name and description. *)
type param = (string * text)

(** Raised exception name and description. *)
type raised_exception = (string * text)

(** Information in a special comment. *)
type info = {
    i_desc : text option; (** The description text. *)
    i_authors : string list; (** The list of authors in \@author tags. *)
    i_version : string option; (** The string in the \@version tag. *)
    i_sees : see list; (** The list of \@see tags. *)
    i_since : string option; (** The string in the \@since tag. *)
    i_deprecated : text option; (** The of the \@deprecated tag. *)
    i_params : param list; (** The list of parameter descriptions. *)
    i_raised_exceptions : raised_exception list; (** The list of raised exceptions. *)
    i_return_value : text option ; (** The description text of the return value. *)
    i_custom : (string * text) list ; (** A text associated to a custom @-tag. *)
  } 

(** An empty info structure. *)
let dummy_info = {
  i_desc = None ;
  i_authors = [] ;
  i_version = None ;
  i_sees = [] ;
  i_since = None ;
  i_deprecated = None ;
  i_params = [] ;
  i_raised_exceptions = [] ;
  i_return_value = None ;
  i_custom = [] ;
} 

(** Location of elements in implementation and interface files. *)
type location = {
    loc_impl : (string * int) option ; (** implementation file name and position *)
    loc_inter : (string * int) option ; (** interface file name and position *)
  } 

(** A dummy location. *)
let dummy_loc = { loc_impl = None ; loc_inter = None }

(** The information to merge from two elements when they both have some information. *)
type merge_option =
  | Merge_description (** Descriptions are concatenated. *)
  | Merge_author (** Lists of authors are concatenated. *)
  | Merge_version (** Versions are concatenated. *)
  | Merge_see (** See references are concatenated. *)
  | Merge_since (** Since information are concatenated. *)
  | Merge_deprecated (** Deprecated information are concatenated. *)
  | Merge_param (** Information on each parameter is concatenated,
		    and all parameters are kept. *)
  | Merge_raised_exception (** Information on each raised_exception is concatenated,
			       and all raised exceptions are kept. *)
  | Merge_return_value (** Information on return value are concatenated. *)
  | Merge_custom (** Merge custom tags (all pairs (tag, text) are kept). *)
  
(** The list with all merge options. *)
let all_merge_options = [
  Merge_description ;
  Merge_author ;
  Merge_version ;
  Merge_see ;
  Merge_since ;
  Merge_deprecated ;
  Merge_param ;
  Merge_raised_exception ;
  Merge_return_value ;
  Merge_custom ;
] 

