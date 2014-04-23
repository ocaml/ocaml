(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                           Leo White                                 *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Types for the information collected in comments. *)

(** The differents kinds of style. *)
type style_kind =
  | SK_bold
  | SK_italic
  | SK_emphasize
  | SK_center
  | SK_left
  | SK_right
  | SK_superscript
  | SK_subscript
  | SK_custom of string

(** The differents kinds of element references. *)
type ref_kind =
    RK_element
  | RK_module
  | RK_module_type
  | RK_class
  | RK_class_type
  | RK_value
  | RK_type
  | RK_exception
  | RK_attribute
  | RK_method
  | RK_section
  | RK_recfield
  | RK_const
  | RK_link
  | RK_custom of string

(* The different kinds of special reference *)
type special_ref_kind =
    SRK_module_list of string list
  | SRK_index_list

and text_element =
  | Raw of string (** Raw text. *)
  | Code of string (** The string is source code. *)
  | PreCode of string (** The string is pre-formatted source code. *)
  | Verbatim of string (** String 'as is'. *)
  | Style of style_kind * text (** Text tagged with a style. *)
  | List of text list (** A list. *)
  | Enum of text list (** An enumerated list. *)
  | Newline   (** To force a line break. *)
  | Block of text (** Like html's block quote. *)
  | Title of int * string option * text
              (** Style number, optional label, and text. *)
  | Ref of ref_kind * string * text option
    (** A reference to an element. Complete name and kind. An optional
        text can be given to display this text instead of the element name.*)
  | Special_ref of special_ref_kind (** Special kinds of reference *)
  | Target of string option * string (** (target, code) : to specify code for a specific target format *)

(** [text] is a list of text_elements. The order matters. *)
and text = text_element list

(** The different forms of references in \@see tags. *)
type see_ref =
    See_url of string
  | See_file of string
  | See_doc of string

(** Tags *)
type tag =
    Author of string (** \@author tag *)
  | Version of string (** \@version tag *)
  | See of see_ref * text (** \@see tag *)
  | Since of string (** \@since tag *)
  | Before of string * text (** \@before tag *)
  | Deprecated of text (** \@deprecated tag *)
  | Param of string * text (** \@param tag *)
  | Raised_exception of string * text (** \@raise tag *)
  | Return_value of text (** \@return tag *)
  | Custom of string * text (** custom tag *)

(** A special comment *)
type t =
    Cinfo of text * tag list
  | Cstop
