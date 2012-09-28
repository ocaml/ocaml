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

(* $Id: odoc_comments.mli 6336 2004-05-27 09:18:38Z maranget $ *)

(** Analysis of comments. *)

val simple_blank : string

(** The type of modules in argument to Info_retriever *)
module type Texter = 
  sig
    (** Return a text structure from a string. *)
    val text_of_string : string -> Odoc_types.text
  end

(** The basic module for special comments analysis.*)
module Basic_info_retriever :
  sig
    (** Return true if the given string contains a blank line. *)
    val blank_line_outside_simple :
        string -> string -> bool

    (** This function retrieves all the special comments in the given string. *)
    val all_special : string -> string -> int * Odoc_types.info list

    (** [just_after_special file str] return the pair ([length], [info_opt])
       where [info_opt] is the first optional special comment found 
       in [str], without any blank line before. [length] is the number
       of chars from the beginning of [str] to the end of the special comment. *)
    val just_after_special :
        string -> string -> int * Odoc_types.info option

    (** [first_special file str] return the pair ([length], [info_opt])
       where [info_opt] is the first optional special comment found 
       in [str]. [length] is the number of chars from the beginning of
       [str] to the end of the special comment. *)
    val first_special :
        string -> string -> int * Odoc_types.info option

    (** Return a pair [(comment_opt, element_comment_list)], where [comment_opt] is the last special
       comment found in the given string and not followed by a blank line, 
       and [element_comment_list] the list of values built from the other
       special comments found and the given function. *)
    val get_comments :
        (Odoc_types.text -> 'a) ->
          string -> string -> Odoc_types.info option * 'a list

  end
