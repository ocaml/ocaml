(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

type stream_filter 'a 'loc = Stream.t ('a * 'loc) -> Stream.t ('a * 'loc);

module type S = sig

  module Loc : Loc.S;

  type t = 'abstract;

  value to_string : t -> string;
  
  value print : Format.formatter -> t -> unit;

  value match_keyword : string -> t -> bool;

  value extract_string : t -> string;

  module Filter : sig

    type token_filter = stream_filter t Loc.t;

    (** The type for this filter chain.
        A basic implementation just store the [is_keyword] function given
        by [mk] and use it in the [filter] function. *)
    type t = 'abstract;

    (** The given predicate function returns true if the given string
        is a keyword. This function can be used in filters to translate
        identifier tokens to keyword tokens. *)
    value mk : (string -> bool) -> t;

    (** This function allows to register a new filter to the token filter chain.
        You can choose to not support these and raise an exception. *)
    value define_filter : t -> (token_filter -> token_filter) -> unit;

    (** This function filter the given stream and return a filtered stream.
        A basic implementation just match identifiers against the [is_keyword]
        function to produce token keywords instead. *)
    value filter : t -> token_filter;

    (** Called by the grammar system when a keyword is used. 
        The boolean argument is True when it's the first time that keyword
        is used. If you do not care about this information just return [()]. *)
    value keyword_added : t -> string -> bool -> unit;

    (** Called by the grammar system when a keyword is no longer used.
        If you do not care about this information just return [()]. *)
    value keyword_removed : t -> string -> unit;
  end;

  module Error : Error.S;
end;
