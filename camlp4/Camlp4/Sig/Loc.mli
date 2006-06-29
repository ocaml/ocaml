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

module type S = sig

  type t = 'a;

  (** Return a start location for the given file name.
      This location starts at the begining of the file. *)
  value mk : string -> t;

  (** The [ghost] location can be used when no location
      information is available. *)
  value ghost : t;

  (** {6 Conversion functions} *)

  (** Return a location where both positions are set the given position. *)
  value of_lexing_position : Lexing.position -> t;

  (** Return an OCaml location. *)
  value to_ocaml_location : t -> Location.t;

  (** Return a location from an OCaml location. *)
  value of_ocaml_location : Location.t -> t;

  (** Return a location from ocamllex buffer. *)
  value of_lexbuf : Lexing.lexbuf -> t;

  (** Return a location from [(file_name, start_line, start_bol, start_off,
      stop_line,  stop_bol,  stop_off, ghost)]. *)
  value of_tuple : (string * int * int * int * int * int * int * bool) -> t;

  (** Return [(file_name, start_line, start_bol, start_off,
      stop_line,  stop_bol,  stop_off, ghost)]. *)
  value to_tuple : t -> (string * int * int * int * int * int * int * bool);

  (** [merge loc1 loc2] Return a location that starts at [loc1] and end at [loc2]. *)
  value merge : t -> t -> t;

  (** The stop pos becomes equal to the start pos. *)
  value join : t -> t;

  (** [move selector n loc]
      Return the location where positions are moved.
      Affected positions are chosen with [selector].
      Returned positions have their character offset plus [n]. *)
  value move : [= `start | `stop | `both ] -> int -> t -> t;

  (** [shift n loc] Return the location where the new start position is the old
      stop position, and where the new stop position character offset is the
      old one plus [n]. *)
  value shift : int -> t -> t;

  (** [move_line n loc] Return the location with the old line count plus [n].
      The "begin of line" of both positions become the current offset. *)
  value move_line : int -> t -> t;

  (** Accessors *)

  (** Return the file name *)
  value file_name  : t -> string;

  (** Return the line number of the begining of this location. *)
  value start_line : t -> int;

  (** Return the line number of the ending of this location. *)
  value stop_line  : t -> int;

  (** Returns the number of characters from the begining of the file
      to the begining of the line of location's begining. *)
  value start_bol  : t -> int;

  (** Returns the number of characters from the begining of the file
      to the begining of the line of location's ending. *)
  value stop_bol   : t -> int;

  (** Returns the number of characters from the begining of the file
      of the begining of this location. *)
  value start_off  : t -> int;

  (** Return the number of characters from the begining of the file
      of the ending of this location. *)
  value stop_off   : t -> int;

  (** Return the start position as a Lexing.position. *)
  value start_pos  : t -> Lexing.position;

  (** Return the stop position as a Lexing.position. *)
  value stop_pos   : t -> Lexing.position;

  (** Generally, return true if this location does not come
      from an input stream. *)
  value is_ghost   : t -> bool;

  (** Return the associated ghost location. *)
  value ghostify   : t -> t;

  (** Return the location with the give file name *)
  value set_file_name : string -> t -> t;

  (** [strictly_before loc1 loc2] True if the stop position of [loc1] is
      strictly_before the start position of [loc2]. *)
  value strictly_before : t -> t -> bool;

  (** Return the location with an absolute file name. *)
  value make_absolute : t -> t;

  (** Print the location into the formatter in a format suitable for error
      reporting. *)
  value print : Format.formatter -> t -> unit;

  (** Print the location in a short format useful for debugging. *)
  value dump  : Format.formatter -> t -> unit;

  (** Same as {!print} but return a string instead of printting it. *)
  value to_string : t -> string;

  (** [Exc_located loc e] is an encapsulation of the exception [e] with
      the input location [loc]. To be used in quotation expanders
      and in grammars to specify some input location for an error.
      Do not raise this exception directly: rather use the following
      function [Loc.raise]. *)
  exception Exc_located of t and exn;

  (** [raise loc e], if [e] is already an [Exc_located] exception,
      re-raise it, else raise the exception [Exc_located loc e]. *)
  value raise : t -> exn -> 'a;

  (** The name of the location variable used in grammars and in
      the predefined quotations for OCaml syntax trees. Default: [_loc]. *)
  value name : ref string;

end;
