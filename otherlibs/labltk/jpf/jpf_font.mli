(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)
val debug : bool ref

type ('a, 'b) xlfd =
  { mutable foundry: 'a;
    mutable family: 'a;
    mutable weight: 'a;
    mutable slant: 'a;
    mutable setWidth: 'a;
    mutable addStyle: 'a;
    mutable pixelSize: 'b;
    mutable pointSize: 'b;
    mutable resolutionX: 'b;
    mutable resolutionY: 'b;
    mutable spacing: 'a;
    mutable averageWidth: 'b;
    mutable registry: 'a;
    mutable encoding: 'a }

exception Parse_Xlfd_Failure of string

type valid_xlfd = (string, int) xlfd
type pattern = (string option, int option) xlfd

val empty_pattern : pattern

val copy : ('a, 'b) xlfd -> ('a, 'b) xlfd

val string_of_valid_xlfd : valid_xlfd -> string 
val string_of_pattern : pattern -> string

val is_vector_font : valid_xlfd -> bool

val list_fonts : string option -> pattern -> valid_xlfd list

val available_pixel_size : 
    string option -> pattern -> (int * valid_xlfd list) list

val nearest_pixel_size :
    string option -> bool -> pattern -> valid_xlfd
