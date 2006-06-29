(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 1998-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 *)


(* $Id$ *)

(* Hbox: horizontal box
   HVbox: horizontal-vertical box
   HOVbox and HOVCbox: fill maximum of elements horizontally, line by line;
     in HOVbox, if an element has to be displayed vertically (need several
     lines), the next element is displayed next line; in HOVCbox, this next
     element may be displayed same line if it holds.
   Vbox: vertical box
   BEbox: begin-end box: horizontal or 2nd element indented, 3rd element not
   BEVbox: begin-end box always vertical
   LocInfo: call back with location to allow inserting comments *)

(* In case of box displayed vertically, 2nd line and following are indented
   by dt.val spaces, except if first element of the box is empty: to not
   indent, put HVbox [: :] as first element *)

(* The glue type allows to specify how spaces are put around a token:
    LO: Left only
    RO: Right only
    LR: Left right
    NO: No spaces

   Example:
    foo (3 + t.(4))
    "(" => LO
    "+" => LR
    ".(" => NO
    ")" => RO

   Space rules:
    _ + NO or NO + _ => nothing
    R + L => space (ex: ...] [...)
    L + R => nothing (ex: [])
*)
module Make (Loc : Sig.Loc.S) : sig
  module Loc : Sig.Loc.S;

  type glue = [ LO | RO | LR | NO ];
  type pretty =
    [ S of glue and string
    | Hbox of Stream.t pretty
    | HVbox of Stream.t pretty
    | HOVbox of Stream.t pretty
    | HOVCbox of Stream.t pretty
    | Vbox of Stream.t pretty
    | BEbox of Stream.t pretty
    | BEVbox of Stream.t pretty
    | LocInfo of Loc.t and pretty ]
  ;

  type comm = (string * int * int);
  type getcomm = Loc.t -> list comm;

  value print_pretty :
    (char -> unit) -> (string -> unit) -> (unit -> unit) ->
      string -> string -> int -> getcomm -> int -> pretty -> unit;

  value quiet : ref bool;

  value dt : ref int;

  (*--*)

  value tol : ref int;
  value sp : ref char;

end with module Loc = Loc;
