(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

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
  | LocInfo of (int * int) and pretty ]
;
type getcomm = int -> int -> (string * int * int * int);

value print_pretty :
  (char -> unit) -> (string -> unit) -> (unit -> unit) ->
     string -> string -> int -> getcomm -> int -> pretty -> unit;
value quiet : ref bool;

value dt : ref int;

(*--*)

value tol : ref int;
value sp : ref char;
