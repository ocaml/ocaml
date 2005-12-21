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

(* $Id$ *)

open Widget

(* Lower level interface *)
exception TkError of string
      (* Raised by the communication functions *)

val debug : bool ref 
      (* When set to true, displays approximation of intermediate Tcl code *)

type tkArgs =
    TkToken of string
  | TkTokenList of tkArgs list          (* to be expanded *)
  | TkQuote of tkArgs                   (* mapped to Tcl list *)


(* Misc *)
external splitlist : string -> string list
        = "camltk_splitlist"

val add_destroy_hook : (any widget -> unit) -> unit


(* Opening, closing, and mainloop *)
val default_display : unit -> string

val opentk : unit -> toplevel widget
    (* The basic initialization function. *)

val keywords : (string * Arg.spec * string) list
    (* Command line parsing specification for Arg.parse, which contains
       the standard Tcl/Tk command line options such as "-display" and "-name".
       Add [keywords] to a [Arg.parse] call, then call [opentk].
       Then [opentk] can make use of these command line options 
       to initiate applications. *)

val opentk_with_args : string list -> toplevel widget
    (* [opentk_with_args] is a lower level interface to initiate Tcl/Tk 
       applications.  [opentk_with_args argv] initializes Tcl/Tk with
       the command line options given by [argv] *)

val openTk : ?display:string -> ?clas:string -> unit -> toplevel widget
    (* [openTk ~display:display ~clas:clas ()] is equivalent to
       [opentk_with_args ["-display"; display; "-name"; clas]] *)

(* Legacy opentk functions *)
val openTkClass: string -> toplevel widget
    (* [openTkClass class] is equivalent to [opentk ["-name"; class]] *)
val openTkDisplayClass: string -> string -> toplevel widget
    (* [openTkDisplayClass disp class] is equivalent to 
       [opentk ["-display"; disp; "-name"; class]] *)

val closeTk : unit -> unit
val finalizeTk : unit -> unit 
    (* Finalize tcl/tk before exiting. This function will be automatically 
       called when you call [Pervasives.exit ()] *)

val mainLoop : unit -> unit
    (* Start the event loop *)

type event_flag =
  DONT_WAIT | X_EVENTS | FILE_EVENTS | TIMER_EVENTS | IDLE_EVENTS | ALL_EVENTS
val do_one_event : event_flag list -> bool
    (* Process a single event *)
val do_pending : unit -> unit
    (* Process all pending events, without waiting.
       This lets you use Tk from the toplevel, for instance. *)


(* Direct evaluation of tcl code *)
val tkEval : tkArgs array -> string

val tkCommand : tkArgs array -> unit

(* Returning a value from a Tcl callback *)
val tkreturn: string -> unit


(* Callbacks: this is private *)

type cbid

type callback_buffer = string list
      (* Buffer for reading callback arguments *)

val callback_naming_table : (cbid, callback_buffer -> unit) Hashtbl.t
val callback_memo_table : (any widget, cbid) Hashtbl.t
      (* Exported for debug purposes only. Don't use them unless you
         know what you are doing *)
val new_function_id : unit -> cbid
val string_of_cbid : cbid -> string
val register_callback : 'a widget -> callback:(callback_buffer -> unit) -> string
      (* Callback support *)
val clear_callback : cbid -> unit
      (* Remove a given callback from the table *)
val remove_callbacks : 'a widget -> unit
      (* Clean up callbacks associated to widget. Must be used only when
         the Destroy event is bind by the user and masks the default
         Destroy event binding *)

val cTKtoCAMLwidget : string -> any widget
val cCAMLtoTKwidget : 'a widget -> tkArgs

val register : string -> callback:(callback_buffer -> unit) -> unit

(*-*)
val prerr_cbid : cbid -> unit
