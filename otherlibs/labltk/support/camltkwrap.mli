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
module Widget : sig
  type widget = Widget.any Widget.widget
    (* widget is an abstract type *)
  
  val default_toplevel : widget
    (* [default_toplevel] is "." in Tk, the toplevel widget that is
       always existing during a Tk session. Destroying [default_toplevel]
       ends the main loop
     *)
  
  val atom : parent: widget -> name: string -> widget
    (* [atom parent name] returns the widget [parent.name]. The widget is
       not created. Only its name is returned. In a given parent, there may
       only exist one children for a given name.
       This function should only be used to check the existence of a widget
       with a known name. It doesn't add the widget to the internal tables
       of CamlTk.
     *)
  
  val name : widget -> string
    (* [name w] returns the name (tk "path") of a widget *)
  
  (*--*)
  (* The following functions are used internally.
     There is normally no need for them in users programs
   *)
  
  val known_class : widget -> string
    (* [known_class w] returns the class of a widget (e.g. toplevel, frame),
       as known by the CamlTk interface.
       Not equivalent to "winfo w" in Tk.
     *)
  
  val dummy : widget
    (* [dummy] is a widget used as context when we don't have any.
       It is *not* a real widget.
     *)
            
  val new_atom : parent: widget -> ?name: string -> string -> widget
      (* incompatible with the classic camltk *)

  val get_atom : string -> widget
    (* [get_atom path] returns the widget with Tk path [path] *)
  
  val remove : widget -> unit
    (* [remove w] removes widget from the internal tables *)
  
  (* Subtypes tables *)
  val widget_any_table : string list
  val widget_button_table : string list
  val widget_canvas_table : string list
  val widget_checkbutton_table : string list
  val widget_entry_table : string list
  val widget_frame_table : string list
  val widget_label_table : string list
  val widget_listbox_table : string list
  val widget_menu_table : string list
  val widget_menubutton_table : string list
  val widget_message_table : string list
  val widget_radiobutton_table : string list
  val widget_scale_table : string list
  val widget_scrollbar_table : string list
  val widget_text_table : string list
  val widget_toplevel_table : string list
  
  val chk_sub : string -> 'a list -> 'a -> unit
  val check_class : widget -> string list -> unit
        (* Widget subtyping *)
  
  exception IllegalWidgetType of string
        (* Raised when widget command applied illegally*)

  (* this function is not used, but introduced for the compatibility
     with labltk. useless for camltk users *)
  val coe : 'a Widget.widget -> Widget.any Widget.widget 
end

module Protocol : sig
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
  
  val add_destroy_hook : (widget -> unit) -> unit
  
  
  (* Opening, closing, and mainloop *)
  val default_display : unit -> string
  
  val opentk : unit -> widget
    (* The basic initialization function. [opentk ()] parses automatically
       the command line options and use the tk related options in them
       such as "-display localhost:0" to initialize Tk applications. 
       Consult wish manpage about the supported options. *)
  
  val keywords : (string * Arg.spec * string) list
    (* Command line parsing specification for Arg.parse, which contains
       the standard Tcl/Tk command line options such as "-display" and "-name".
       These Tk command line options are used by opentk *)
  
  val opentk_with_args : string list -> widget
    (* [opentk_with_args argv] invokes [opentk] with the tk related 
       command line options given by [argv] to the executable program. *) 
  
  val openTk : ?display:string -> ?clas:string -> unit -> widget
      (* [openTk ~display:display ~clas:clas ()] is equivalent to
         [opentk ["-display"; display; "-name"; clas]] *)
  
  (* Legacy opentk functions *)
  val openTkClass: string -> widget
      (* [openTkClass class] is equivalent to [opentk ["-name"; class]] *)
  val openTkDisplayClass: string -> string -> widget
      (* [openTkDisplayClass disp class] is equivalent to 
         [opentk ["-display"; disp; "-name"; class]] *)
  
  val closeTk : unit -> unit
  val finalizeTk : unit -> unit 
      (* Finalize tcl/tk before exiting. This function will be automatically 
         called when you call [Pervasives.exit ()] *)
  
  val mainLoop : unit -> unit
  
  
  (* Direct evaluation of tcl code *)
  val tkEval : tkArgs array -> string
  
  val tkCommand : tkArgs array -> unit
  
  (* Returning a value from a Tcl callback *)
  val tkreturn: string -> unit
  
  
  (* Callbacks: this is private *)
  
  type cbid = Protocol.cbid
  
  type callback_buffer = string list
        (* Buffer for reading callback arguments *)
  
  val callback_naming_table : (cbid, callback_buffer -> unit) Hashtbl.t
  (* CAMLTK val callback_memo_table : (widget, cbid) Hashtbl.t *)
  val callback_memo_table : (widget, cbid) Hashtbl.t
        (* Exported for debug purposes only. Don't use them unless you
           know what you are doing *)
  val new_function_id : unit -> cbid
  val string_of_cbid : cbid -> string
  val register_callback : widget -> callback:(callback_buffer -> unit) -> string
        (* Callback support *)
  val clear_callback : cbid -> unit
        (* Remove a given callback from the table *)
  val remove_callbacks : widget -> unit
        (* Clean up callbacks associated to widget. Must be used only when
           the Destroy event is bind by the user and masks the default
           Destroy event binding *)
  
  val cTKtoCAMLwidget : string -> widget
  val cCAMLtoTKwidget : string list -> widget -> tkArgs
  
  val register : string -> callback:(callback_buffer -> unit) -> unit
  
  (*-*)
  val prerr_cbid : cbid -> unit
end

module Textvariable : sig
  open Widget
  open Protocol
  
  type textVariable = Textvariable.textVariable
        (* TextVariable is an abstract type *)
  
  val create : ?on: widget -> unit -> textVariable
        (* Allocation of a textVariable with lifetime associated to widget 
           if a widget is specified *)
  val create_temporary : widget -> textVariable
        (* for backward compatibility
           [create_temporary w] is equivalent to [create ~on:w ()] *)

  val set : textVariable -> string -> unit
        (* Setting the val of a textVariable *)
  val get : textVariable -> string
        (* Reading the val of a textVariable *)
  val name : textVariable -> string
        (* Its tcl name *)
  
  val cCAMLtoTKtextVariable : textVariable -> tkArgs
        (* Internal conversion function *)
  
  val handle : textVariable -> (unit -> unit) -> unit
        (* Callbacks on variable modifications *)
  
  val coerce : string -> textVariable
  
  (*-*)
  val free : textVariable -> unit
end

module Fileevent : sig
  open Unix
  
  val   add_fileinput : file_descr -> (unit -> unit) -> unit
  val   remove_fileinput: file_descr -> unit
  val   add_fileoutput : file_descr -> (unit -> unit) -> unit
  val   remove_fileoutput: file_descr -> unit
        (* see [tk] module *)
end

module Timer : sig
  type t = Timer.t
  
  val add : int -> (unit -> unit) -> t
  val set : int -> (unit -> unit) -> unit
  val remove : t -> unit
end

(* 
Tkwait exists, but is not used in support
module Tkwait : sig
  val internal_tracevis : string -> string -> unit
  val internal_tracedestroy : string -> string -> unit
end
*)
