open Widget

(* Lower level interface *)
exception TkError of string
      (* Raised by the communication functions *)

val debug : bool ref 
      (* When set to true, displays approximation of intermediate Tcl code *)

type tkArgs =
    TkToken of string
  | TkTokenList of tkArgs list		(* to be expanded *)
  | TkQuote of tkArgs			(* mapped to Tcl list *)


(* Misc *)
external splitlist : string -> string list
      	= "camltk_splitlist"

val add_destroy_hook : (any widget -> unit) -> unit


(* Opening, closing, and mainloop *)
val   openTk : unit -> toplevel widget
val   openTkClass: string -> toplevel widget
val   openTkDisplayClass: display:string -> string -> toplevel widget
val   closeTk : unit -> unit
val   mainLoop : unit -> unit


(* Direct evaluation of tcl code *)
val   tkEval : tkArgs array -> string

(* Returning a value from a Tcl callback *)
val   tkreturn: string -> unit


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
