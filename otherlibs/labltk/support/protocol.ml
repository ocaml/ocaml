(* $Id$ *)

open Widget

type callback_buffer = string list
      (* Buffer for reading callback arguments *)

type tkArgs =
    TkToken of string
  | TkTokenList of tkArgs list		(* to be expanded *)
  | TkQuote of tkArgs			(* mapped to Tcl list *)

type cbid = int

external opentk : string -> string -> unit
        =  "camltk_opentk"
external tcl_eval : string -> string
      	=  "camltk_tcl_eval"
external tk_mainloop : unit -> unit
      	=  "camltk_tk_mainloop"
external tcl_direct_eval : tkArgs array -> string
      	=  "camltk_tcl_direct_eval"
external splitlist : string -> string list
      	= "camltk_splitlist"
external tkreturn : string -> unit
      	= "camltk_return"
external callback_init : unit -> unit
        = "camltk_init"

exception TkError of string
      (* Raised by the communication functions *)
let _ = Callback.register_exception "tkerror" (TkError "")

(* Debugging support *)
let debug = 
 ref (try Sys.getenv "CAMLTKDEBUG"; true
      with Not_found -> false)

(* This is approximative, since we don't quote what needs to be quoted *)
let dump_args args =
  let rec print_arg = function 
    TkToken s -> prerr_string s; prerr_string " "
  | TkTokenList l -> List.iter fun:print_arg l
  | TkQuote a -> prerr_string "{"; print_arg a; prerr_string "} "
 in
  Array.iter fun:print_arg args;
  prerr_newline()

(*
 * Evaluating Tcl code
 *   debugging support should not affect performances...
 *)

let tkEval args = 
  if !debug then dump_args args;
  let res = tcl_direct_eval args in
  if !debug then begin
    prerr_string "->>";
    prerr_endline res
    end;
  res

(*
 * Callbacks
 *)

let cCAMLtoTKwidget w = 
  TkToken (Widget.name w)

let cTKtoCAMLwidget = function
   "" -> raise (Invalid_argument "cTKtoCAMLwidget")
 | s -> Widget.get_atom s


let callback_naming_table = 
   (Hashtbl.create 401 : (int, callback_buffer -> unit) Hashtbl.t) 

let callback_memo_table =
   (Hashtbl.create 401 : (any widget, int) Hashtbl.t)

let new_function_id =
  let counter = ref 0 in
  function () -> incr counter;  !counter

let string_of_cbid = string_of_int

(* Add a new callback, associated to widget w *)
(* The callback should be cleared when w is destroyed *)
let register_callback w callback:f =
  let id = new_function_id () in
    Hashtbl.add callback_naming_table key:id data:f;
    if (forget_type w) <> (forget_type Widget.dummy) then 
      Hashtbl.add callback_memo_table key:(forget_type w) data:id;
    (string_of_cbid id)

let clear_callback id =
  Hashtbl.remove callback_naming_table key:id

(* Clear callbacks associated to a given widget *)
let remove_callbacks w =
  let w = forget_type w in
  let cb_ids = Hashtbl.find_all callback_memo_table key:w in
    List.iter fun:clear_callback cb_ids;
    for i = 1 to List.length cb_ids do
      Hashtbl.remove callback_memo_table key:w
    done

(* Hand-coded callback for destroyed widgets
 * This may be extended by the application, or by other layers of Camltk.
 * Could use bind + of Tk, but I'd rather give an alternate mechanism so
 * that hooks can be set up at load time (i.e. before openTk)
 *)
let destroy_hooks = ref []
let add_destroy_hook f = 
  destroy_hooks := f :: !destroy_hooks

let _ =
  add_destroy_hook (fun w -> remove_callbacks w; Widget.remove w)

let install_cleanup () =
  let call_destroy_hooks = function
      [wname] -> 
      	let w = cTKtoCAMLwidget wname in
	 List.iter fun:(fun f -> f w) !destroy_hooks
    | _ -> raise (TkError "bad cleanup callback") in
  let fid = new_function_id () in
  Hashtbl.add callback_naming_table key:fid data:call_destroy_hooks;
  (* setup general destroy callback *)
  tcl_eval ("bind all <Destroy> {camlcb " ^ (string_of_cbid fid) ^" %W}")


let prerr_cbid id =
  prerr_string "camlcb "; prerr_int id

(* The callback dispatch function *)
let dispatch_callback id args =
  if !debug then begin
    prerr_cbid id;
    List.iter fun:(fun x -> prerr_string " "; prerr_string x) args;
    prerr_newline()
    end;
  (Hashtbl.find callback_naming_table key:id) args;
  if !debug then prerr_endline "<<-"

let protected_dispatch id args =
  try
    Printexc.print (dispatch_callback id) args
  with
     Out_of_memory -> raise Out_of_memory
   | Sys.Break -> raise Sys.Break
   | e -> flush Pervasives.stderr

let _ = Callback.register "camlcb" protected_dispatch

(* Make sure the C variables are initialised *)
let _ = callback_init ()

(* Different version of initialisation functions *)
(* Native opentk is [opentk display class]       *)
let openTk () =
  opentk "" "LablTk";
  install_cleanup();
  Widget.default_toplevel

let openTkClass s =
  opentk "" s;
  install_cleanup();
  Widget.default_toplevel

let openTkDisplayClass display:disp cl =
  opentk disp cl;
  install_cleanup();
  Widget.default_toplevel

(* Destroy all widgets, thus cleaning up table and exiting the loop *)
let closeTk () =
  tcl_eval "destroy ."; ()

let mainLoop =
  tk_mainloop 


(* [register tclname f] makes [f] available from Tcl with 
   name [tclname] *)
let register tclname callback:cb =
  let s = register_callback Widget.default_toplevel callback:cb in
    tcl_eval (Printf.sprintf "proc %s {args} {eval {camlcb %s} $args}"
		             tclname s);
    ()
  
