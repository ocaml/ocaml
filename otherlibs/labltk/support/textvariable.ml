(* $Id$ *)

open Protocol

external internal_tracevar : string -> cbid -> unit
      	= "camltk_trace_var"
external internal_untracevar : string -> cbid -> unit
      	= "camltk_untrace_var"
external set : string -> to:string -> unit = "camltk_setvar"
external get : string -> string = "camltk_getvar"


type textVariable = string

(* List of handles *)
let handles = Hashtbl.create 401

let add_handle var cbid = 
  try
    let r = Hashtbl.find handles key:var in
    r := cbid :: !r
  with
    Not_found -> 
      Hashtbl.add handles key:var data:(ref [cbid])

let exceptq x =
  let rec ex acc = function
     [] -> acc
   | y::l when y == x -> ex acc l
   | y::l -> ex (y::acc) l
  in
  ex []

let rem_handle var cbid =
  try
    let r = Hashtbl.find handles key:var in
    match exceptq cbid !r with
      [] -> Hashtbl.remove handles key:var
    | remaining -> r := remaining
  with
    Not_found -> ()

(* Used when we "free" the variable (otherwise, old handlers would apply to
 * new usage of the variable)
 *)
let rem_all_handles var =
  try
    let r = Hashtbl.find handles key:var in
    List.iter fun:(internal_untracevar var) !r;
    Hashtbl.remove handles key:var
  with
    Not_found -> ()


(* Variable trace *)
let handle vname f =
  let id = new_function_id() in
  let wrapped _ =
    clear_callback id;
    rem_handle vname id;
    f() in
  Hashtbl.add callback_naming_table key:id data:wrapped;
  add_handle vname id;
  if !Protocol.debug then begin
    prerr_cbid id; prerr_string " for variable "; prerr_endline vname
  end;
  internal_tracevar vname id

(* Avoid space leak (all variables are global in Tcl) *)
module StringSet =
  Set.Make(struct type t = string let compare = compare end)
let freelist = ref (StringSet.empty)
let memo = Hashtbl.create 101

(* Added a variable v referenced by widget w *)
let add w v =
  let w = Widget.forget_type w in
  let r = 
    try Hashtbl.find memo key:w 
    with
      Not_found -> 
      	let r = ref StringSet.empty in
	  Hashtbl.add memo key:w data:r;
	  r in
   r := StringSet.add !r elt:v

(* to be used with care ! *)
let free v =
  rem_all_handles v;
  freelist := StringSet.add elt:v !freelist

(* Free variables associated with a widget *)
let freew w =
  try
    let r = Hashtbl.find memo key:w in
    StringSet.iter fun:free !r;
    Hashtbl.remove memo key:w 
  with
    Not_found -> ()

let _ = add_destroy_hook freew

(* Allocate a new variable *)
let counter = ref 0
let getv () = 
  let v = 
    if StringSet.is_empty !freelist then begin
      incr counter; 
      "camlv("^ string_of_int !counter ^")"
      end
    else
      let v = StringSet.choose !freelist in
	freelist := StringSet.remove elt:v !freelist;
	v in
    set v to:"";
    v

let create ?on: w () =
  let v = getv() in
  begin
  match w with
     Some w -> add w v
   | None -> ()
  end;
  v

(* to be used with care ! *)
let free v =
  freelist := StringSet.add elt:v !freelist

let cCAMLtoTKtextVariable s = TkToken s

let name s = s
let coerce s = s

