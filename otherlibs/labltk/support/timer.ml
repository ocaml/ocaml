(* $Id$ *)

(* Timers *)
open Protocol

type tkTimer = int

external internal_add_timer : int -> cbid -> tkTimer
      	=  "camltk_add_timer"
external internal_rem_timer : tkTimer -> unit
        =  "camltk_rem_timer"

type t = tkTimer * cbid	(* the token and the cb id *)

(* A timer is used only once, so we must clean the callback table *)
let add ms:milli callback:f =
  let id = new_function_id () in
  let wrapped _ =
    clear_callback id; (* do it first in case f raises exception *)
    f() in
  Hashtbl.add callback_naming_table key:id data:wrapped;
  if !Protocol.debug then begin
    prerr_cbid id; prerr_endline " for timer"
  end;
  let t = internal_add_timer milli id in
   t,id

(* If the timer has never been used, there is a small space leak in
   the C heap, where a copy of id has been stored *)
let remove (tkTimer, id) =
  internal_rem_timer tkTimer;
  clear_callback id

