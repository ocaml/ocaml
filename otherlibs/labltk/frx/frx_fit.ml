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
open Camltk

let debug = ref false

let vert wid =
  let newsize = ref 0
  and pending_resize = ref false
  and last_last = ref 0.0 in
  let rec resize () =
    pending_resize := false;
    if !debug then 
      (Printf.eprintf "%s Resize %d\n"
                      (Widget.name wid) !newsize; flush stderr);
    Text.configure wid [TextHeight !newsize];
    ()
  and check () = 
    let first, last = Text.yview_get wid in 
      check1 first last

  and check1 first last =
    let curheight = int_of_string (cget wid CHeight) in
      if !debug then begin
         Printf.eprintf "%s C %d %f %f\n" 
                        (Widget.name wid) curheight first last;
         flush stderr
         end;
      if first = 0.0 && last = 1.0 then ()
      (* Don't attempt anything if widget is not visible *)
      else if not (Winfo.viewable wid) then begin
        if !debug then 
          (Printf.eprintf "%s C notviewable\n" (Widget.name wid);
           flush stderr);
        (* Try again later *)
        bind wid [[], Expose] (BindSet ([], fun _ ->
               bind wid [[], Expose] BindRemove;
               check()))
        end
      else  begin
        let delta = 
          if last = 0.0 then 1
          else if last = !last_last then
            (* it didn't change since our last resize ! *)
            1
           else begin
            last_last := last;
            (* never to more than double *)
            let visible = max 0.5 (last -. first) in
            max 1 (truncate (float curheight *. (1. -. visible)))
            end in
        newsize := max (curheight + delta) !newsize;
        if !debug then
           (Printf.eprintf "%s newsize: %d\n" (Widget.name wid) !newsize;
            flush stderr);
        if !pending_resize then ()
        else begin
          pending_resize := true;
          Timer.set 300 (fun () -> Frx_after.idle resize)
          end
        end

    and scroll first last =
      if !debug then
        (Printf.eprintf "%s V %f %f\n" (Widget.name wid) first last;
         flush stderr);
      if first = 0.0 && last = 1.0 then ()
      else check1 first last
    in
      scroll, check
