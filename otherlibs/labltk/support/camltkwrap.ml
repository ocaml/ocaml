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
module Widget = struct
  include Rawwidget
  type widget = raw_any raw_widget

  let default_toplevel = coe default_toplevel
end

module Protocol = struct
  open Widget
  include Protocol
      
  let opentk () = coe (opentk ())
  let opentk_with_args args = coe (opentk_with_args args)
  let openTk ?display ?clas () = coe (openTk ?display ?clas ())

  let cCAMLtoTKwidget table w = 
    Widget.check_class w table; (* we need run time type check of widgets *)
    TkToken (Widget.name w)

  (* backward compatibility *)
  let openTkClass s = coe (openTkClass s)
  let openTkDisplayClass disp c = coe (openTkDisplayClass disp c)
end

module Textvariable = struct
  open Textvariable
  type textVariable = Textvariable.textVariable
  let create = create
  let set = set
  let get = get
  let name = name
  let cCAMLtoTKtextVariable = cCAMLtoTKtextVariable
  let handle tv cbk = handle tv ~callback:cbk
  let coerce = coerce

  (*-*)
  let free = free

  (* backward compatibility *)
  let create_temporary w = create ~on: w ()
end

module Fileevent = struct
  open Fileevent
  let add_fileinput fd callback = add_fileinput ~fd ~callback
  let remove_fileinput fd = remove_fileinput ~fd
  let add_fileoutput fd callback = add_fileoutput ~fd ~callback
  let remove_fileoutput fd = remove_fileoutput ~fd
end

module Timer = struct
  open Timer
  type t = Timer.t
  let add ms callback = add ~ms ~callback
  let set ms callback = set ~ms ~callback
  let remove = remove
end

(* 
Not compiled in support
module Tkwait = Tkwait 
*)
