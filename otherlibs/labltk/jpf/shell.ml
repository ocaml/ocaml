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
open Unix

(************************************************************* Subshell call *)

let subshell cmd = 
  let r,w = pipe () in
    match fork () with
      0 -> close r; dup2 w stdout; 
           close stderr;
           execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
    | id -> 
        close w; 
        let rc = in_channel_of_descr r in
        let rec it () = try 
            let x = input_line rc in x:: it ()
          with _ -> []
        in 
          let answer = it() in
          close_in rc;  (* because of finalize_channel *)
          let _ = waitpid [] id in answer

