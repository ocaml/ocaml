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

open Code

let debug = ref false
let defined = ref []
let linenum = ref 1

let rec nop = function
  | Line _ -> incr linenum
  | Ifdef (_, _, c1, c2o) ->
      List.iter nop c1;
      begin match c2o with
      | Some c2 -> List.iter nop c2
      | None -> ()
      end
  | _ -> ()
;;

let rec exec lp f = function
  | Line line -> 
      if !debug then 
        prerr_endline (Printf.sprintf "%03d: %s" !linenum 
                         (String.sub line 0 ((String.length line) - 1)));
      f line; incr linenum
  | Ifdef (sw, k, c1, c2o) ->
      if List.mem k !defined = sw then begin
        List.iter (exec lp f) c1;
        begin match c2o with
        | Some c2 -> List.iter nop c2
        | None -> ()
        end;
        lp !linenum
      end else begin
        List.iter nop c1;
        match c2o with
        | Some c2 -> 
            lp !linenum;
            List.iter (exec lp f) c2
        | None -> ()
      end
  | Define k -> defined := k :: !defined
  | Undef k -> 
      defined := List.fold_right (fun k' s ->
        if k = k' then s else k' :: s) [] !defined
;;
