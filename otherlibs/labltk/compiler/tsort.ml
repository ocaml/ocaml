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

open StdLabels

(* Topological Sort.list *)
(* d'apres More Programming Pearls *)

(* node * pred count * successors *)

type 'a entry =
    {node : 'a;
     mutable pred_count : int;
     mutable successors : 'a entry list
     }

type 'a porder = 'a entry list ref

exception Cyclic

let find_entry order node =
  let rec search_entry =
    function 
      [] -> raise Not_found
    | x::l -> if x.node = node then x else search_entry l
  in
  try
    search_entry !order
  with
    Not_found -> let entry = {node = node;
                              pred_count = 0;
                              successors = []} in
                  order := entry::!order;
                  entry

let create () = ref [] 

(* Inverted args because Sort.list builds list in reverse order *)
let add_relation order (succ,pred) =
  let pred_entry = find_entry order pred
  and succ_entry = find_entry order succ in
    succ_entry.pred_count <- succ_entry.pred_count + 1;
    pred_entry.successors <- succ_entry::pred_entry.successors

(* Just add it *)
let add_element order e =
  ignore (find_entry order e)

let sort order =
    let q = Queue.create () 
    and result = ref [] in
    List.iter !order
      ~f:(function {pred_count = n} as node ->
                if n = 0 then Queue.add node q);
    begin try 
      while true do
        let t = Queue.take q in
          result := t.node :: !result;
          List.iter t.successors ~f:
            begin fun s -> 
              let n = s.pred_count - 1 in
              s.pred_count <- n;
              if n = 0 then Queue.add s q
            end
        done
    with
      Queue.Empty -> 
         List.iter !order
           ~f:(fun node -> if node.pred_count <> 0
                              then raise Cyclic)
    end;
    !result
                         
    
