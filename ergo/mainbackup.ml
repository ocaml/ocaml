(**************************************************************************)
(*                                                                        *)
(*     The Alt-ergo theorem prover                                        *)
(*     Copyright (C) 2006-2010                                            *)
(*                                                                        *)
(*     Sylvain Conchon                                                    *)
(*     Evelyne Contejean                                                  *)
(*     Stephane Lescuyer                                                  *)
(*     Mohamed Iguernelala                                                *)
(*     Alain Mebsout                                                      *)
(*                                                                        *)
(*     CNRS - INRIA - Universite Paris Sud                                *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

open Why_ptree
open Frontend
open Format
open Options


let _ = 
  Sys.set_signal Sys.sigint 
    (Sys.Signal_handle 
       (fun _ -> print_endline "User wants me to stop."; exit 1))


let print_status d s =
  (* let satmode = !smtfile or !satmode in *)
  let satmode = !satmode in 
  match s with
    | Unsat -> 
	if not satmode then Loc.report d.st_loc;
	if satmode then printf "@{<C.F_Red>unsat@}@." 
	else printf "@{<C.F_Green>Valid@} (%2.4f)@." (Time.get())
	  
    | Inconsistent ->
	if not satmode then 
	  (Loc.report d.st_loc; 
	   fprintf fmt "Inconsistent assumption@.")
	else printf "unsat@."
	  
    | Unknown ->
	if not satmode then
	  (Loc.report d.st_loc; printf "I don't know.@.")
	else printf "unknown@."
	  
    | Sat  -> 
	if not satmode then Loc.report d.st_loc;
	if satmode then printf "unknown (sat)@." 
	else printf "I don't know@."
	  
let mklexp e = {pp_loc=(0,0); pp_desc = e}

let lexp = {pp_loc = (1,2); 
            pp_desc = PPinfix (mklexp(PPinfix (mklexp(PPvar "i"), PPge, 
                                        mklexp(PPconst (ConstInt "0")))), 
                      PPimplies , 
                      mklexp(PPinfix (mklexp(PPvar "i"), PPlt , 
                      mklexp(PPinfix (mklexp(PPvar "i"), 
                      PPadd, mklexp(PPconst (ConstInt "1")))))))}

let e1 = PPforall (["i"], PPTint, [], lexp)

let file = [Goal ((1,2), "g1", mklexp(e1))]

(*
let main _ = 
  try 
    let d = open_file file in 
    processing print_status d
  with
     Common.Error(e,l) -> 
	(* Loc.report l; *)
	printf "typing error: %a\n@." Common.report e;
	exit 1
*)

let print_output s = 
  (* let satmode = !smtfile or !satmode in *)
  let satmode = !satmode in 
  match s with
    | Unsat -> 
	if satmode then printf "@{<C.F_Red>unsat@}@." 
	else printf "@{<C.F_Green>Valid@} (%2.4f)@." (Time.get())
	  
    | Inconsistent ->
	if not satmode then 
	  (fprintf fmt "Inconsistent assumption@.")
	else printf "unsat@."
	  
    | Unknown ->
	if not satmode then
	  (printf "I don't know.@.")
	else printf "unknown@."
	  
    | Sat  -> 
	if satmode then printf "unknown (sat)@." 
	else printf "I don't know@."

let status s = match s with
 | Unsat -> "Unsat"
 | Inconsistent -> "Inconsistent"
 | Unknown -> "Unknown"
 | Sat -> "Sat"

let main _ = 

    let res = askErgo file in
    print_string (status res);    
    print_output res

(*  askErgo print_status file *)
  

let _ = main ();



