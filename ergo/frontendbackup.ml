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

let _ = 
  Sys.set_signal Sys.sigint 
    (Sys.Signal_handle 
       (fun _ -> print_endline "User wants me to stop."; exit 1))

open Format
open Options

module Time = struct

  open Unix
    
  let u = ref 0.0
    
  let start () = u:=(times()).tms_utime

  let get () = 
    let res = (times()).tms_utime -. !u in
    start();
    res

end

(* let print_status fmt s =  *)
(*   fprintf fmt "%s@." *)
(*     ((function  *)
(* 	| Smt_ast.Unsat -> "unsat"  *)
(* 	| Smt_ast.Unknown -> "unknown" *)
(* 	| Smt_ast.Sat  -> "unknown (sat)") s) *)

type output = Unsat | Inconsistent | Sat | Unknown

let process_decl report (env, consistent) d =
  try
    match d.st_decl with
      | Assume(f,mf) -> 
	  Sat.assume env {Sat.f=f;age=0;name=None;mf=mf;gf=false}, consistent

      |	PredDef f -> 
	  Sat.pred_def env f , consistent

      | Query(n,f,lits)-> 
	  if consistent then
	    begin
	      Sat.unsat env 
		{Sat.f=f;age=0;name=None;mf=true;gf=true} stopb 
	    end;
	  report d Unsat;
	  env , consistent
  with 
    | Sat.Sat _ -> 
	report d Sat;
	env , consistent
    | Sat.Unsat -> 
	report d Inconsistent;
	env , false
    | Sat.I_dont_know -> 
	report d Unknown;
	env , consistent

let open_file file =
   if !smtfile then []
   else
   let a = file in  (* val file : decl list *)   
   Why_typing.print_file Format.std_formatter a;
   let ltd = Why_typing.file a in
   List.iter (fun (d, _) -> 
       Why_typing.print_tdecl Format.std_formatter d)  
   ltd;
   let lltd = Why_typing.split_goals ltd in
   lltd

let pruning = 
  List.map
    (fun d -> 
       if select > 0 then Pruning.split_and_prune select d 
       else [List.map (fun f -> f,true) d])
    
let processing report declss = 
  let declss = List.map (List.map fst) declss in
  List.iter
    (List.iter 
       (fun dcl ->
	  let cnf = Cnf.make dcl in 
	  ignore (Queue.fold (process_decl report) (Sat.empty,true) cnf)
       )) (pruning declss)

let process (env, (consistent, result)) d =
  try
    match d.st_decl with
      | Assume(f,mf) -> 
	  Sat.assume env {Sat.f=f;age=0;name=None;mf=mf;gf=false}, (consistent, result)

      |	PredDef f -> 
	  Sat.pred_def env f , (consistent, result)

      | Query(n,f,lits)-> 
	  if consistent then
	    begin
	      Sat.unsat env 
		{Sat.f=f;age=0;name=None;mf=true;gf=true} stopb 
	    end;
          print_string "I am at Unsat";
	  env , (consistent, Unsat)
  with 
    | Sat.Sat _ -> 
        print_string "I am at Sat";
	env , (consistent, Sat)
    | Sat.Unsat -> 
        print_string "I am at Unsat->Inconsistent";
	env , (false, Inconsistent)
    | Sat.I_dont_know -> 
        print_string "I am at Unknown";
	env , (consistent, Unknown)

let askErgo file = 
  try
   let d = open_file file in
   let declss = List.map (List.map fst) d in
 
   let results = List.map
    (List.map 
       (fun dcl ->
	  let cnf = Cnf.make dcl in 
	  Queue.fold process (Sat.empty,(true, Sat)) cnf
       )) (pruning declss) in
   snd (snd (List.hd (List.hd results)))
(*
  List.iter
    (List.iter 
       (fun dcl ->
	  let cnf = Cnf.make dcl in 
	  ignore (Queue.fold (process_decl report) (Sat.empty,true) cnf)
       )) (pruning declss)
*)
  with
   Common.Error(e,l) -> 
	printf "typing error: %a\n@." Common.report e;
	exit 1
