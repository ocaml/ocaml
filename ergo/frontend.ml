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

let open_file file =
   let a = file in  (* val file : decl list *)   
   (* Why_typing.print_file Format.std_formatter a; *)
   let ltd = Why_typing.file a in
   (* List.iter (fun (d, _) -> 
       Why_typing.print_tdecl Format.std_formatter d) ltd; *)
   let lltd = Why_typing.split_goals ltd in
   lltd

let pruning = 
  List.map
    (fun d -> 
       if select > 0 then Pruning.split_and_prune select d 
       else [List.map (fun f -> f,true) d])
    
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
	  env , (consistent, Unsat)
  with 
    | Sat.Sat _ -> 
	env , (consistent, Sat)
    | Sat.Unsat -> 
	env , (false, Inconsistent)
    | Sat.I_dont_know -> 
	env , (consistent, Unknown)

(* this is final result what we want *)
type result = Valid | RUnsat | RUnknown | IDontKnow

let real_output = function 
  | Unsat -> Valid
  | Inconsistent -> RUnsat
  | Unknown -> RUnknown
  | Sat -> IDontKnow

let print_result s = match s with
 | Valid -> print_string "Valid\n"
 | RUnsat -> print_string "Unsat\n"
 | RUnknown -> print_string "Unknown\n"
 | IDontKnow -> print_string "I don't know\n"

let askErgosrc file = 
  smtfile := false;
  try
   let d = open_file file in
   let declss = List.map (List.map fst) d in
   let results = List.map
    (List.map 
       (fun dcl ->
	  let cnf = Cnf.make dcl in 
	  Queue.fold process (Sat.empty,(true, Sat)) cnf
       )) (pruning declss) in
  (* results = [[goal1_result] [goali_result] [goaln_result]]
     results has type [[(env,(consistent,result))]] 
     we want all subgoals to be Valid 
  let allValid =  List.for_all (fun x -> match real_output(snd (snd x)) with
                      Valid -> true | _ -> false)
                  (List.hd results)  in
  if allValid then Valid else IDontKnow *)
  real_output (snd (snd (List.hd (List.hd results)))) 
  with
   Common.Error(e,l) -> 
	printf "typing error: %a\n@." Common.report e;
	(* exit 1 *)
        IDontKnow

(* -- testing --- 
let mklexp e = {pp_loc=Location.none; pp_desc = e}

let lexp = {pp_loc = Location.none; 
            pp_desc = PPinfix (mklexp(PPinfix (mklexp(PPvar "i"), PPge, 
                                        mklexp(PPconst (ConstInt "0")))), 
                      PPimplies , 
                      mklexp(PPinfix (mklexp(PPvar "i"), PPlt , 
                      mklexp(PPinfix (mklexp(PPvar "i"), 
                      PPadd, mklexp(PPconst (ConstInt "1")))))))}

let f_and e1 e2     =  PPinfix (e1, PPand, e2)
let a_list = PPTexternal ([PPTvarid ("'a", Location.none)], "list", Location.none) 
let et = PPconst ConstTrue
let ef = PPconst ConstFalse
let e1 = PPforall (["i"], PPTint, [], lexp)
let cons1 = PPapp ("cons", [mklexp(PPconst (ConstInt "1")); mklexp (PPvar "nil")])
let e2 = PPforall (["x"], a_list, [], mklexp (PPinfix (mklexp (cons1), PPeq, mklexp (PPapp ("nil", [])))))
let e3 = PPforall (["x"], a_list, [], mklexp (PPinfix (mklexp (PPvar "x"), PPeq, mklexp (PPvar "nil"))))

let file = 
  [TypeDecl (Location.none, ["a"], "list");
   Logic (Location.none, false, ["nil"], PFunction ([], a_list));
   Logic (Location.none, false, ["cons"], PFunction ([PPTvarid ("'a", Location.none); a_list], a_list ));
 Axiom (Location.none, "a1", mklexp(e1))
           ;Goal (Location.none, "g2", mklexp(e3))]

let test = let res = askErgo file in
           print_result res
*)
