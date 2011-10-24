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

(*let fmt = Format.std_formatter*)
let fmt = Format.err_formatter
let _ = 
  Format.pp_set_tags fmt true;
  Print_color.add_to_format_tag fmt 

let usage = "usage: alt-ergo [options] file.<mlw|smt>"

let bouclage = ref 1
let smt_arrays = ref false
let type_only = ref false
let parse_only = ref false
let stopb = ref 8
let age_limite = ref 10
let debug = ref false
let notriggers = ref false
let dcc = ref false
let duse = ref false
let darrays = ref false
let duf = ref false
let dsat = ref false
let dsats = ref false
let dtyping = ref false
let dconstr = ref false
let dpairs = ref false
let verbose = ref false
let dfm = ref false
let darith = ref false
let dcombine = ref false
let dbitv = ref false
let dac = ref false
let ddispatch = ref false
let dfinite = ref false
let options = ref false
let tracefile = ref ""
let smtfile = ref false
let satmode = ref false
let bjmode = ref false
let glouton = ref false
let triggers_var = ref false
let redondance = ref 2
let astuce = ref false
let select = ref 0
let no_rm_eq_existential = ref false
let nocontracongru = ref false
let finitetest = ref false
let omega = ref false
let arrays = ref false
let pairs = ref false
let term_like_pp = ref false
let types = ref false 
let all_models = ref false
let goal_directed = ref false

let show_version () = Format.printf "Alt-Ergo %s@." "Version.version"; exit 0 

let spec = [
  "-parse-only", Arg.Set parse_only, " stop after parsing";
  "-smt-arrays", Arg.Set smt_arrays, " uses select/store symbols for Arrays";
  "-type-only", Arg.Set type_only , " stop after typing";
  "-notriggers", Arg.Set notriggers, "  trigger inference";
  "-debug", Arg.Set debug, "  sets the debugging flag";
  "-dcc", Arg.Set dcc, "  sets the debugging flag of cc";
  "-duse", Arg.Set duse, "  sets the debugging flag of use";
  "-duf", Arg.Set duf, "  sets the debugging flag of uf";
  "-dfm", Arg.Set dfm, "  sets the debugging flag of Fourier-Moutzkin";
  "-darith", Arg.Set darith, " sets the debugging flag of Arith (without fm)";
  "-dbitv", Arg.Set dbitv, "  sets the debugging flag of bitv";
  "-dac", Arg.Set dac, "  sets the debugging flag of ac";
  "-dsat", Arg.Set dsat, "  sets the debugging flag of sat";
  "-dsats", Arg.Set dsats, "  sets the debugging flag of sat (simple output)";
  "-dtyping", Arg.Set dtyping, "  sets the debugging flag of typing";
  "-types", Arg.Set types, "  sets the debugging flag of types";
  "-dconstr", Arg.Set dconstr, "  sets the debugging flag of constructors";
  "-dpairs", Arg.Set dpairs, "  sets the debugging flag of pairs";
  "-darrays", Arg.Set darrays, "  sets the debugging flag of arrays";
  "-dcombine", Arg.Set dcombine, "  sets the debugging flag of combine";
  "-dfinite", Arg.Set dfinite, "  sets the debugging flag of finite test";
   "-v", Arg.Set verbose, "  sets the verbose mode";
  "-version", Arg.Unit show_version, "  prints the version number";
  "-ddispatch", Arg.Set ddispatch, "  sets the debugging flag of sat";
  "-stop", Arg.Set_int stopb, " <n> set the stop bound";
  "-age-limite", Arg.Set_int age_limite, " <n> set the age limite bound";
  "-sat" , Arg.Set satmode , " mode sat/unsat";
  "-bj" , Arg.Set bjmode , " mode sat/unsat";
  "-glouton" , Arg.Set glouton, 
  " use ground terms in non-instanciated lemmas";
  "-redondance" , Arg.Set_int redondance, 
  " number of redondant (multi)triggers (2 by default)";
  "-select" , Arg.Set_int select, 
  "k tries to select relevant (at level k) hypotheses for each goal";
  "-triggers-var" , Arg.Set triggers_var , " allows variables as triggers";
  "-cctrace", Arg.Set_string tracefile, 
  " <file> set output file for cc trace ";
  "-no-rm-eq-existential", Arg.Set no_rm_eq_existential, " does not substitute a variable in an existential when an equality gives the value of the variable";
  "-astuce" , Arg.Set astuce, "";
  "-color" , 
  Arg.Unit (fun () -> Print_color.set_margin_with_term_width fmt;
              Print_color.disable false), "Set ainsi color in output";
  "-nocontracongru", Arg.Set nocontracongru, "";
  "-finitetest",Arg.Set finitetest, "";
  "-omega", Arg.Set omega, "Use omega for arithmetic equalities";
  "-arrays", Arg.Set arrays, "experimental support for the theory of arrays";
  "-pairs", Arg.Set pairs, "experimental support for the theory of pairs";
  "-term-like-pp", Arg.Set term_like_pp, "Output semantic values as terms";
  "-all-models", Arg.Set all_models, "experimental support for model";
  "-goal-directed", Arg.Set goal_directed, " instantiate lemmas only with the terms from the goal";
  "-bouclage", Arg.Set_int bouclage, " number of instantiations at each matching round";

]

(*
let file = ref " stdin"
let cin =
  let ofile = ref None in
  let set_file s =
    if Filename.check_suffix s ".mlw" || Filename.check_suffix s ".why"
    then ofile := Some s
    else
      if Filename.check_suffix s ".smt"
      then begin 
	smtfile := true ; ofile := Some s
      end
      else raise (Arg.Bad "no .mlw or .smt extension");
  in
  Arg.parse spec set_file usage;
  match !ofile with Some f -> file := f ; open_in f 
    | None -> 	smtfile := true ; stdin
*)

let type_only = ! type_only
let parse_only = ! parse_only
let stopb = !stopb
let age_limite = !age_limite
let notriggers = !notriggers
let debug = !debug
let debug_cc = !dcc
let debug_use = !duse
let debug_uf = !duf
let debug_fm = !dfm
let debug_arith = !darith
let debug_bitv = !dbitv
let debug_ac   = !dac
let debug_sat = !dsat
let debug_sat_simple = !dsats
let debug_typing = !dtyping
let debug_constr = !dconstr
let debug_pairs = !dpairs
let debug_finite = !dfinite
let verbose = !verbose
let debug_dispatch = !ddispatch
let tracefile = !tracefile
let bjmode = !bjmode
let glouton = !glouton
let triggers_var = !triggers_var
let redondance = !redondance
let astuce = !astuce
let select = !select
let no_rm_eq_existential = !no_rm_eq_existential
let nocontracongru = !nocontracongru
let finitetest = !finitetest
let omega = !omega
let arrays = !arrays
let pairs = !pairs
let term_like_pp = !term_like_pp || true
let debug_arrays = !darrays
let debug_types = !types
let all_models = !all_models
let debug_combine = !dcombine
let smt_arrays = ! smt_arrays
let goal_directed = !goal_directed
let bouclage = ! bouclage
