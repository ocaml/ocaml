(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*      Damien Doligez and Francois Rouaix, INRIA Rocquencourt         *)
(*          Ported to Caml Special Light by John Malecki               *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Printf

open Clflags
open Config
open Location
open Misc
open Parsetree

(* Errors specific to the profiler *)
exception Profiler of string

(* Modes *)
let instr_fun    = ref false
and instr_match  = ref false
and instr_if	 = ref false
and instr_loops  = ref false
and instr_try	 = ref false

let cur_point = ref 0
and inchan = ref stdin
and outchan = ref stdout

(* In case we forgot something *)
exception Inversion of int * int

let copy next =
  seek_in !inchan !cur_point;
  let buf = try String.create (next - !cur_point)
            with Invalid_argument _ -> raise (Inversion (!cur_point,next))
  in really_input !inchan buf 0 (next - !cur_point);
     output_string !outchan buf; 
     cur_point := next

let profile_counter = ref 0

let instr_mode = ref false
let insert_action = ref (function x -> () : int -> unit)

(* Producing instrumented code *)
let add_incr_counter mod_name prof_counter =
   fprintf !outchan
           "profile_%s_.(%d) <- profile_%s_.(%d) + 1; "
           mod_name prof_counter mod_name prof_counter

let counters = ref (Array.create 0 0)

(* User defined marker *)
let special_id = ref ""

(* Producing results of profile run *)
let add_val_counter prof_counter =
   fprintf !outchan "(* %s%d *) " !special_id !counters.(prof_counter); 
   ()

(* ************* rewrite ************* *)

(* Producing results of profile run *)
let add_val_counter prof_counter =
   fprintf !outchan "(* %s%d *) " !special_id !counters.(prof_counter); 
   ()

let insert_open {loc_start=st} =
  if !instr_mode then begin
    copy st;
    output_string !outchan "("
    end

and insert_profile {loc_start=st} =
  copy st;
  !insert_action !profile_counter; 
  incr profile_counter

and insert_close {loc_end=fin} =
  if !instr_mode then begin
    copy fin;
    output_string !outchan ")"
    end

(* ************* rewrite ************* *)

let pos_len = ref 0

let init_rewrite modes mod_name =
  cur_point := 0;
  profile_counter := 0;
  if !instr_mode then begin
    fprintf !outchan "let profile_%s_ = Array.create 000000" mod_name;
    pos_len := pos_out !outchan;
    fprintf !outchan 
            " 0;; Profiling.counters := (\"%s\", (\"%s\", profile_%s_)) :: !Profiling.counters;; "
            mod_name modes mod_name
  end

let final_rewrite () =
  copy (in_channel_length !inchan);
  if !instr_mode then begin
  let len = string_of_int !profile_counter
  in if String.length len > 6 then raise(Profiler "too many functions");
     seek_out !outchan (!pos_len - String.length len);
     output_string !outchan len
   end;
  close_out !outchan

let rec rewrite_patexp_list l =
  rewrite_exp_list (List.map snd l)

and rewrite_patlexp_list l =
  rewrite_exp_list (List.map snd l)

and rewrite_labelexp_list l =
  rewrite_exp_list (List.map snd l)

and rewrite_exp_list l =
  List.iter rewrite_exp l

and rewrite_exp sexp =
  match sexp.pexp_desc with
    Pexp_ident lid -> ()
  | Pexp_constant cst -> ()

  | Pexp_let(_, spat_sexp_list, sbody) ->
    rewrite_patexp_list spat_sexp_list;
    rewrite_exp sbody

  | Pexp_function caselist ->
    if !instr_fun then
      rewrite_function caselist
    else
      rewrite_patlexp_list caselist

  | Pexp_match(sarg, caselist) ->
    rewrite_exp sarg;
    if !instr_match then
      rewrite_funmatching caselist
    else
      rewrite_patlexp_list caselist

  | Pexp_try(sbody, caselist) ->
    rewrite_exp sbody;
    if !instr_try then
      rewrite_trymatching caselist
    else
      rewrite_patexp_list caselist

  | Pexp_apply(sfunct, sargs) ->
    rewrite_exp sfunct;
    rewrite_exp_list sargs

  | Pexp_tuple sexpl ->
    rewrite_exp_list sexpl

  | Pexp_construct(_, None) -> ()
  | Pexp_construct(_, Some sarg) ->
    rewrite_exp sarg

  | Pexp_record lid_sexp_list ->
    rewrite_labelexp_list lid_sexp_list

  | Pexp_field(sarg, _) ->
    rewrite_exp sarg

  | Pexp_setfield(srecord, _, snewval) ->
    rewrite_exp srecord;
    rewrite_exp snewval

  | Pexp_array(sargl) ->
    rewrite_exp_list sargl

  | Pexp_ifthenelse(scond, sifso, None) ->
      rewrite_exp scond;
      rewrite_ifbody sifso
  | Pexp_ifthenelse(scond, sifso, Some sifnot) ->
      rewrite_exp scond;
      rewrite_ifbody sifso;
      rewrite_ifbody sifnot
      
  | Pexp_sequence(sexp1, sexp2) ->
    rewrite_exp sexp1;
    rewrite_exp sexp2

  | Pexp_while(scond, sbody) ->
    rewrite_exp scond;
    if !instr_loops then insert_profile sbody.pexp_loc;
    rewrite_exp sbody

  | Pexp_for(_, slow, shigh, _, sbody) ->
    rewrite_exp slow;
    rewrite_exp shigh;
    if !instr_loops then insert_profile sbody.pexp_loc;
    rewrite_exp sbody

  | Pexp_constraint(sarg, _, _) ->
    rewrite_exp sarg

  | Pexp_when(scond, sbody) ->
    rewrite_exp scond;
    rewrite_exp sbody

  | Pexp_send (sobj, _) ->
    rewrite_exp sobj

  | Pexp_new _ -> ()

  | Pexp_setinstvar (_, sarg) ->
    rewrite_exp sarg

  | Pexp_override l ->
      List.iter (fun (_, sexp) -> rewrite_exp sexp) l

and rewrite_ifbody sifbody =
  if !instr_if then begin
    insert_open sifbody.pexp_loc;
    insert_profile sifbody.pexp_loc;
    rewrite_exp sifbody;
    insert_close sifbody.pexp_loc
    end
  else
    rewrite_exp sifbody

(* called only when !instr_fun *)
and rewrite_annotate_exp_list l =
  List.iter
    (function  {pexp_desc = Pexp_when(scond, sbody)} ->
      insert_profile scond.pexp_loc;
      rewrite_exp scond; 
      insert_profile sbody.pexp_loc;
      rewrite_exp sbody
    | sexp ->
      insert_profile sexp.pexp_loc;
      rewrite_exp sexp)
    l

and rewrite_function = function
    [spat, ({pexp_desc = Pexp_function _} as sexp)] -> rewrite_exp sexp
  | l -> rewrite_funmatching l

and rewrite_funmatching l = 
  rewrite_annotate_exp_list (List.map snd l)

and rewrite_trymatching l =
  rewrite_annotate_exp_list (List.map snd l)

(* Rewrite a class definition *)

let rewrite_class_field =
  function
    Pcf_inher (_, _, l, _, _) -> List.iter rewrite_exp l
  | Pcf_val (_, _, _, Some exp, _) -> rewrite_exp exp
  | Pcf_val (_, _, _, None, _) | Pcf_virt _ -> ()
  | Pcf_meth (_, exp, _) -> rewrite_exp exp

let rewrite_class cl =
  List.iter rewrite_class_field cl.pcl_field

(* Rewrite a module expression or structure expression *)

let rec rewrite_mod smod =
  match smod.pmod_desc with
    Pmod_ident lid -> ()
  | Pmod_structure sstr -> List.iter rewrite_str_item sstr
  | Pmod_functor(param, smty, sbody) -> rewrite_mod sbody
  | Pmod_apply(smod1, smod2) -> rewrite_mod smod1; rewrite_mod smod2
  | Pmod_constraint(smod, smty) -> rewrite_mod smod

and rewrite_str_item item =
  match item.pstr_desc with
    Pstr_eval exp -> rewrite_exp exp
  | Pstr_value(_, exps) -> List.iter (function (_,exp) -> rewrite_exp exp) exps
  | Pstr_module(name, smod) -> rewrite_mod smod
  | Pstr_class classes -> List.iter rewrite_class classes
  | _ -> ()

(* Rewrite a .ml file *)
let rewrite_file srcfile =
  inchan := open_in_bin srcfile;
  let lb = Lexing.from_channel !inchan in
  Location.input_name := srcfile;
  List.iter rewrite_str_item (Parse.implementation lb);
  final_rewrite();
  close_in !inchan

(* Setting flags from saved config *)
let set_flags s =
  for i = 0 to String.length s - 1 do
    match String.get s i with
      'f' -> instr_fun := true
    | 'm' -> instr_match := true
    | 'i' -> instr_if := true
    | 'l' -> instr_loops := true
    | 't' -> instr_try := true
    | 'a' -> instr_fun := true; instr_match := true;
      	     instr_if := true; instr_loops := true;
	     instr_try := true
    | _ -> ()
    done

(* Command-line options *)

let modes = ref "fm"
let dumpfile = ref "cslprof.dump"

(* Process a file *)

let process_file filename =
  if not (Filename.check_suffix filename ".ml") then
    raise(Profiler(filename ^ " is not a .ml file"));
  let modname = Filename.basename(Filename.chop_suffix filename ".ml") in
  if !instr_mode then begin
    (* Instrumentation mode *)
    insert_action := add_incr_counter modname;
    set_flags !modes;
    init_rewrite !modes modname;
    rewrite_file filename
  end else begin
    (* Results mode *)
    insert_action := add_val_counter;
    let ic = open_in_bin !dumpfile in
    let allcounters =
      (input_value ic : (string * (string * int array)) list) in
    close_in ic;
    let (modes, cv) =
      try
        List.assoc modname allcounters
      with Not_found ->
        raise(Profiler("Module " ^ modname ^ " not used in this profile."))
    in
    counters := cv;
    set_flags modes;
    init_rewrite modes modname;
    rewrite_file filename
  end

(* Main function *)

open Format

let main () =
  try
    Arg.parse
      ["-instrument", Arg.Set instr_mode;
       "-m", Arg.String (fun s -> modes := s);
       "-f", Arg.String (fun s -> dumpfile := s);
       "-F", Arg.String (fun s -> special_id := s)]
      process_file;
    exit 0
  with x ->
    set_formatter_output stderr;
    open_hovbox 0;
    begin match x with
      Lexer.Error(err, start, stop) ->
        Location.print {loc_start = start; loc_end = stop};
        Lexer.report_error err
    | Parse.Error(start, stop) ->
        Location.print {loc_start = start; loc_end = stop};
        print_string "Syntax error"
    | Profiler msg ->
        print_string msg
    | Inversion(pos, next) ->
        print_string "Internal error: inversion at char "; print_int pos;
        print_string ", "; print_int next
    | Sys_error msg ->
        print_string "I/O error: "; print_string msg
    | _ ->
        close_box(); raise x
    end;
    close_box(); print_newline(); exit 2

let _ = main ()
