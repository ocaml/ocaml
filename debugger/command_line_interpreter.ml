(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(************************ Reading and executing commands ***************)

open Format
open Misc
open Instruct
open Unix
open Debugger_config
open Types
open Primitives
open Unix_tools
open Parser
open Parser_aux
open Lexer
open Input_handling
open Debugcom
open Program_loading
open Program_management
open Lexing
open Parameters
open Show_source
open Show_information
open Time_travel
open Events
open Symbols
open Source
open Breakpoints
open Checkpoints
open Frames
open Printval

(** Instructions, variables and infos lists. **)
type dbg_instruction =
  { instr_name: string;                 (* Name of command *)
    instr_prio: bool;                   (* Has priority *)
    instr_action: lexbuf -> unit;       (* What to do *)
    instr_repeat: bool;                 (* Can be repeated *)
    instr_help: string }                (* Help message *)

let instruction_list = ref ([] : dbg_instruction list)

type dbg_variable =
  { var_name: string;                   (* Name of variable *)
    var_action: (lexbuf -> unit) * (unit -> unit); (* Writing, reading fns *)
    var_help: string }                  (* Help message *)

let variable_list = ref ([] : dbg_variable list)

type dbg_info =
  { info_name: string;                  (* Name of info *)
    info_action: lexbuf -> unit;        (* What to do *)
    info_help: string }                 (* Help message *)

let info_list = ref ([] : dbg_info list)

(** Utilities. **)
let error text =
  prerr_endline text;
  raise Toplevel

let eol =
  end_of_line Lexer.lexeme

let matching_elements list name instr =
  filter (function a -> isprefix instr (name a)) !list

let all_matching_instructions =
  matching_elements instruction_list (fun i -> i.instr_name)

(* itz 04-21-96 don't do priority completion in emacs mode *)
(* XL 25-02-97 why? I find it very confusing. *)

let matching_instructions instr =
  let all = all_matching_instructions instr in
  let prio = filter (fun i -> i.instr_prio) all in
  if prio = [] then all else prio

let matching_variables =
  matching_elements variable_list (fun v -> v.var_name)

let matching_infos =
  matching_elements info_list (fun i -> i.info_name)

let find_ident name matcher action alternative lexbuf =
  match identifier_or_eol Lexer.lexeme lexbuf with
    None ->
      alternative ()
  | Some ident ->
      match matcher ident with
        [] ->
          error ("Unknown " ^ name ^ ".")
      | [a] ->
          action a lexbuf
      | _ ->
          error ("Ambiguous " ^ name ^ ".")

let find_variable action alternative lexbuf =
  find_ident "variable name" matching_variables action alternative lexbuf

let find_info action alternative lexbuf =
  find_ident "info command" matching_infos action alternative lexbuf

let add_breakpoint_at_pc pc =
  try
    new_breakpoint (any_event_at_pc pc)
  with Not_found ->
    prerr_string "Can't add breakpoint at pc ";
    prerr_int pc;
    prerr_endline " : no event there.";
    raise Toplevel

let add_breakpoint_after_pc pc =
  let rec try_add n =
    if n < 3 then begin
      try
        new_breakpoint (any_event_at_pc (pc + n * 4))
      with Not_found ->
        try_add (n+1)
    end else begin
      prerr_endline
        "Can't add breakpoint at beginning of function: no event there";
      raise Toplevel
    end
  in try_add 0

let convert_module mdle =
  match mdle with
    Some m ->
      (* Strip .ml extension if any, and capitalize *)
      String.capitalize(if Filename.check_suffix m ".ml"
                        then Filename.chop_suffix m ".ml"
                        else m)
  | None ->
      try
        let (x, _) = current_point () in x
      with Not_found ->
        prerr_endline "Not in a module.";
        raise Toplevel

(** Toplevel. **)
let current_line = ref ""

let interprete_line line =
  current_line := line;
  let lexbuf = Lexing.from_string line in
    try
      match identifier_or_eol Lexer.lexeme lexbuf with
        Some x ->
          begin match matching_instructions x with
             [] ->
               error "Unknown command."
           | [i] ->
               i.instr_action lexbuf;
               resume_user_input ();
               i.instr_repeat
           | l ->
               error "Ambiguous command."
          end
      | None ->
          resume_user_input ();
          false
    with
      Parsing.Parse_error ->
        error "Syntax error."

let line_loop line_buffer =
  resume_user_input ();
  let previous_line = ref "" in
    try
      while true do
        if !loaded then
          History.add_current_time ();
        let new_line = string_trim (line line_buffer) in
          let line =
            if new_line <> "" then
              new_line
            else
              !previous_line
          in
            previous_line := "";
            if interprete_line line then
              previous_line := line
      done
    with
      Exit ->
        stop_user_input ()
    | Sys_error s ->
        prerr_endline ("System error : " ^ s);
        raise Toplevel


(** Instructions. **)
let instr_cd lexbuf =
  let dir = argument_eol argument lexbuf in
    if ask_kill_program () then
      try
        Sys.chdir (expand_path dir)
      with
        Sys_error s ->
          prerr_endline s;
          raise Toplevel

let instr_pwd lexbuf =
  eol lexbuf;
  system "/bin/pwd";
  ()

let instr_dir lexbuf =
  let new_directory = argument_list_eol argument lexbuf in
    if new_directory = [] then begin
      if yes_or_no "Reinitialize directory list" then begin
        Config.load_path := !default_load_path;
        Envaux.reset_cache ();
        flush_buffer_list ()
        end
      end
    else
      List.iter (function x -> add_path (expand_path x)) (List.rev new_directory);
    open_box 2;
    print_string "Directories :";
    List.iter (function x -> print_space(); print_string x) !Config.load_path;
    close_box();
    print_newline ()

let instr_kill lexbuf =
  eol lexbuf;
  if not !loaded then
    (prerr_endline "The program is not being run."; raise Toplevel);
  if (yes_or_no "Kill the program being debugged") then begin
    kill_program ();
    show_no_point()
  end

let instr_run lexbuf =
  eol lexbuf;
  ensure_loaded ();
  reset_named_values();
  run ();
  show_current_event ()

let instr_reverse lexbuf =
  eol lexbuf;
  ensure_loaded ();
  reset_named_values();
  back_run ();
  show_current_event ()

let instr_step lexbuf =
  let step_count =
    match opt_signed_integer_eol Lexer.lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    ensure_loaded ();
    reset_named_values();
    step step_count;
    show_current_event ()

let instr_back lexbuf =
  let step_count =
    match opt_signed_integer_eol Lexer.lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    ensure_loaded ();
    reset_named_values();
    step (-step_count);
    show_current_event ()

let instr_finish lexbuf =
  eol lexbuf;
  ensure_loaded ();
  reset_named_values();
  finish ();
  show_current_event ()

let instr_next lexbuf =
  let step_count =
    match opt_integer_eol Lexer.lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    ensure_loaded ();
    reset_named_values();
    next step_count;
    show_current_event ()

let instr_start lexbuf =
  eol lexbuf;
  ensure_loaded ();
  reset_named_values();
  start ();
  show_current_event ()

let instr_previous lexbuf =
  let step_count =
    match opt_integer_eol Lexer.lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    ensure_loaded ();
    reset_named_values();
    previous step_count;
    show_current_event ()

let instr_goto lexbuf =
  let time = integer_eol Lexer.lexeme lexbuf in
    ensure_loaded ();
    reset_named_values();
    go_to time;
    show_current_event ()

let instr_quit _ =
  raise Exit

let print_variable_list () =
  print_endline "List of variables :";
  List.iter (fun v -> print_string v.var_name; print_space()) !variable_list;
  print_newline ()

let print_info_list () =
  print_endline "List of info commands :";
  List.iter (fun i -> print_string i.info_name; print_space()) !info_list;
  print_newline ()

let instr_complete lexbuf =
  let rec print_list l = 
    try 
      eol lexbuf;
      List.iter (function i -> print_string i; print_newline ()) l
    with _ ->
      remove_file !user_channel
  and match_list lexbuf =
    match identifier_or_eol Lexer.lexeme lexbuf with
      None ->
        List.map (fun i -> i.instr_name) !instruction_list
    | Some x ->
        match matching_instructions x with
          [ {instr_name = ("set" | "show" as i_full)} ] ->
            if x = i_full then begin
              match identifier_or_eol Lexer.lexeme lexbuf with
                Some ident ->
                  begin match matching_variables ident with
                    [v] -> if v.var_name = ident then [] else [v.var_name]
                  | l   -> List.map (fun v -> v.var_name) l
                  end
              | None ->
                  List.map (fun v -> v.var_name) !variable_list
            end
            else [i_full]
        | [ {instr_name = "info"} ] ->
            if x = "info" then begin
              match identifier_or_eol Lexer.lexeme lexbuf with
                Some ident ->
                  begin match matching_infos ident with
                    [i] -> if i.info_name = ident then [] else [i.info_name]
                  | l   -> List.map (fun i -> i.info_name) l
                  end
              | None -> 
                  List.map (fun i -> i.info_name) !info_list
            end
            else ["info"]
        | [ {instr_name = "help"} ] ->
            if x = "help" then match_list lexbuf else ["help"]
        | [ i ] ->
            if x = i.instr_name then [] else [i.instr_name]
        | l ->
            List.map (fun i -> i.instr_name) l
  in
    print_list(match_list lexbuf)

let instr_help lexbuf =
  match identifier_or_eol Lexer.lexeme lexbuf with
    Some x ->
      let print_help nm hlp =
        eol lexbuf;
        print_string nm;
        print_string " : ";
        print_string hlp;
        print_newline ()
      in
        begin match matching_instructions x with
          [] ->
            eol lexbuf;
            print_string "No matching command.";
            print_newline ()
        | [ {instr_name = "set"} ] ->
            find_variable
              (fun v _ ->
                 print_help ("set " ^ v.var_name) ("set " ^ v.var_help))
              (fun () ->
                 print_help "set" "set debugger variable.";
                 print_variable_list ())
              lexbuf
        | [ {instr_name = "show"} ] ->
            find_variable
              (fun v _ ->
                 print_help ("show " ^ v.var_name) ("show " ^ v.var_help))
              (fun () ->
                 print_help "show" "display debugger variable.";
                 print_variable_list ())
              lexbuf
        | [ {instr_name = "info"} ] ->
            find_info
              (fun i _ -> print_help ("info " ^ i.info_name) i.info_help)
              (fun () ->
                 print_help "info" "display infos about the program being debugged.";
                 print_info_list ())
              lexbuf
        | [i] ->
            print_help i.instr_name i.instr_help
        | l ->
            eol lexbuf;
            print_string ("Ambiguous command \"" ^ x ^ "\" : ");
            List.iter
              (fun i -> print_string i.instr_name; print_space())
              l;
            print_newline ()
        end
  | None ->
      print_endline "List of commands :";
      List.iter
        (fun i -> print_string i.instr_name; print_space()) 
        !instruction_list;
      print_newline ()

(* Printing values *)

let print_expr depth ev env expr =
  try
    let (v, ty) = Eval.expression ev env expr in
    print_named_value depth expr v ty env
  with Eval.Error msg ->
    Eval.report_error msg;
    raise Toplevel

let print_command depth lexbuf =
  let exprs = expression_list_eol Lexer.lexeme lexbuf in
  ensure_loaded ();
  let env =
    try
      Envaux.env_of_event !selected_event
    with
      Envaux.Error msg ->
        Envaux.report_error msg;
        raise Toplevel
  in
  List.iter (print_expr depth !selected_event env) exprs

let instr_print lexbuf = print_command !max_printer_depth lexbuf

let instr_display lexbuf = print_command 1 lexbuf

(* Loading of command files *)

let extract_filename arg =
  (* Allow enclosing filename in quotes *)
  let l = String.length arg in
  let pos1 = if l > 0 && arg.[0] = '"' then 1 else 0 in
  let pos2 = if l > 0 && arg.[l-1] = '"' then l-1 else l in
  String.sub arg pos1 (pos2 - pos1)

let instr_source lexbuf =
  let file = extract_filename(argument_eol argument lexbuf)
  and old_state = !interactif
  and old_channel = !user_channel in
    let io_chan =
      try
        io_channel_of_descr (openfile (expand_path file) [O_RDONLY] 0)
      with
        (Unix_error _) as x  -> Unix_tools.report_error x; raise Toplevel
    in
      try
        interactif := false;
        user_channel := io_chan;
        line_loop (Lexing.from_function read_user_input);
        close_io io_chan;
        interactif := old_state;
        user_channel := old_channel
      with
        x ->
          stop_user_input ();
          close_io io_chan;
          interactif := old_state;
          user_channel := old_channel;
          raise x

let instr_set =
  find_variable
    (function {var_action = (funct, _)} -> funct)
    (function () -> prerr_endline "Argument required."; raise Toplevel)

let instr_show =
  find_variable
    (fun {var_action = (_, funct)} lexbuf -> eol lexbuf; funct ())
    (function () ->
       List.iter
         (function {var_name = nm; var_action = (_, funct)} ->
              print_string (nm ^ " : ");
              funct ())
         !variable_list)

let instr_info =
  find_info
    (fun i lexbuf -> i.info_action lexbuf)
    (function () ->
       prerr_endline
         "\"info\" must be followed by the name of an info command.";
       raise Toplevel)

let instr_break lexbuf =
  let argument = break_argument_eol Lexer.lexeme lexbuf in
    ensure_loaded ();
    match argument with
      BA_none ->                                (* break *)
        (match !selected_event with
           Some ev ->
             new_breakpoint ev
         | None ->
             prerr_endline "Can't add breakpoint at this point.";
             raise Toplevel)
    | BA_pc pc ->                               (* break PC *)
        add_breakpoint_at_pc pc
    | BA_function expr ->                       (* break FUNCTION *)
        let env =
          try
            Envaux.env_of_event !selected_event
          with
            Envaux.Error msg ->
              Envaux.report_error msg;
              raise Toplevel
        in
        begin try
          let (v, ty) = Eval.expression !selected_event env expr in
          match (Ctype.repr ty).desc with
            Tarrow (_, _) ->
              add_breakpoint_after_pc (Remote_value.closure_code v)
          | _ ->
              prerr_endline "Not a function.";
              raise Toplevel
        with Eval.Error msg ->
          Eval.report_error msg;
          raise Toplevel
        end
    | BA_pos1 (mdle, line, column) ->         (* break @ [MODULE] LINE [COL] *)
        let module_name = convert_module mdle in
        new_breakpoint
          (try
             let buffer =
               try get_buffer module_name with Not_found ->
                 prerr_endline ("No source file for " ^ module_name ^ ".");
                 raise Toplevel
             in
             match column with
               None ->
                 event_at_pos module_name (fst (pos_of_line buffer line))
             | Some col ->
                 event_near_pos module_name (point_of_coord buffer line col)
           with
             Not_found -> (* event_at_pos / event_near pos *)
               prerr_endline "Can't find any event there.";
               raise Toplevel
           | Out_of_range -> (* pos_of_line / point_of_coord *)
               prerr_endline "Position out of range.";
               raise Toplevel)
    | BA_pos2 (mdle, position) ->             (* break @ [MODULE] # POSITION *)
        try
          new_breakpoint (event_near_pos (convert_module mdle) position)
        with
          Not_found ->
            prerr_endline "Can't find any event there."

let instr_delete lexbuf =
  match integer_list_eol Lexer.lexeme lexbuf with
    [] ->
      if (breakpoints_count () <> 0) & (yes_or_no "Delete all breakpoints")
      then remove_all_breakpoints ()
  | breakpoints ->
      List.iter
        (function x ->
           try
             remove_breakpoint x
           with
             Not_found ->
               ())
        breakpoints

let instr_frame lexbuf =
  let frame_number =
    match opt_integer_eol Lexer.lexeme lexbuf with
      None -> !current_frame
    | Some x -> x
  in
    ensure_loaded ();
    try
      select_frame frame_number;
      show_current_frame true
    with
      Not_found ->
        prerr_endline ("No frame number " ^ (string_of_int frame_number) ^ ".");
        raise Toplevel

let instr_backtrace lexbuf =
  let number =
    match opt_signed_integer_eol Lexer.lexeme lexbuf with
      None -> 0
    | Some x -> x in
  ensure_loaded ();
  match current_report() with
    None | Some {rep_type = Exited | Uncaught_exc} -> ()
  | Some _ ->
      let frame_counter = ref 0 in
      let print_frame first_frame last_frame = function
        None ->
          print_string "(Encountered a function with no debugging information)";
          print_newline();
          false
      | Some event ->
          if !frame_counter >= first_frame then
            show_one_frame !frame_counter event;
          incr frame_counter;
          if !frame_counter >= last_frame then begin
            print_string "(More frames follow)"; print_newline()
          end;
          !frame_counter < last_frame in
      if number = 0 then
        do_backtrace (print_frame 0 max_int)
      else if number > 0 then
        do_backtrace (print_frame 0 number)
      else begin
        let num_frames = stack_depth() in
        if num_frames < 0 then begin
          print_string
            "(Encountered a function with no debugging information)";
          print_newline()
        end else
          do_backtrace (print_frame (num_frames + number) max_int)
      end

let instr_up lexbuf =
  let offset =
    match opt_signed_integer_eol Lexer.lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    ensure_loaded ();
    try
      select_frame (!current_frame + offset);
      show_current_frame true
    with
      Not_found ->
        prerr_endline "No such frame.";
        raise Toplevel

let instr_down lexbuf =
  let offset =
    match opt_signed_integer_eol Lexer.lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    ensure_loaded ();
    try
      select_frame (!current_frame - offset);
      show_current_frame true
    with
      Not_found ->
        prerr_endline "No such frame.";
        raise Toplevel

let instr_last lexbuf =
  let count =
    match opt_signed_integer_eol Lexer.lexeme lexbuf with
      None -> 1
    | Some x -> x
  in
    reset_named_values();
    go_to (History.previous_time count);
    show_current_event ()

let instr_list lexbuf =
  let (mo, beg, e) = list_arguments_eol Lexer.lexeme lexbuf in
    let (curr_mod, point) =
      try
        selected_point ()
      with
        Not_found ->
          ("", -1)
    in
      let mdle = convert_module mo in
        let beginning =
          match beg with
            None when (mo <> None) || (point = -1) ->
              1
          | None ->
              let buffer =
                try get_buffer mdle with Not_found ->
                 prerr_endline ("No source file for " ^ mdle ^ ".");
                 raise Toplevel
              in
              begin try
                max 1 ((snd (line_of_pos buffer point)) - 10)
              with Out_of_range ->
                1
              end
          | Some x -> x
        in
          let en =
            match e with
              None -> beginning + 20
            | Some x -> x
          in
            if mdle = curr_mod then
              show_listing mdle beginning en point
                (current_event_is_before ())
            else
              show_listing mdle beginning en (-1) true

(** Variables. **)
let raw_variable kill name =
  (function
     lexbuf ->
       let argument = argument_eol argument lexbuf in
         if (not kill) or (ask_kill_program ()) then
           name := argument),
  function
    () ->
      print_string !name;
      print_newline ()

let raw_line_variable kill name =
  (function
     lexbuf ->
       let argument = argument_eol line_argument lexbuf in
         if (not kill) or (ask_kill_program ()) then
           name := argument),
  function
    () ->
      print_string !name;
      print_newline ()

let integer_variable kill min msg name =
  (function
    lexbuf ->
      let argument = integer_eol Lexer.lexeme lexbuf in
        if argument < min then
          print_endline msg
        else
          if (not kill) or (ask_kill_program ()) then
            name := argument),
  function
    () ->
      print_int !name;
      print_newline ()

let boolean_variable kill name =
  (function
    lexbuf ->
      let argument =
        match identifier_eol Lexer.lexeme lexbuf with
          "on" -> true
        | "of" | "off" -> false
        | _ -> error "Syntax error."
      in
        if (not kill) or (ask_kill_program ()) then
          name := argument),
  function
    () ->
      print_string (if !name then "on" else "off");
      print_newline ()

let path_variable kill name =
  (function
     lexbuf ->
       let argument = argument_eol argument lexbuf in
         if (not kill) or (ask_kill_program ()) then
           name := (expand_path argument)),
  function
    () ->
      print_string !name;
      print_newline ()

let loading_mode_variable =
  (find_ident
     "loading mode"
     (matching_elements (ref loading_modes) fst)
     (fun (_, mode) lexbuf ->
        eol lexbuf; set_launching_function mode)
     (function () -> error "Syntax error.")),
  function
    () ->
      let rec find =
        function
          [] -> ()
        | (name, funct)::l ->
            if funct == !launching_func then
              print_string name
            else
              find l
      in
        find loading_modes;
        print_newline ()

(** Infos. **)

let info_modules lexbuf =
  eol lexbuf;
  ensure_loaded ();
  print_endline "Used modules :";
  List.iter (function x -> print_string x; print_space()) !modules;
  print_flush ()
(********
  print_endline "Opened modules :";
  if !opened_modules_names = [] then
    print_endline "(no module opened)."
  else
    (List.iter (function x -> print_string x; print_space) !opened_modules_names;
     print_newline ())
*********)

let info_checkpoints lexbuf =
  eol lexbuf;
  if !checkpoints = [] then
    (print_string "No checkpoint."; print_newline ())
  else
    (if !debug_breakpoints then
       (prerr_endline "      Time   Pid Version";
        List.iter
          (function
             {c_time = time; c_pid = pid; c_breakpoint_version = version} ->
               Printf.printf "%10d %5d %d\n" time pid version)
          !checkpoints)
     else
       (print_endline "      Time   Pid";
        List.iter
          (function
             {c_time = time; c_pid = pid} ->
               Printf.printf "%10d %5d\n" time pid)
          !checkpoints))

let info_breakpoints lexbuf =
  eol lexbuf;
  if !breakpoints = [] then
    (print_string "No breakpoint."; print_newline ())
  else
    (print_endline "Num    Address  Where";
     List.iter
       (function (num, {ev_pos = pc; ev_module = md; ev_char = char}) ->
          Printf.printf "%3d %10d  in %s, character %d\n" num pc md char)
       (List.rev !breakpoints))

let info_events lexbuf =
  ensure_loaded ();
  let mdle = convert_module (opt_identifier_eol Lexer.lexeme lexbuf) in
    print_endline ("Module : " ^ mdle);
    print_endline "   Address  Character      Kind      Repr.";
    List.iter
      (fun {ev_pos = pc; ev_char = char; ev_kind = kind; ev_repr = repr} ->
         Printf.printf
           "%10d %10d  %8s %10s\n"
           pc
           char
           (match kind with
               Event_before   -> "before"
             | Event_after _  -> "after"
             | Event_function -> "function"
             | Event_return _ -> "return")
           (match repr with
              Event_none        -> ""
            | Event_parent _    -> "(repr)"
            | Event_child repr  -> string_of_int !repr))
      (events_in_module mdle)

(** User-defined printers **)

let instr_load_printer lexbuf =
  let filename = extract_filename(argument_eol argument lexbuf) in
  try
    Loadprinter.loadfile filename
  with Loadprinter.Error e ->
    Loadprinter.report_error e; raise Toplevel

let instr_install_printer lexbuf =
  let lid = longident_eol Lexer.lexeme lexbuf in
  try
    Loadprinter.install_printer lid
  with Loadprinter.Error e ->
    Loadprinter.report_error e; raise Toplevel

let instr_remove_printer lexbuf =
  let lid = longident_eol Lexer.lexeme lexbuf in
  try
    Loadprinter.remove_printer lid
  with Loadprinter.Error e ->
    Loadprinter.report_error e; raise Toplevel

(** Initialization. **)
let _ =
  instruction_list := [
     { instr_name = "cd"; instr_prio = false;
       instr_action = instr_cd; instr_repeat = true; instr_help =
"set working directory to DIR for debugger and program being debugged." };
     { instr_name = "complete"; instr_prio = false;
       instr_action = instr_complete; instr_repeat = false; instr_help =
"complete word at cursor according to context. Useful for Emacs." };
     { instr_name = "pwd"; instr_prio = false;
       instr_action = instr_pwd; instr_repeat = true; instr_help =
"print working directory." };
     { instr_name = "directory"; instr_prio = false;
       instr_action = instr_dir; instr_repeat = false; instr_help =
"add directory DIR to beginning of search path for source and\n\
interface files.\n\
Forget cached info on source file locations and line positions.\n\
With no argument, reset the search path." };
     { instr_name = "kill"; instr_prio = false;
       instr_action = instr_kill; instr_repeat = true; instr_help =
"kill the program being debugged." };
     { instr_name = "help"; instr_prio = false;
       instr_action = instr_help; instr_repeat = true; instr_help =
"print list of commands." };
     { instr_name = "quit"; instr_prio = false;
       instr_action = instr_quit; instr_repeat = false; instr_help =
"exit the debugger." };
      (* Displacements *)
     { instr_name = "run"; instr_prio = true;
       instr_action = instr_run; instr_repeat = true; instr_help =
"run the program from current position." };
     { instr_name = "reverse"; instr_prio = false;
       instr_action = instr_reverse; instr_repeat = true; instr_help =
"run the program backward from current position." };
     { instr_name = "step"; instr_prio = true;
       instr_action = instr_step; instr_repeat = true; instr_help =
"step program until it reaches the next event.\n\
Argument N means do this N times (or till program stops for another reason)." };
     { instr_name = "backstep"; instr_prio = true;
       instr_action = instr_back; instr_repeat = true; instr_help =
"step program backward until it reaches the previous event.\n\
Argument N means do this N times (or till program stops for another reason)." };
     { instr_name = "goto"; instr_prio = false;
       instr_action = instr_goto; instr_repeat = true; instr_help =
"go to the given time." };
     { instr_name = "finish"; instr_prio = true;
       instr_action = instr_finish; instr_repeat = true; instr_help =
"execute until topmost stack frame returns." };
     { instr_name = "next"; instr_prio = true;
       instr_action = instr_next; instr_repeat = true; instr_help =
"step program until it reaches the next event.\n\
Skip over function calls.\n\
Argument N means do this N times (or till program stops for another reason)." };
     { instr_name = "start"; instr_prio = false;
       instr_action = instr_start; instr_repeat = true; instr_help =
"execute backward until the current function is exited." };
     { instr_name = "previous"; instr_prio = true;
       instr_action = instr_previous; instr_repeat = true; instr_help =
"step program until it reaches the previous event.\n\
Skip over function calls.\n\
Argument N means do this N times (or till program stops for another reason)." };
     { instr_name = "print"; instr_prio = true;
       instr_action = instr_print; instr_repeat = true; instr_help =
"print value of expressions (deep printing)." };
     { instr_name = "display"; instr_prio = true;
       instr_action = instr_display; instr_repeat = true; instr_help =
"print value of expressions (shallow printing)." };
     { instr_name = "source"; instr_prio = false;
       instr_action = instr_source; instr_repeat = true; instr_help =
"read command from file FILE." };
     (* Breakpoints *)
     { instr_name = "break"; instr_prio = false;
       instr_action = instr_break; instr_repeat = false; instr_help =
"Set breakpoint at specified line or function." };
     { instr_name = "delete"; instr_prio = false;
       instr_action = instr_delete; instr_repeat = false; instr_help =
"delete some breakpoints.\n\
Arguments are breakpoint numbers with spaces in between.\n\
To delete all breakpoints, give no argument." };
     { instr_name = "set"; instr_prio = false;
       instr_action = instr_set; instr_repeat = false; instr_help =
"--unused--" };
     { instr_name = "show"; instr_prio = false;
       instr_action = instr_show; instr_repeat = true; instr_help =
"--unused--" };
     { instr_name = "info"; instr_prio = false;
       instr_action = instr_info; instr_repeat = true; instr_help =
"--unused--" };
     (* Frames *)
     { instr_name = "frame"; instr_prio = false;
       instr_action = instr_frame; instr_repeat = true; instr_help =
"select and print a stack frame.\n\
With no argument, print the selected stack frame.\n\
An argument specifies the frame to select." };
     { instr_name = "backtrace"; instr_prio = false;
       instr_action = instr_backtrace; instr_repeat = true; instr_help =
"print backtrace of all stack frames, or innermost COUNT frames.\n\
With a negative argument, print outermost -COUNT frames." };
     { instr_name = "bt"; instr_prio = false;
       instr_action = instr_backtrace; instr_repeat = true; instr_help =
"print backtrace of all stack frames, or innermost COUNT frames.\n\
With a negative argument, print outermost -COUNT frames." };
     { instr_name = "up"; instr_prio = false;
       instr_action = instr_up; instr_repeat = true; instr_help =
"select and print stack frame that called this one.\n\
An argument says how many frames up to go." };
     { instr_name = "down"; instr_prio = false;
       instr_action = instr_down; instr_repeat = true; instr_help =
"select and print stack frame called by this one.\n\
An argument says how many frames down to go." };
     { instr_name = "last"; instr_prio = true;
       instr_action = instr_last; instr_repeat = true; instr_help =
"go back to previous time." };
     { instr_name = "list"; instr_prio = false;
       instr_action = instr_list; instr_repeat = true; instr_help =
"list the source code." };
     (* User-defined printers *)
     { instr_name = "load_printer"; instr_prio = false;
       instr_action = instr_load_printer; instr_repeat = false; instr_help =
"load in the debugger a .cmo or .cma file containing printing functions." };
     { instr_name = "install_printer"; instr_prio = false;
       instr_action = instr_install_printer; instr_repeat = false; instr_help =
"use the given function for printing values of its input type.\n\
The code for the function must have previously been loaded in the debugger\n\
using \"load_printer\"." };
     { instr_name = "remove_printer"; instr_prio = false;
       instr_action = instr_remove_printer; instr_repeat = false; instr_help =
"stop using the given function for printing values of its input type." }
];
  variable_list := [
    (* variable name, (writing, reading), help reading, help writing *)
     { var_name = "arguments";
       var_action = raw_line_variable true arguments;
       var_help =
"arguments to give program being debugged when it is started." };
     { var_name = "program";
       var_action = path_variable true program_name;
       var_help =
"name of program to be debugged." };
     { var_name = "loadingmode";
       var_action = loading_mode_variable;
       var_help =
"mode of loading.\n\
It can be either :
  direct : the program is directly called by the debugger.\n\
  runtime : the debugger execute `camlrun -D socket programname arguments'.\n\
  manual : the program is not launched by the debugger,\n\
    but manually by the user." };
     { var_name = "processcount";
       var_action = integer_variable false 1 "Must be > 1."
                                     checkpoint_max_count;
       var_help =
"maximum number of process to keep." };
     { var_name = "checkpoints"; 
       var_action = boolean_variable false make_checkpoints;
       var_help =
"whether to make checkpoints or not." };
     { var_name = "bigstep";
       var_action = integer_variable false 1 "Must be > 1."
                                     checkpoint_big_step;
       var_help =
"step between checkpoints during long displacements." };
     { var_name = "smallstep";
       var_action = integer_variable false 1 "Must be > 1."
                                     checkpoint_small_step;
       var_help =
"step between checkpoints during small displacements." };
     { var_name = "socket";
       var_action = raw_variable true socket_name;
       var_help =
"name of the socket used by communications debugger-runtime." };
     { var_name = "history";
       var_action = integer_variable false 0 "" history_size;
       var_help =
"history size." };
     { var_name = "print_depth";
       var_action = integer_variable false 1 "Must be at least 1"
                                     max_printer_depth;
       var_help =
"maximal depth for printing of values." };
     { var_name = "print_length";
       var_action = integer_variable false 1 "Must be at least 1"
                                     max_printer_steps;
       var_help =
"maximal number of value nodes printed." }];

  info_list :=
    (* info name, function, help *)
    [{ info_name = "modules"; info_action = info_modules; info_help =
"list opened modules." };
     { info_name = "checkpoints"; info_action = info_checkpoints; info_help =
"list checkpoints." };
     { info_name = "breakpoints"; info_action = info_breakpoints; info_help =
"list breakpoints." };
     { info_name = "events"; info_action = info_events; info_help =
"list events in MODULE (default is current module)." }]
