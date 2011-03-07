(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open StdLabels
open Tk
open Parsetree
open Location
open Jg_tk
open Mytypes

(* Optionally preprocess a source file *)

let preprocess ~pp ~ext text =
  let sourcefile = Filename.temp_file "caml" ext in
  begin try
    let oc = open_out_bin sourcefile in
    output_string oc text;
    flush oc;
    close_out oc
  with _ ->
    failwith "Preprocessing error"
  end;
  let tmpfile = Filename.temp_file "camlpp" ext in
  let comm = Printf.sprintf "%s %s > %s" pp sourcefile tmpfile in
  if Ccomp.command comm <> 0 then begin
    Sys.remove sourcefile;
    Sys.remove tmpfile;
    failwith "Preprocessing error"
  end;
  Sys.remove sourcefile;
  tmpfile

exception Outdated_version

let parse_pp ~parse ~wrap ~ext text =
  Location.input_name := "";
  match !Clflags.preprocessor with
    None ->
      let buffer = Lexing.from_string text in
      Location.init buffer "";
      parse buffer
  | Some pp ->
      let tmpfile = preprocess ~pp ~ext text in
      let ast_magic =
        if ext = ".ml" then Config.ast_impl_magic_number
        else Config.ast_intf_magic_number in
      let ic = open_in_bin tmpfile in
      let ast =
        try
          let buffer = String.create (String.length ast_magic) in
          really_input ic buffer 0 (String.length ast_magic);
          if buffer = ast_magic then begin
            ignore (input_value ic);
            wrap (input_value ic)
          end else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
            raise Outdated_version
          else
            raise Exit
        with
          Outdated_version ->
            close_in ic;
            Sys.remove tmpfile;
            failwith "Ocaml and preprocessor have incompatible versions"
        | _ ->
            seek_in ic 0;
            let buffer = Lexing.from_channel ic in
            Location.init buffer "";
            parse buffer
      in
      close_in ic;
      Sys.remove tmpfile;
      ast

let nowarnings = ref false

let f txt =
  let error_messages = ref [] in
  let text = Jg_text.get_all txt.tw
  and env = ref (Env.open_pers_signature "Pervasives" Env.initial) in
  let tl, ew, end_message =
    Jg_message.formatted ~title:"Warnings" ~ppf:Format.err_formatter () in
  Text.tag_remove txt.tw ~tag:"error" ~start:tstart ~stop:tend;
  txt.structure <- [];
  txt.type_info <- [];
  txt.signature <- [];
  txt.psignature <- [];
  ignore (Stypes.get_info ());
  Clflags.annotations := true;

  begin try

    if Filename.check_suffix txt.name ".mli" then
    let psign = parse_pp text ~ext:".mli"
        ~parse:Parse.interface ~wrap:(fun x -> x) in
    txt.psignature <- psign;
    txt.signature <- Typemod.transl_interface txt.name !env psign

    else (* others are interpreted as .ml *)

    let psl = parse_pp text ~ext:".ml"
        ~parse:Parse.use_file ~wrap:(fun x -> [Parsetree.Ptop_def x]) in
    List.iter psl ~f:
    begin function
      Ptop_def pstr ->
        let str, sign, env' = Typemod.type_structure !env pstr Location.none in
        txt.structure <- txt.structure @ str;
        txt.signature <- txt.signature @ sign;
        env := env'
    | Ptop_dir _ -> ()
    end;
    txt.type_info <- Stypes.get_info ();

  with
    Lexer.Error _ | Syntaxerr.Error _
  | Typecore.Error _ | Typemod.Error _
  | Typeclass.Error _ | Typedecl.Error _
  | Typetexp.Error _ | Includemod.Error _
  | Env.Error _ | Ctype.Tags _ | Failure _ as exn ->
      txt.type_info <- Stypes.get_info ();
      let et, ew, end_message = Jg_message.formatted ~title:"Error !" () in
      error_messages := et :: !error_messages;
      let range = match exn with
        Lexer.Error (err, l) ->
          Lexer.report_error Format.std_formatter err; l
      | Syntaxerr.Error err ->
          Syntaxerr.report_error Format.std_formatter err;
          begin match err with
            Syntaxerr.Unclosed(l,_,_,_) -> l
          | Syntaxerr.Applicative_path l -> l
          | Syntaxerr.Other l -> l
          end
      | Typecore.Error (l,err) ->
          Typecore.report_error Format.std_formatter err; l
      | Typeclass.Error (l,err) ->
          Typeclass.report_error Format.std_formatter err; l
      | Typedecl.Error (l, err) ->
          Typedecl.report_error Format.std_formatter err; l
      | Typemod.Error (l,err) ->
          Typemod.report_error Format.std_formatter err; l
      | Typetexp.Error (l,err) ->
          Typetexp.report_error Format.std_formatter err; l
      | Includemod.Error errl ->
          Includemod.report_error Format.std_formatter errl; Location.none
      | Env.Error err ->
          Env.report_error Format.std_formatter err; Location.none
      | Ctype.Tags(l, l') ->
          Format.printf "In this program,@ variant constructors@ `%s and `%s@ have same hash value.@." l l';
          Location.none
      | Failure s ->
          Format.printf "%s.@." s; Location.none
      | _ -> assert false
      in
      end_message ();
      let s = range.loc_start.Lexing.pos_cnum in
      let e = range.loc_end.Lexing.pos_cnum in
      if s < e then
        Jg_text.tag_and_see txt.tw ~start:(tpos s) ~stop:(tpos e) ~tag:"error"
  end;
  end_message ();
  if !nowarnings || Text.index ew ~index:tend = `Linechar (2,0)
  then destroy tl
  else begin
    error_messages := tl :: !error_messages;
    Text.configure ew ~state:`Disabled;
    bind ew ~events:[`Modified([`Double], `ButtonReleaseDetail 1)]
      ~action:(fun _ ->
        try
          let start, ende = Text.tag_nextrange ew ~tag:"sel" ~start:(tpos 0) in
          let s = Text.get ew ~start:(start,[]) ~stop:(ende,[]) in
          let n = int_of_string s in
          Text.mark_set txt.tw ~index:(tpos n) ~mark:"insert";
          Text.see txt.tw ~index:(`Mark "insert", [])
        with _ -> ())
  end;
  !error_messages
