(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open Tk
open Parsetree
open Location
open Jg_tk
open Mytypes

let nowarnings = ref false

let f txt =
  let error_messages = ref [] in
  let text = Jg_text.get_all txt.tw
  and env = ref (Env.open_pers_signature "Pervasives" Env.initial) in
  let tl, ew, end_message =
    Jg_message.formatted ~title:"Warnings" ~ppf:Format.err_formatter () in
  Text.tag_remove txt.tw ~tag:"error" ~start:tstart ~stop:tend;
  begin
  txt.structure <- [];
  txt.signature <- [];
  txt.psignature <- [];
  try

    if Filename.check_suffix txt.name ".mli" then
    let psign = Parse.interface (Lexing.from_string text) in
    txt.psignature <- psign;
    txt.signature <- Typemod.transl_signature !env psign

    else (* others are interpreted as .ml *)

    let psl = Parse.use_file (Lexing.from_string text) in
    List.iter psl ~f:
    begin function
      Ptop_def pstr ->
        let str, sign, env' = Typemod.type_structure !env pstr in
        txt.structure <- txt.structure @ str;
        txt.signature <- txt.signature @ sign;
        env := env'
    | Ptop_dir _ -> ()
    end

  with
    Lexer.Error _ | Syntaxerr.Error _
  | Typecore.Error _ | Typemod.Error _
  | Typeclass.Error _ | Typedecl.Error _
  | Typetexp.Error _ | Includemod.Error _
  | Env.Error _ | Ctype.Tags _ as exn ->
      let et, ew, end_message = Jg_message.formatted ~title:"Error !" () in
      error_messages := et :: !error_messages;
      let s, e = match exn with
        Lexer.Error (err, s, e) ->
          Lexer.report_error Format.std_formatter err; s,e
      | Syntaxerr.Error err ->
          Syntaxerr.report_error Format.std_formatter err;
          let l =
            match err with
              Syntaxerr.Unclosed(l,_,_,_) -> l
            | Syntaxerr.Other l -> l
          in l.loc_start, l.loc_end
      | Typecore.Error (l,err) ->
          Typecore.report_error Format.std_formatter err;
          l.loc_start, l.loc_end
      | Typeclass.Error (l,err) ->
          Typeclass.report_error Format.std_formatter err;
          l.loc_start, l.loc_end
      | Typedecl.Error (l, err) ->
          Typedecl.report_error Format.std_formatter err;
          l.loc_start, l.loc_end
      | Typemod.Error (l,err) ->
          Typemod.report_error Format.std_formatter err; l.loc_start, l.loc_end
      | Typetexp.Error (l,err) ->
          Typetexp.report_error Format.std_formatter err; l.loc_start, l.loc_end
      | Includemod.Error errl ->
          Includemod.report_error Format.std_formatter errl; 0, 0
      | Env.Error err ->
          Env.report_error Format.std_formatter err; 0, 0
      | Ctype.Tags(l, l') ->
          Format.printf "In this program,@ variant constructors@ `%s and `%s@ have same hash value.@." l l'; 0, 0
      | _ -> assert false
      in
      end_message ();
      if s < e then
        Jg_text.tag_and_see txt.tw ~start:(tpos s) ~stop:(tpos e) ~tag:"error"
  end;
  end_message ();
  if !nowarnings or Text.index ew ~index:tend = `Linechar (2,0)
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
