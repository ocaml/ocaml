(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Paris                  *)
(*             Jacques Garrigue, Nagoya University                        *)
(*             Florian Angeletti                                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@warning "+a-4-6-40..42-44-48"]
open StdLabels
open Str

let camlprefix = "caml"

let latex_escape s = String.concat "" ["$"; s; "$"]
let toplevel_prompt= latex_escape {|\?|} ^ " "

let camlbunderline = "<<"
let camleunderline = ">>"


(** Restrict the number of latex environment *)
type env = Env of string
let main = Env "example"
let input_env = Env "input"
let ok_output = Env "output"
let error = Env "error"
let warning = Env "warn"
let phrase_env = Env ""

let start out (Env s) args =
  Format.fprintf out "\\begin{%s%s}" camlprefix s;
  List.iter (Format.fprintf out "{%s}") args;
  Format.fprintf out "\n"

let stop out (Env s) =
  Format.fprintf out "\\end{%s%s}" camlprefix s;
  Format.fprintf out "\n"

let code_env env out s =
  let sep = if s.[String.length s - 1] = '\n' then "" else "\n" in
  Format.fprintf out "%a%s%s%a"
    (fun ppf env -> start ppf env [])
    env s sep stop env


type example_mode = Toplevel | Verbatim | Signature
let string_of_mode =  function
  | Toplevel -> "toplevel"
  | Verbatim -> "verbatim"
  | Signature -> "signature"


let verbose = ref true
let linelen = ref 72
let outfile = ref ""
let cut_at_blanks = ref false
let files = ref []
let repo_root = ref ""

let (~!) =
  let memo = ref [] in
  fun key ->
    try List.assq key !memo
    with Not_found ->
      let data = Str.regexp key in
      memo := (key, data) :: !memo;
      data

exception Phrase_parsing of string

module Toplevel = struct
  (** Initialize the toplevel loop, redirect stdout and stderr,
      capture warnings and error messages *)

  type output =
    {
      error : string; (** error message text *)
      warnings : string list; (** warning messages text *)
      values : string; (** toplevel output *)
      stdout : string; (** output printed on the toplevel stdout *)
      underlined : (int * int) list
      (** locations to underline in input phrases *)
    }

  let buffer_fmt () =
    let b = Buffer.create 30 in b, Format.formatter_of_buffer b

  let error_fmt = buffer_fmt ()
  let warning_fmt = buffer_fmt ()

  let out_fmt = buffer_fmt ()

  let flush_fmt (b,fmt) =
    Format.pp_print_flush fmt ();
    let r = Buffer.contents b in
    Buffer.reset b;
    r

  (** Redirect the stdout *)
  let stdout_out, stdout_in = Unix.pipe ~cloexec:true ()
  let () = Unix.dup2 stdout_in Unix.stdout

  let self_error_fmt = Format.formatter_of_out_channel stderr
  let eprintf = Format.eprintf

  let read_stdout =
    let size = 50 in
    let b = Bytes.create size in
    let buffer = Buffer.create 100 in
    let rec read_toplevel_stdout () =
      match Unix.select[stdout_out][][] 0. with
      | [_a], _, _ ->
          let n = Unix.read stdout_out b 0 size in
          Buffer.add_subbytes buffer b 0 n;
          if n = size then read_toplevel_stdout ()
      | _  -> ()
    in
    fun () ->
      let () = flush stdout; read_toplevel_stdout () in
      let r = Buffer.contents buffer in
      Buffer.reset buffer;
      r

  (** Store character intervals directly *)
  let locs = ref []
  let register_loc (loc : Location.t) =
    let startchar = loc.loc_start.pos_cnum in
    let endchar = loc.loc_end.pos_cnum in
    if startchar >= 0 then
      locs := (startchar, endchar) :: !locs

  (** Record locations in the main error and suberrors without printing them *)
  let printer_register_locs =
    let base = Location.batch_mode_printer in
    { Location.pp_main_loc = (fun _ _ _ loc -> register_loc loc);
      pp_submsg_loc = (fun _ _ _ loc -> register_loc loc);

      (* The following fields are kept identical to [base],
         listed explicitly so that future field additions result in an error
         -- using (Location.batch_mode_printer with ...) would be the symmetric
         problem to a fragile pattern-matching. *)
      pp = base.pp;
      pp_report_kind = base.pp_report_kind;
      pp_main_txt = base.pp_main_txt;
      pp_submsgs = base.pp_submsgs;
      pp_submsg = base.pp_submsg;
      pp_submsg_txt = base.pp_submsg_txt;
    }

  (** Capture warnings and keep them in a list *)
  let warnings = ref []
  let report_printer =
    (* Extend [printer_register_locs] *)
    let pp self ppf report =
      match report.Location.kind with
      | Location.Report_warning _ | Location.Report_warning_as_error _ ->
          printer_register_locs.pp self (snd warning_fmt) report;
          let w = flush_fmt warning_fmt in
          warnings := w :: !warnings
      | _ ->
          printer_register_locs.pp self ppf report
    in
    { printer_register_locs with pp }

  let fatal ic oc fmt =
    Format.kfprintf
      (fun ppf -> Format.fprintf ppf "@]@."; close_in ic; close_out oc; exit 1)
      self_error_fmt ("@[<hov 2>  Error " ^^ fmt)

  let init () =
    Location.report_printer := (fun () -> report_printer);
    Clflags.color := Some Misc.Color.Never;
    Clflags.no_std_include := true;
    Compenv.last_include_dirs := [Filename.concat !repo_root "stdlib"];
    Compmisc.init_path ~auto_include:Load_path.no_auto_include ();
    try
      Toploop.initialize_toplevel_env ();
      Sys.interactive := false
    with _ ->
      (eprintf "Invalid repo root: %s?%!" !repo_root; exit 2)

  let exec (_,ppf) p =
    try
      ignore @@ Toploop.execute_phrase true ppf p
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      begin try Location.report_exception (snd error_fmt) exn
      with _ ->
        eprintf "Uncaught exception: %s\n%s\n"
          (Printexc.to_string exn)
          (Printexc.raw_backtrace_to_string bt)
      end

  let parse fname mode s =
    let lex = Lexing.from_string s in
    Location.init lex fname;
    Location.input_name := fname;
    Location.input_lexbuf := Some lex;
    try
      match mode with
      | Toplevel -> Parse.toplevel_phrase lex
      | Verbatim -> Parsetree.Ptop_def (Parse.implementation lex)
      | Signature ->
          let sign = Parse.interface lex in
          let name = Location.mknoloc "wrap" in
          let str =
            Ast_helper.[Str.modtype @@ Mtd.mk ~typ:(Mty.signature sign) name] in
          Parsetree.Ptop_def str
    with
    | Lexer.Error _ | Syntaxerr.Error _ ->
        raise (Phrase_parsing s)

  let take x = let r = !x in x := []; r

  let read_output ()  =
    let warnings = take warnings in
    let error = flush_fmt error_fmt in
    let values =
      replace_first ~!{|^#\( *\*\)* *|} "" @@ flush_fmt out_fmt in
    (* the inner ( *\* )* group is here to clean the starting "*"
       introduced for multiline comments *)
    let underlined = take locs in
    let stdout = read_stdout () in
    { values; warnings; error; stdout; underlined }

  (** exec and ignore all output from the toplevel *)
  let eval b =
    let s = Buffer.contents b in
    let ast = Parse.toplevel_phrase (Lexing.from_string s) in
    exec out_fmt ast;
    ignore (read_output());
    Buffer.reset b

end

let () =
  Arg.parse ["-n", Arg.Int (fun n -> linelen := n), "line length";
             "-o", Arg.String (fun s -> outfile := s), "output";
             "-repo-root", Arg.String ((:=) repo_root ), "repo root";
             "-w", Arg.Set cut_at_blanks, "cut at blanks";
             "-v", Arg.Bool (fun b -> verbose := b ), "output result on stderr"
            ]
    (fun s -> files := s :: !files)
    "ocamltex: ";
  Toplevel.init ()


(** The Output module deals with the analysis and classification
    of the interpreter output and the parsing of status-related options
    or annotations for the caml_example environment *)
module Output = struct

  (** Interpreter output status *)
  type status =
    | Ok
    | Warning of int
    | Error

  type kind =
    | Annotation (** Local annotation: [ [@@expect (*annotation*) ] ]*)
    | Option (** Global environment option:
                 [\begin{caml_example}[option[=value]]
                 ...
                 \end{caml_example}] *)

  (** Pretty printer for status *)
  let pp_status ppf = function
    | Error -> Format.fprintf ppf "error"
    | Ok -> Format.fprintf ppf "ok"
    | Warning n -> Format.fprintf ppf "warning %d" n

  (** Pretty printer for status preceded with an undefined determinant *)
  let pp_a_status ppf = function
    | Error -> Format.fprintf ppf "an error"
    | Ok -> Format.fprintf ppf "an ok"
    | Warning n -> Format.fprintf ppf "a warning %d" n

  (** {1 Related latex environment } *)
  let env = function
    | Error -> error
    | Warning _ -> warning
    | Ok -> ok_output

  (** {1 Exceptions } *)
  exception Parsing_error of kind * string

  type source =
    {
      file : string;
      lines : int * int;
      phrase : string;
      output : string
    }
  type unexpected_report = {source : source; expected : status; got : status}
  exception Unexpected_status of unexpected_report

  let print_source ppf {file; lines = (start, stop); phrase; output} =
    Format.fprintf ppf "%s, lines %d to %d:\n\"\n%s\n\"\n\"\n%s\n\"."
      file start stop phrase output

  let print_unexpected {source; expected; got} =
    if expected = Ok then
      Toplevel.eprintf
        "Error when evaluating a caml_example environment in %a\n\
         Unexpected %a status.\n\
         If %a status was expected, add an [@@expect %a] annotation.\n"
        print_source source
        pp_status got
        pp_a_status got
        pp_status got
    else
      Toplevel.eprintf
        "Error when evaluating a guarded caml_example environment in %a\n\
         Unexpected %a status, %a status was expected.\n\
         If %a status was in fact expected, change the status annotation to \
         [@@expect %a].\n"
        print_source source
        pp_status got
        pp_a_status expected
        pp_a_status got
        pp_status got;
    flush stderr

  let print_parsing_error k s =
    match k with
    | Option ->
        Toplevel.eprintf
          "Unknown caml_example option: [%s].\n\
           Supported options are \"ok\",\"error\", or \"warning=n\" (with n \
           a warning number).\n" s
    | Annotation ->
        Toplevel.eprintf
          "Unknown caml_example phrase annotation: [@@expect %s].\n\
           Supported annotations are [@@expect ok], [@@expect error],\n\
           and [@@expect warning n] (with n a warning number).\n" s

  (** {1 Output analysis} *)
  let catch_error = function
    | "" -> None
    | _ -> Some Error

  let catch_warning =
    function
    | [] -> None
    | s :: _ when string_match ~!{|Warning \([0-9]+\)\( \[[a-z-]+\]\)?:|} s 0 ->
        Some (Warning (int_of_string @@ matched_group 1 s))
    | _ -> None

  let status ws es =
    match catch_warning ws, catch_error es with
    | Some w, _ -> w
    | None, Some e -> e
    | None, None -> Ok

  (** {1 Parsing caml_example options } *)

  (** Parse [warning=n] options for caml_example options *)
  let parse_warning s =
    if string_match ~!{|warning=\([0-9]+\)|} s 0 then
      Some (Warning (int_of_string @@ matched_group 1 s))
    else
      None

  (** Parse [warning n] annotations *)
  let parse_local_warning s =
    if string_match ~!{|warning \([0-9]+\)|} s 0 then
      Some (Warning (int_of_string @@ matched_group 1 s))
    else
      None

  let parse_error s =
    if s="error" then Some Error else None

  let parse_ok s =
    if s = "ok" then Some Ok else None

  (** Parse the environment-wide expected status output *)
  let expected s =
    match parse_warning s, parse_error s with
    | Some w, _ -> w
    | None, Some e -> e
    | None, None -> raise (Parsing_error (Option,s))

  (** Parse the local (i.e. phrase-wide) expected status output *)
  let local_expected s =
    match parse_local_warning s, parse_error s, parse_ok s with
    | Some w, _, _ -> w
    | None, Some e, _ -> e
    | None, None, Some ok -> ok
    | None, None, None -> raise (Parsing_error (Annotation,s))

end

module Text_transform = struct

  type kind =
    | Underline
    | Ellipsis

  type t = { kind : kind; start : int; stop : int}
  exception Intersection of
      {
        line : int;
        file : string;
        left : t;
        right : t;
      }

  let pp ppf = function
    | Underline -> Format.fprintf ppf "underline"
    | Ellipsis -> Format.fprintf ppf "ellipsis"

  let underline start stop = { kind = Underline; start; stop}
  let escape_specials s =
    s
    |> global_replace ~!{|\$|} {|$\textdollar$|}

  let rec apply_transform input (pos,underline_stop,out) t =
    if pos >= String.length input then pos, underline_stop, out
    else match underline_stop with
      | Some stop when stop <= t.start ->
          let f = escape_specials (String.sub input ~pos ~len:(stop - pos)) in
          let out =  camleunderline :: f :: out in
          apply_transform input (stop,None,out) t
      | _ ->
          let out =
            escape_specials (String.sub input ~pos ~len:(t.start - pos))::out in
          match t.kind with
          | Ellipsis -> t.stop, underline_stop, latex_escape {|\ldots|} :: out
          | Underline ->
              t.start, Some t.stop, camlbunderline :: out

  (** Merge consecutive transforms:
       - drop nested underline transform
       - raise an error with transforms nested under an ellipsis
       - raise an error when consecutive transforms partially overlap
  *)
  let merge_transforms file line ts =
    let rec merge (active, active_stack, acc) t =
      if active.stop <= t.start then
         (* no overlap, the next transform starts after the end of the current
            active transform *)
        match active_stack with
        | [] ->
            (* there were no other active transforms, the new transform becomes
               the active one *)
            t, [], t :: acc
        | last :: active_stack ->
            (* we check that [t] is still conflict-free with our parent
               transforms *)
            merge (last, active_stack,acc) t
      else if active.stop < t.stop (* not nested *) then
        raise (Intersection {line; file; left = active; right=t})
      else (* nested transforms *)
        match active.kind, t.kind  with
        | Ellipsis, _ -> (* no nesting allowed under an ellipsis *)
            raise (Intersection {line; file; left = active; right=t})
        | Underline, Ellipsis -> (* underlined ellipsis are allowed *)
            (t , active :: active_stack, t :: acc)
        | Underline, Underline ->
            (* multiple underlining are flattened to one *)
            (t, active :: active_stack, acc)
    in
    match ts with
    | [] -> []
    | a :: q ->
        let _, _, ts = List.fold_left ~f:merge ~init:(a,[],[a]) q in
        List.rev ts

  let apply ts file line s =
    (* remove duplicated transforms that can appear due to
        duplicated parse tree elements. For instance,
        [let f : (_ [@ellipsis] = ()] is transformed to
        [let f: (_ [@ellipsis]) = (():(_ [@ellipsis])] with the same location
        for the two ellipses. *)
    let ts = List.sort_uniq compare ts in
    let ts = List.sort (fun x y -> compare x.start y.start) ts in
    let ts = merge_transforms file line ts in
    let last, underline, ls =
      List.fold_left ~f:(apply_transform s) ~init:(0,None,[]) ts in
    let last, ls = match underline with
      | None -> last, ls
      | Some stop ->
          let f = escape_specials (String.sub s ~pos:last ~len:(stop - last)) in
          stop, camleunderline :: f :: ls in
    let ls =
      let n = String.length s in
      if last = n then ls else
        escape_specials (String.sub s last (n-last)) :: ls in
    String.concat "" (List.rev ls)
end


exception Missing_double_semicolon of string * int

exception Missing_mode of string * int

type incompatibility =
  | Signature_with_visible_answer of string * int
exception Incompatible_options of incompatibility


module Ellipsis = struct
  (** This module implements the extraction of ellipsis locations
      from phrases.

      An ellipsis is either an [[@ellipsis]] attribute, or a pair
      of [[@@@ellipsis.start]...[@@@ellipsis.stop]] attributes. *)

  exception Unmatched_ellipsis of {kind : string; start : int; stop : int}
  (** raised when an [[@@@ellipsis.start]] or [[@@@ellipsis.stop]] is
      not paired with another ellipsis attribute *)

  exception Nested_ellipses of {first : int ; second : int}
  (** raised by [[@@@ellipsis.start][@@@ellipsis.start]] *)

  let extract f x =
    let transforms = ref [] in
    let last_loc = ref Location.none in
    let left_mark = ref None (* stored position of [@@@ellipsis.start]*) in
    let location _this loc =
      (* we rely on the fact that the default iterator calls first
         the location subiterator, then the attribute subiterator *)
      last_loc := loc in
    let attribute _this attr =
      let module L = Location in
      let module P = Parsetree in
      let name = attr.P.attr_name.L.txt in
      let loc = !last_loc in
      let start = loc.L.loc_start.Lexing.pos_cnum in
      let attr_start = attr.P.attr_loc.L.loc_start.Lexing.pos_cnum in
      let attr_stop = attr.P.attr_loc.L.loc_end.Lexing.pos_cnum in
      let stop = max loc.L.loc_end.Lexing.pos_cnum attr_stop  in
      let check_nested () = match !left_mark with
        | Some (first,_) -> raise (Nested_ellipses {first; second=attr_start})
        | None -> () in
      match name with
      | "ellipsis" ->
          check_nested ();
          transforms :=
            {Text_transform.kind=Ellipsis; start; stop }
            :: !transforms
      | "ellipsis.start" ->
          check_nested ();
          left_mark := Some (start, stop)
      | "ellipsis.stop" ->
          begin match !left_mark with
          | None -> raise (Unmatched_ellipsis {kind="right"; start; stop})
          | Some (start', stop' ) ->
              let start, stop = min start start', max stop stop' in
              let transform = {Text_transform.kind=Ellipsis; start ; stop } in
              transforms :=  transform :: !transforms;
              left_mark := None
          end
      | _ -> ()
    in
    f {Ast_iterator.default_iterator with location; attribute} x;
    (match !left_mark with
     | None -> ()
     | Some (start,stop) ->
         raise (Unmatched_ellipsis {kind="left"; start; stop })
    );
    !transforms

  let find = function
    | Parsetree.Ptop_def ast -> extract (fun it -> it.structure it) ast
    | Parsetree.Ptop_dir _ -> []

end

let format_input mode s =  match mode with
  | Verbatim | Signature -> s
  | Toplevel ->
      match String.split_on_char '\n' s with
      | [] -> assert false
      | a :: q -> String.concat ~sep:"\n  " ((toplevel_prompt^a)::q)

let process_file file =
  let ic = try open_in file with _ -> failwith "Cannot read input file" in
  let phrase_start = ref 1 and phrase_stop = ref 1 in
  let incr_phrase_start () =
    incr phrase_start;
    phrase_stop := !phrase_start in
  let oc =
    try if !outfile = "-" then
      stdout
    else if !outfile = "" then
      open_out (replace_first ~!"\\.tex$" "" file ^ ".ml.tex")
    else
      open_out_gen [Open_wronly; Open_creat; Open_append; Open_text]
        0x666 !outfile
    with _ -> failwith "Cannot open output file" in
  let tex_fmt = Format.formatter_of_out_channel oc in
  let fatal x = Toplevel.fatal ic oc x in
  let re_spaces = "[ \t]*" in
  let re_start = ~!(
      {|\\begin{caml_example\(\*?\)}|} ^ re_spaces
      ^ {|\({toplevel}\|{verbatim}\|{signature}\)?|} ^ re_spaces
      ^ {|\(\[\(.*\)\]\)?|} ^ re_spaces
      ^ "$"
    ) in
  try while true do
    let input = ref (input_line ic) in
    incr_phrase_start();
    if string_match re_start !input 0
    then begin
      let omit_answer = matched_group 1 !input = "*" in
      let mode =
        match matched_group 2 !input with
        | exception Not_found -> raise (Missing_mode(file, !phrase_stop))
        | "{toplevel}" -> Toplevel
        | "{verbatim}" -> Verbatim
        | "{signature}" -> Signature
        | _ -> assert false in
      if mode = Signature && not omit_answer then raise
          (Incompatible_options(
              Signature_with_visible_answer(file,!phrase_stop))
          );
      let explicit_stop = match mode with
        | Verbatim | Signature -> false
        | Toplevel -> true in
      let global_expected = try Output.expected @@ matched_group 4 !input
        with Not_found -> Output.Ok in
      start tex_fmt main [string_of_mode mode];
      let first = ref true in
      let read_phrase () =
        let phrase = Buffer.create 256 in
        let rec read () =
          let input = incr phrase_stop; input_line ic in
          let implicit_stop =
            if string_match ~!"\\\\end{caml_example\\*?}[ \t]*$"
                input 0
            then
              begin
                if !phrase_stop = 1 + !phrase_start then
                  raise End_of_file
                else if explicit_stop then
                  raise @@ Missing_double_semicolon (file,!phrase_stop)
                else
                  true
              end
            else false in
          if Buffer.length phrase > 0 then Buffer.add_char phrase '\n';
          let stop =
            implicit_stop ||
            ( not (mode = Signature)
              && string_match ~!"\\(.*\\)[ \t]*;;[ \t]*$" input 0 )
          in
          if not stop then (
            Buffer.add_string phrase input; read ()
          )
          else begin
            decr phrase_stop;
            let last_input =
              if implicit_stop then "" else matched_group 1 input in
            let expected =
              if string_match ~!{|\(.*\)\[@@expect \(.*\)\]|} last_input 0 then
                ( Buffer.add_string phrase (matched_group 1 last_input);
                  Output.local_expected @@ matched_group 2 last_input )
              else
                (Buffer.add_string phrase last_input; global_expected)
            in
            if not implicit_stop then Buffer.add_string phrase ";;";
            implicit_stop, Buffer.contents phrase, expected
          end in
        read ()
      in
      try while true do
        let implicit_stop, phrase, expected = read_phrase () in
        let ast = Toplevel.parse file mode phrase in
        let ellipses = Ellipsis.find ast in
        let () = Toplevel.(exec out_fmt) ast in
        let out = Toplevel.read_output () in
        let error_msgs = String.concat "" (out.warnings @ [out.error]) in
        let output = String.concat "" [error_msgs; out.stdout; out.values] in
        let status = Output.status out.warnings out.error in
        if status <> expected then (
          let source = Output.{
              file;
              lines = (!phrase_start, !phrase_stop);
              phrase;
              output
            } in
          raise (Output.Unexpected_status
                   {Output.got=status; expected; source} ) )
        else ( incr phrase_stop; phrase_start := !phrase_stop );
        let phrase =
          let underline =
            List.map (fun (x,y) -> Text_transform.underline x y)
              out.underlined in
          Text_transform.apply (underline @ ellipses)
            file !phrase_stop phrase in
        (* Special characters may also appear in output strings -Didier *)
        let output = Text_transform.escape_specials output in
        let phrase = format_input mode phrase in
        let final_output = if omit_answer then error_msgs else output in
        start tex_fmt phrase_env [];
        code_env input_env tex_fmt phrase;
        if String.length final_output > 0 then
          code_env (Output.env status) tex_fmt final_output;
        stop tex_fmt phrase_env;
        flush oc;
        first := false;
        if implicit_stop then raise End_of_file
      done
      with End_of_file -> phrase_start:= !phrase_stop; stop tex_fmt main
    end
    else if string_match ~!"\\\\begin{caml_eval}[ \t]*$" !input 0
    then begin
      let eval_buffer = Buffer.create 256 in
      while input := input_line ic;
        not (string_match ~!"\\\\end{caml_eval}[ \t]*$" !input 0)
      do
        Buffer.add_string eval_buffer !input;
        Buffer.add_char eval_buffer '\n';
        if string_match ~!".*;;[ \t]*$" !input 0 then begin
          Toplevel.eval eval_buffer
        end
      done;
      if Buffer.length eval_buffer > 0 then
        ( Buffer.add_string eval_buffer ";;\n"; Toplevel.eval eval_buffer )
    end else begin
      Format.fprintf tex_fmt "%s\n" !input;
      Format.pp_print_flush tex_fmt ()
    end
  done with
  | End_of_file -> close_in ic; close_out oc
  | Output.Unexpected_status r ->
          ( Output.print_unexpected r; close_in ic; close_out oc; exit 1 )
  | Output.Parsing_error (k,s) ->
      ( Output.print_parsing_error k s;
        close_in ic; close_out oc; exit 1 )
  | Phrase_parsing s -> fatal "when parsing the following phrase:@ %s" s
  | Missing_double_semicolon (file, line_number) ->
      fatal
        "when evaluating a caml_example environment in %s:@;\
         missing \";;\" at line %d" file (line_number-2)
  | Missing_mode (file, line_number) ->
      fatal "when parsing a caml_example environment in %s:@;\
             missing mode argument at line %d,@ \
             available modes {toplevel,verbatim}"
          file (line_number-2)
  | Incompatible_options Signature_with_visible_answer (file, line_number) ->
      fatal
          "when parsing a caml_example environment in@ \
           %s, line %d:@,\
           the signature mode is only compatible with \"caml_example*\"@ \
           @{<hint>Hint@}: did you forget to add \"*\"?"
          file (line_number-2);
  | Text_transform.Intersection {line;file;left;right} ->
      fatal
        "when evaluating a caml_example environment in %s, line %d:@ \
         Textual transforms must be well-separated.@ The \"%a\" transform \
         spanned the interval %d-%d,@ \
         intersecting with another \"%a\" transform @ \
         on the %d-%d interval.@ \
         @{<hint>Hint@}: did you try to elide a code fragment \
         which raised a warning?"
        file (line-2)
        Text_transform.pp left.kind left.start left.stop
        Text_transform.pp right.kind right.start right.stop
  | Ellipsis.Unmatched_ellipsis {kind;start;stop} ->
      fatal "when evaluating a caml_example environment,@ \
             the %s mark at position %d-%d was unmatched"
        kind start stop
  | Ellipsis.Nested_ellipses {first;second} ->
      fatal "when evaluating a caml_example environment,@ \
             there were two nested ellipsis attribute.@ The first one \
             started at position %d,@ the second one at %d"
        first second

let _ =
  if !outfile <> "-" && !outfile <> "" then begin
    try close_out (open_out !outfile)
    with _ -> failwith "Cannot open output file"
  end;
  List.iter process_file (List.rev !files);
