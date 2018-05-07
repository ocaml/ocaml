(* $Id$ *)

open StdLabels
open Printf
open Str

let camlbegin = "\\caml"
let camlend = "\\endcaml"
let camlin = {|\\?\1|}
let camlout = {|\\:\1|}
let camlbunderline = "\\<"
let camleunderline = "\\>"

let start newline out s args =
  Printf.fprintf out "%s%s" camlbegin s;
  List.iter (Printf.fprintf out "{%s}") args;
  if newline then Printf.fprintf out "\n"

let stop newline out s =
  Printf.fprintf out "%s%s" camlend s;
  if newline then Printf.fprintf out "\n"

let code_env ?(newline=true) env out s =
  Printf.fprintf out "%a%s\n%a"
    (fun ppf env -> start false ppf env []) env s (stop newline) env

let main = "example"
type example_mode = Toplevel | Verbatim | Signature
let string_of_mode =  function
  | Toplevel -> "toplevel"
  | Verbatim -> "verbatim"
  | Signature -> "signature"

let input_env = "input"
let ok_output ="output"
let error ="error"
let warning ="warn"
let phrase_env = ""


let camllight = ref "TERM=norepeat ocaml"
let verbose = ref true
let linelen = ref 72
let outfile = ref ""
let cut_at_blanks = ref false
let files = ref []

let _ =
  Arg.parse ["-n", Arg.Int (fun n -> linelen := n), "line length";
             "-o", Arg.String (fun s -> outfile := s), "output";
             "-caml", Arg.String (fun s -> camllight := s), "toplevel";
             "-w", Arg.Set cut_at_blanks, "cut at blanks";
             "-v", Arg.Bool (fun b -> verbose := b ), "output result on stderr"
            ]
    (fun s -> files := s :: !files)
    "caml-tex2: "

let (~!) =
  let memo = ref [] in
  fun key ->
    try List.assq key !memo
    with Not_found ->
      let data = Str.regexp key in
      memo := (key, data) :: !memo;
      data

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
    | Error -> Printf.fprintf ppf "error"
    | Ok -> Printf.fprintf ppf "ok"
    | Warning n -> Printf.fprintf ppf "warning %d" n

  (** Pretty printer for status preceded with an undefined determinant *)
  let pp_a_status ppf = function
    | Error -> Printf.fprintf ppf "an error"
    | Ok -> Printf.fprintf ppf "an ok"
    | Warning n -> Printf.fprintf ppf "a warning %d" n

  (** {1 Related latex environment } *)
  let env = function
    | Error -> error
    | Warning _ -> warning
    | Ok -> ok_output

  (** {1 Exceptions } *)
  exception Parsing_error of kind * string

  type source = { file:string; lines:int * int; phrase:string; output:string }
  type unexpected_report = {source:source; expected:status; got:status}
  exception Unexpected_status of unexpected_report

  let print_source ppf {file; lines = (start, stop); phrase; output} =
    Printf.fprintf ppf "%s, lines %d to %d:\n\"\n%s\n\"\n\"\n%s\n\"."
      file start stop phrase output

  let print_unexpected {source; expected; got} =
    if expected = Ok then
      Printf.eprintf
        "Error when evaluating a caml_example environment in %a\n\
         Unexpected %a status.\n\
         If %a status was expected, add an [@@expect %a] annotation.\n"
        print_source source
        pp_status got
        pp_a_status got
        pp_status got
    else
      Printf.eprintf
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
        Printf.eprintf
          "Unknown caml_example option: [%s].\n\
           Supported options are \"ok\",\"error\", or \"warning=n\" (with n \
           a warning number).\n" s
    | Annotation ->
        Printf.eprintf
          "Unknown caml_example phrase annotation: [@@expect %s].\n\
           Supported annotations are [@@expect ok], [@@expect error],\n\
           and [@@expect warning n] (with n a warning number).\n" s

  (** {1 Output analysis} *)
  let catch_error s =
    if string_match ~!{|Error:|} s 0 then Some Error else None

  let catch_warning s =
    if string_match ~!{|Warning \([0-9]+\):|} s 0 then
      Some (Warning (int_of_string @@ matched_group 1 s))
    else
      None

  let status s = match catch_warning s, catch_error s with
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

  exception Intersection of
      {line:int; file:string; left:kind; stop:int; start:int; right:kind}

  let pp ppf = function
    | Underline -> Format.fprintf ppf "underline"
    | Ellipsis -> Format.fprintf ppf "ellipsis"

  type t = { kind:kind; start:int; stop:int}
  let escape_specials s =
    let s1 = global_replace ~!"\\\\" "\\\\\\\\" s in
    let s2 = global_replace ~!"'" "\\\\textquotesingle\\\\-" s1 in
    let s3 = global_replace ~!"`" "\\\\textasciigrave\\\\-" s2 in
    s3

  let rec apply_transform input (pos,underline_stop,out) t =
    if pos >= String.length input then pos, underline_stop, out
    else match underline_stop with
      | Some stop when stop <= t.start ->
          let f = escape_specials (String.sub input ~pos ~len:(stop - pos)) in
          let out =  {|\>|} :: f :: out in
          apply_transform input (stop,None,out) t
      | _ ->
          let out =
            escape_specials (String.sub input ~pos ~len:(t.start - pos))::out in
          match t.kind with
          | Ellipsis -> t.stop, underline_stop, {|\ldots|} :: out
          | Underline ->
              t.start, Some t.stop, {|\<|} :: out

  (** Check that all ellipsis are strictly nested inside underline transform
      and that otherwise no transform starts before the end of the previous
      transform in a list of transforms *)
  type partition = U of t * t list | E of t
  let check_partition line file l =
    let init = Ellipsis, 0 in
    let rec partition = function
      | [] -> []
      | {kind=Underline; _ } as t :: q -> underline t [] q
      | {kind=Ellipsis; _ } as t :: q -> E t :: partition q
    and underline u n = function
      | [] -> end_underline u n []
      | {kind=Underline; _ } :: _ as q -> end_underline u n q
      | {kind=Ellipsis; _ } as t :: q ->
          if t.stop < u.stop then underline u (t::n) q
          else end_underline u n (t::q)
    and end_underline u n l = U(u,List.rev n) :: partition l in
    let check_elt (left,stop) t =
      if t.start < stop then
        raise (Intersection{line;file;left;stop;start=t.start;right=t.kind})
      else
        (t.kind,t.stop) in
    let check acc = function
      | E t -> check_elt acc t
      | U(u,n) ->
          let _ = check_elt acc u in
          let _ = List.fold_left ~f:check_elt ~init n in
          u.kind, u.stop in
    List.fold_left ~f:check ~init (partition l)
    |> ignore

  let apply ts file line s =
    let ts = List.sort (fun x y -> compare x.start y.start) ts in
    check_partition line file ts;
    let last, underline, ls =
      List.fold_left ~f:(apply_transform s) ~init:(0,None,[]) ts in
    let last, ls = match underline with
      | None -> last, ls
      | Some stop ->
          let f = escape_specials (String.sub s ~pos:last ~len:(stop - last)) in
          stop, {|\>|} :: f :: ls in
    let ls =
      let n = String.length s in
      if last = n then ls else
        escape_specials (String.sub s last (n-last)) :: ls in
    String.concat "" (List.rev ls)
end


let caml_input, caml_output =
  let cmd = !camllight ^ " 2>&1" in
  try Unix.open_process cmd with _ -> failwith "Cannot start toplevel"
let () =
  at_exit (fun () -> ignore (Unix.close_process (caml_input, caml_output)));
  ignore (input_line caml_input);
  ignore (input_line caml_input)

let read_output () =
  let input = ref (input_line caml_input) in
  input := replace_first ~!{|^#\( *\*\)* *|} "" !input;
  (* the inner ( *\* )* group is here to clean the starting "*"
     introduced for multiline comments *)
  let underline =
    if string_match ~!"Characters *\\([0-9]+\\)-\\([0-9]+\\):$" !input 0
    then
      let start = int_of_string (matched_group 1 !input)
      and stop = int_of_string (matched_group 2 !input) in
      input := input_line caml_input;
      Text_transform.[{kind=Underline; start; stop}]
    else []
  in
  let output = Buffer.create 256 in
  let first_line = ref true in
  while not (string_match ~!".*\"end_of_input\"$" !input 0) do
    if !verbose then prerr_endline !input;
    if not !first_line then Buffer.add_char output '\n' else first_line:=false;
    Buffer.add_string output !input;
    input := input_line caml_input;
  done;
  Buffer.contents output, underline

exception Missing_double_semicolon of string * int

exception Missing_mode of string * int

type incompatibility =
  | Signature_with_visible_answer of string * int
exception Incompatible_options of incompatibility

exception Phrase_parsing of string

module Ellipsis = struct
  (** This module implements the extraction of ellipsis locations
      from phrases.

      An ellipsis is either an [[@ellipsis]] attribute, or a pair
      of [[@@@ellipsis.start]...[@@@ellipsis.stop]] attributes. *)

  exception Unmatched_ellipsis of {kind:string; start:int; stop:int}
  (** raised when an [[@@@ellipsis.start]] or [[@@@ellipsis.stop]] is
      not paired with another ellipsis attribute *)

  exception Nested_ellipses of {first:int ; second:int }
  (** raised by [[@@@ellipsis.start][@@@ellipsis.start]] *)

  let extract f x =
    let transforms = ref [] in
    let last_loc = ref Location.none in
    let left_mark = ref None (* stored position of [@@@ellipsis.start]*) in
    let location _this loc =
      (* we rely on the fact that the default iterator call first
         the location subiterator, then the attribute subiterator *)
      last_loc := loc in
    let attribute _this (attr,_) =
      let name = attr.Location.txt in
      let loc = !last_loc in
      let start = loc.Location.loc_start.Lexing.pos_cnum in
      let attr_start = attr.Location.loc.loc_start.Lexing.pos_cnum in
      let attr_stop = 1 + attr.Location.loc.loc_end.Lexing.pos_cnum in
      let stop = loc.Location.loc_end.Lexing.pos_cnum in
      let check_nested () = match !left_mark with
        | Some (first,_) -> raise (Nested_ellipses {first; second=attr_start})
        | None -> () in
      match name with
      | "ellipsis" ->
          check_nested ();
          transforms :=
            {Text_transform.kind=Ellipsis; start; stop=max attr_stop stop }
            :: !transforms
      | "ellipsis.start" ->
          check_nested ();
          left_mark := Some (start, stop)
      | "ellipsis.stop" ->
          begin match !left_mark with
          | None -> raise (Unmatched_ellipsis {kind="right"; start; stop})
          | Some (start, _ ) ->
              transforms := {kind=Ellipsis; start ; stop } :: !transforms;
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

  let find fname mode s =
    let lex = Lexing.from_string s in
    Location.init lex fname;
    Location.input_name := fname;
    Location.input_lexbuf := Some lex;
    try
      match mode with
      | Toplevel -> begin
          match Parse.toplevel_phrase lex with
          | Ptop_dir _ -> []
          | Ptop_def str -> extract (fun it -> it.structure it) str
        end
      | Verbatim ->
          extract (fun it -> it.structure it) (Parse.implementation lex)
      | Signature ->
          extract (fun it -> it.signature it) (Parse.interface lex)
    with Syntaxerr.Error _ -> raise (Phrase_parsing s)

end

let process_file file =
  prerr_endline ("Processing " ^ file);
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
  let fatal fmt =
    Format.kfprintf
      (fun ppf -> Format.fprintf ppf "@]@."; close_in ic; close_out oc; exit 1)
      Format.err_formatter ("@[<hov 2>  Error " ^^ fmt) in
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
      start true oc main [string_of_mode mode];
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
            let last_input = if implicit_stop then "" else matched_group 1 input in
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
        let ellipses = Ellipsis.find file mode phrase in
        if mode = Signature then fprintf caml_output "module type Wrap = sig\n";
        fprintf caml_output "%s%s%s" phrase
        (if mode = Signature then "\nend" else "")
        (if implicit_stop then ";;\n" else "\n");
        flush caml_output;
        output_string caml_output "\"end_of_input\";;\n";
        flush caml_output;
        let output, underline = read_output () in
        let status = Output.status output in
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
          Text_transform.apply (underline @ ellipses)
            file !phrase_stop phrase in
        (* Special characters may also appear in output strings -Didier *)
        let output = Text_transform.escape_specials output in
        let phrase = global_replace ~!{|^\(.\)|} camlin phrase
        and output = global_replace ~!{|^\(.\)|} camlout output in
        start false oc phrase_env [];
        code_env ~newline:omit_answer input_env oc phrase;
        if not omit_answer then
          code_env ~newline:false (Output.env status) oc output;
        stop true oc phrase_env;
        flush oc;
        first := false;
        if implicit_stop then raise End_of_file
      done
      with End_of_file -> phrase_start:= !phrase_stop; stop true oc main
    end
    else if string_match ~!"\\\\begin{caml_eval}[ \t]*$" !input 0
    then begin
      while input := input_line ic;
        not (string_match ~!"\\\\end{caml_eval}[ \t]*$" !input 0)
      do
        fprintf caml_output "%s\n" !input;
        if string_match ~!".*;;[ \t]*$" !input 0 then begin
          flush caml_output;
          output_string caml_output "\"end_of_input\";;\n";
          flush caml_output;
          ignore (read_output ())
        end
      done
    end else begin
      fprintf oc "%s\n" !input;
      flush oc
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
         missing \";;\" at line %d@]@." file (line_number-2)
  | Missing_mode (file, line_number) ->
      fatal "when parsing a caml_example environment in %s:@;\
             missing mode argument at line %d,@ \
             available modes {toplevel,verbatim}@]@."
          file (line_number-2)
  | Incompatible_options Signature_with_visible_answer (file, line_number) ->
      fatal
          "when parsing a caml_example environment in@ \
           %s, line %d:@,\
           the signature mode is only compatible with \"caml_example*\"@ \
           Hint: did you forget to add \"*\"?@]@."
          file (line_number-2);
  | Text_transform.Intersection {line;file;left;stop;start;right} ->
      fatal
        "when evaluating a caml_example environment in %s, line %d:@ \
         Textual transforms must be well-separated.@ The \"%a\" transform \
         ended at %d,@ after the start at %d of another \"%a\" transform.@ \
         Hind: did you try to elide a code fragment which raised a warning?\
         @]@."
        file (line-2)
        Text_transform.pp left stop start Text_transform.pp right
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
  List.iter process_file (List.rev !files)
