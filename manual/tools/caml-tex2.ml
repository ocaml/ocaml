(* $Id$ *)

open Printf
open Str

let camlbegin = "\\caml\n"
let camlend = "\\endcaml\n"
let camlin = "\\\\?\\1"
let camlout = "\\\\:\\1"
let camlbunderline = "\\<"
let camleunderline = "\\>"

let camllight = ref "TERM=dumb ocaml"
let linelen = ref 72
let outfile = ref ""
let cut_at_blanks = ref false
let files = ref []

let _ =
  Arg.parse ["-n", Arg.Int (fun n -> linelen := n), "line length";
             "-o", Arg.String (fun s -> outfile := s), "output";
             "-caml", Arg.String (fun s -> camllight := s), "toplevel";
             "-w", Arg.Set cut_at_blanks, "cut at blanks"]
    (fun s -> files := s :: !files)
    "caml-tex2: "

let (~) =
  let memo = ref [] in
  fun key ->
    try List.assq key !memo
    with Not_found ->
      let data = Str.regexp key in
      memo := (key, data) :: !memo;
      data

let caml_input, caml_output =
  let cmd = !camllight ^ " 2>&1" in
  try Unix.open_process cmd with _ -> failwith "Cannot start toplevel"
let () =
  at_exit (fun () -> ignore (Unix.close_process (caml_input, caml_output)));
  ignore (input_line caml_input);
  ignore (input_line caml_input)

let read_output () =
  let input = ref (input_line caml_input) in
  input := replace_first pat:~"^# *" templ:"" !input;
  let underline =
    if string_match pat:~"Characters *\\([0-9]+\\)-\\([0-9]+\\):$" !input pos:0
    then
      let b = int_of_string (matched_group 1 !input)
      and e = int_of_string (matched_group 2 !input) in
      input := input_line caml_input;
      b, e
    else 0, 0
  in
  let output = Buffer.create 256 in
  while not (string_match pat:~".*\"end_of_input\"$" !input pos:0) do
    prerr_endline !input;
    Buffer.add_string output !input;
    Buffer.add_char output '\n';
    input := input_line caml_input;
  done;
  Buffer.contents output, underline

let escape_backslash = global_replace pat:~"\\\\" templ:"\\\\\\\\"

let process_file file =
  prerr_endline ("Processing " ^ file);
  let ic = try open_in file with _ -> failwith "Cannot read input file" in
  let oc =
    try if !outfile = "-" then
      stdout
    else if !outfile = "" then
      open_out (replace_first pat:~"\\.tex$" templ:"" file ^ ".ml.tex")
    else
      open_out_gen mode:[Open_wronly; Open_creat; Open_append; Open_text]
        perm:0x666 !outfile
    with _ -> failwith "Cannot open output file" in
  let first = ref true in
  try while true do
    let input = ref (input_line ic) in
    if string_match pat:~"\\\\begin{caml_example\\(\\*?\\)}[ \t]*$"
        !input pos:0
    then begin
      let omit_answer = matched_group 1 !input = "*" in
      output_string oc camlbegin;
      let read_phrase () =
        let phrase = Buffer.create 256 in
        while
          let input = input_line ic in
          if string_match pat:~"\\\\end{caml_example\\*?}[ \t]*$"
              input pos:0
          then raise End_of_file;
          Buffer.add_char phrase '\n';
          Buffer.add_string phrase input;
          not (string_match pat:~".*;;[ \t]*$" input pos:0)
        do
          ()
        done;
        Buffer.contents phrase
      in
      try while true do
        let phrase = read_phrase () in
        fprintf caml_output "%s\n" phrase;
        flush caml_output;
        output_string caml_output "\"end_of_input\";;\n";
        flush caml_output;
        let output, (b, e) = read_output () in
        if not omit_answer then begin
          let phrase =
            if b < e then begin
              let start = String.sub phrase pos:0 len:b
              and underlined = String.sub phrase pos:b len:(e-b)
              and rest = String.sub phrase pos:e len:(String.length phrase - e)
              in
              String.concat ""
                [escape_backslash start; "\\<";
                 escape_backslash underlined; "\\>";
                 escape_backslash rest]
            end else
              escape_backslash phrase in
          let phrase = global_replace pat:~"^\(.\)" templ:camlin phrase
          and output = global_replace pat:~"^\(.\)" templ:camlout output in
          if not !first then output_string oc "\\;";
          fprintf oc "%s\n%s" phrase output;
          flush oc;
          first := false
        end
      done
      with End_of_file -> output_string oc camlend
    end
    else if string_match pat:~"\\\\begin{caml_eval}[ \t]*$" !input pos:0
    then begin
      while input := input_line ic; 
        not (string_match pat:~"\\\\end{caml_eval}[ \t]*$"
               !input pos:0)
      do
        fprintf caml_output "%s\n" !input;
        if string_match pat:~".*;;[ \t]*$" !input pos:0 then begin
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
    End_of_file -> close_in ic; close_out oc

let _ =
  if !outfile <> "-" && !outfile <> "" then begin
    try close_out (open_out !outfile)
    with _ -> failwith "Cannot open output file"
  end;
  List.iter process_file (List.rev !files)
