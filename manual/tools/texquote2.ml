type environment =
  | Normal
  | Caml
  | Verbatim of string * string
  | Verbatim_like

let in_quotes = ref false

let is_alpha c =
  ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

let is_prefix prefix str =
  let length_prefix = String.length prefix in
  let length_str = String.length str in
  if length_prefix > length_str
  then false
  else (String.sub str 0 length_prefix) = prefix

let escape = function
  | ' ' | '\n' -> "\\ "
  | '{' -> "{\\char123}"
  | '}' -> "{\\char125}"
  | '^' -> "{\\char94}"
  | '_' -> "{\\char95}"
  | '\\' -> "{\\char92}"
  | '~' -> "{\\char126}"
  | '$' -> "\\$"
  | '&' -> "{\\char38}"
  | '#' -> "\\#"
  | '%' -> "\\%"
  | '\'' -> "{\\textquotesingle}"
  | '`' -> "{\\textasciigrave}"
  | _ -> ""

let process_normal_line line =
  let (verb_mark : char option ref) = ref None in
  let l = String.length line in
  let i = ref 0 in
  while !i<l do
    match !verb_mark with
    | None ->
      (match line.[!i] with
      | '"' ->
        let r = if !in_quotes then "}}" else "{\\machine{" in
        print_string r;
        in_quotes := not !in_quotes;
        incr i;
      | '\\' ->
        if !in_quotes
        then begin
          if (!i < l-1) && (line.[!i+1] = '"' || line.[!i+1] = '\\')
          then incr i;
          let t = escape line.[!i] in
          if t<>"" then print_string t else print_char line.[!i];
          incr i;
        end else if is_prefix "\\verb" (String.sub line !i (l - !i))
                    && not (is_alpha line.[!i+5])
        then begin
          i := !i+5;
          verb_mark := Some line.[!i];
          print_string "\\verb";
          print_char line.[!i];
          incr i;
        end else (print_char '\\'; incr i)
      | _ ->
        if !in_quotes && (escape line.[!i] <> "")
        then print_string (escape line.[!i])
        else print_char line.[!i];
        incr i;
      )
    | Some mark ->
      if line.[!i] = mark
      then verb_mark := None
      else if line.[!i] = '\'' || line.[!i] = '`'
      then Printf.eprintf "Warning: %c found in \\verb\n" line.[!i];
      print_char line.[!i];
      incr i;
  done

let process_line line = function
  | Normal ->
    if is_prefix "\\begin{caml_" line || is_prefix "\\begin{rawhtml}" line
    then (print_string line; Verbatim_like)
    else if is_prefix "\\begin{camlexample}" line
    then (print_endline line; Caml)
    else if is_prefix "\\begin{verbatim}" line
    then begin
      print_string "\\begin{machineenv}";
      (Verbatim ("\\end{verbatim}", "\\end{machineenv}"))
    end else if is_prefix "\\begin{ocamldoccode}" line
    then begin
      print_string "\\begin{ocamldoccode}";
      (Verbatim ("\\end{ocamldoccode}", "\\end{ocamldoccode}"))
    end else begin
      process_normal_line line;
      if !in_quotes
      then print_string (escape '\n')
      else print_newline();
      Normal
    end
  | Caml ->
    print_endline line;
    if is_prefix "\\end{camlexample}" line then Normal else Caml
  | Verbatim (verbatim_end_in, verbatim_end_out) as env ->
    if is_prefix verbatim_end_in line
    then begin
      print_string verbatim_end_out;
      Normal
    end else begin
      for i=0 to (String.length line) - 1 do
        let c = line.[i] in
        let t = escape c in
        if c=' ' || c='\n' || t=""
        then print_char c
        else print_string t
      done;
      print_newline();
      env
    end
  | Verbatim_like ->
    print_endline line;
    if is_prefix "\\end{caml_" line || is_prefix "\\end{rawhtml}" line
    then Normal
    else Verbatim_like

let rec process_input env = match input_line stdin with
  | exception End_of_file -> ()
  | line ->
    let env = process_line line env in
    process_input env

let main() =
  print_endline "% THIS FILE IS GENERATED.";
  print_newline();
  process_input Normal

let _ = main()
