let copyline input output =
  let rec copy() = match input_char input with
    | exception End_of_file ->
      output_string output "<end of file>\n"
    | char ->
      output_char output char;
      if char='\n' then () else copy()
  in
  copy();
  flush output

let output_endline output str =
  output_string output str;
  output_char output '\n';
  flush output

let output_env_var output env_var =
  let value = match Sys.getenv_opt env_var with
    | None -> "<no such variable>"
    | Some v -> v
  in
  output_endline stdout value

let options =
[
  ("-i2o",
    Arg.Unit (fun () -> (copyline stdin stdout)),
    "copy one line from stdin to stdout");
  ("-i2e",
    Arg.Unit (fun () -> (copyline stdin stderr)),
    "copy one line from stdin to stderr");
  ("-o",
    Arg.String (output_endline stdout),
    "-o <txt> write <txt> plus newline to stdout");
  ("-e",
    Arg.String (output_endline stderr),
    "-e <txt> write <txt> plus newline to stderr");
  ("-v",
    Arg.String (output_env_var stdout),
    "-v <var>   write value of environment variable <env> to stdout");
]

let report_bad_argument _arg =
  output_endline stderr "<bad argument>"

let () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  set_binary_mode_out stderr true;
  Arg.parse options report_bad_argument  ""
