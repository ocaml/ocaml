(* camlp4r q_MLast.cmo *)
(* Id *)

let action_arg s sl =
  function
    Arg.Unit f -> if s = "" then begin f (); Some sl end else None
  | Arg.Set r -> if s = "" then begin r := true; Some sl end else None
  | Arg.Clear r -> if s = "" then begin r := false; Some sl end else None
  | Arg.Rest f -> List.iter f (s :: sl); Some []
  | Arg.String f ->
      if s = "" then
        match sl with
          s :: sl -> f s; Some sl
        | [] -> None
      else begin f s; Some sl end
  | Arg.Int f ->
      if s = "" then
        match sl with
          s :: sl ->
            begin try f (int_of_string s); Some sl with
              Failure "int_of_string" -> None
            end
        | [] -> None
      else
        begin try f (int_of_string s); Some sl with
          Failure "int_of_string" -> None
        end
  | Arg.Float f ->
      if s = "" then
        match sl with
          s :: sl -> f (float_of_string s); Some sl
        | [] -> None
      else begin f (float_of_string s); Some sl end
;;

let common_start s1 s2 =
  let rec loop i =
    if i == String.length s1 || i == String.length s2 then i
    else if s1.[i] == s2.[i] then loop (i + 1)
    else i
  in
  loop 0
;;

let rec parse_arg s sl =
  function
    (name, action, _) :: spec_list ->
      let i = common_start s name in
      if i == String.length name then
        try action_arg (String.sub s i (String.length s - i)) sl action with
          Arg.Bad _ -> parse_arg s sl spec_list
      else parse_arg s sl spec_list
  | [] -> None
;;

let rec parse_aux spec_list anon_fun =
  function
    [] -> []
  | s :: sl ->
      if String.length s > 1 && s.[0] = '-' then
        match parse_arg s sl spec_list with
          Some sl -> parse_aux spec_list anon_fun sl
        | None -> s :: parse_aux spec_list anon_fun sl
      else begin (anon_fun s : unit); parse_aux spec_list anon_fun sl end
;;

let line_of_loc fname (bp, ep) =
  let ic = open_in_bin fname in
  let rec loop lin col cnt =
    if cnt < bp then
      let (lin, col) =
        match input_char ic with
          '\n' -> lin + 1, 0
        | _ -> lin, col + 1
      in
      loop lin col (cnt + 1)
    else lin, col, col + ep - bp
  in
  let r =
    try loop 1 0 0 with
      e ->
        begin try close_in ic with
          _ -> ()
        end;
        raise e
  in
  begin try close_in ic with
    _ -> ()
  end;
  r
;;

let loc_fmt =
  match Sys.os_type with
    "MacOS" ->
      ("File \"%s\"; line %d; characters %d to %d\n### " :
       ('a, 'b, 'c) format)
  | _ -> ("File \"%s\", line %d, characters %d-%d:\n" : ('a, 'b, 'c) format)
;;

let print_location loc =
  if !(Pcaml.input_file) <> "-" then
    let (line, bp, ep) = line_of_loc !(Pcaml.input_file) loc in
    Printf.eprintf loc_fmt !(Pcaml.input_file) line bp ep
  else Printf.eprintf "At location %d-%d\n" (fst loc) (snd loc)
;;

let print_warning loc s = print_location loc; Printf.eprintf "%s\n" s;;

let process pa pr getdir =
  let name = !(Pcaml.input_file) in
  Pcaml.warning := print_warning;
  let ic = if name = "-" then stdin else open_in_bin name in
  let cs = Stream.of_channel ic in
  let clear () = if name = "-" then () else close_in ic in
  let phr =
    try
      let rec loop () =
        let (pl, stopped_at_directive) = Grammar.Entry.parse pa cs in
        if stopped_at_directive then
          begin
            begin match getdir (List.rev pl) with
              Some x ->
                begin match x with
                  loc, "load", Some (MLast.ExStr (_, s)) ->
                    Odyl_main.loadfile s
                | loc, "directory", Some (MLast.ExStr (_, s)) ->
                    Odyl_main.directory s
                | loc, _, _ ->
                    Stdpp.raise_with_loc loc (Stream.Error "bad directive")
                end
            | None -> ()
            end;
            pl @ loop ()
          end
        else pl
      in
      loop ()
    with
      x -> clear (); raise x
  in
  clear (); pr phr
;;

let gind =
  function
    (MLast.SgDir (loc, n, dp), _) :: _ -> Some (loc, n, dp)
  | _ -> None
;;

let gimd =
  function
    (MLast.StDir (loc, n, dp), _) :: _ -> Some (loc, n, dp)
  | _ -> None
;;

let process_intf () = process Pcaml.interf !(Pcaml.print_interf) gind;;
let process_impl () = process Pcaml.implem !(Pcaml.print_implem) gimd;;

type file_kind = Intf | Impl;;
let file_kind = ref Intf;;
let file_kind_of_name name =
  if Filename.check_suffix name ".mli" then Intf
  else if Filename.check_suffix name ".ml" then Impl
  else raise (Arg.Bad ("don't know what to do with " ^ name))
;;

let print_version () =
  Printf.eprintf "Camlp4 version %s\n" Pcaml.version; flush stderr; exit 0
;;

let usage =
  "\
Usage: camlp4 [load-options] [--] [other-options]
Load-options are:
  -I directory  Add directory in search patch for object files.
  -where        Print camlp4 library directory and exit.
  -nolib        No automatic search for object files in library directory.
  <object-file> Load this file in Camlp4 core.
Other-options are:
  <file>        Parse this file."
;;

let initial_spec_list =
  ["-intf", Arg.String (fun x -> file_kind := Intf; Pcaml.input_file := x),
   "<file>  Parse <file> as an interface, whatever its extension.";
   "-impl", Arg.String (fun x -> file_kind := Impl; Pcaml.input_file := x),
   "<file>  Parse <file> as an implementation, whatever its extension.";
   "-unsafe", Arg.Set Ast2pt.fast,
   "      Generate unsafe accesses to array and strings.";
   "-noassert", Arg.Set Pcaml.no_assert,
   "    Don't compile assertion checks.";
   "-verbose", Arg.Set Grammar.error_verbose,
   "     More verbose in parsing errors.";
   "-loc", Arg.String (fun x -> Stdpp.loc_name := x),
   "<name>   Name of the location variable (default: " ^ !(Stdpp.loc_name) ^
     ")";
   "-QD", Arg.String (fun x -> Pcaml.quotation_dump_file := Some x),
   "<file>    Dump quotation expander result in case of syntax error.";
   "-o", Arg.String (fun x -> Pcaml.output_file := Some x),
   "<file>     Output on <file> instead of standard output.";
   "-v", Arg.Unit print_version, "           Print Camlp4 version and exit."]
;;

let anon_fun x = Pcaml.input_file := x; file_kind := file_kind_of_name x;;

let parse spec_list anon_fun remaining_args =
  let spec_list =
    Sort.list (fun (k1, _, _) (k2, _, _) -> k1 >= k2) spec_list
  in
  try parse_aux spec_list anon_fun remaining_args with
    Arg.Bad s ->
      Printf.eprintf "Error: %s\n" s;
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 2
;;

let remaining_args =
  let rec loop l i =
    if i == Array.length Sys.argv then l else loop (Sys.argv.(i) :: l) (i + 1)
  in
  List.rev (loop [] (!(Arg.current) + 1))
;;

let report_error =
  function
    Odyl_main.Error (fname, msg) ->
      Format.print_string "Error while loading \"";
      Format.print_string fname;
      Format.print_string "\": ";
      Format.print_string msg
  | exc -> Pcaml.report_error exc
;;

let go () =
  let arg_spec_list = initial_spec_list @ Pcaml.arg_spec_list () in
  begin match parse arg_spec_list anon_fun remaining_args with
    [] -> ()
  | "-help" :: sl -> Arg.usage arg_spec_list usage; exit 0
  | s :: sl ->
      Printf.eprintf "%s: unknown or misused option\n" s;
      Printf.eprintf "Use option -help for usage\n";
      exit 2
  end;
  try
    if !(Pcaml.input_file) <> "" then
      match !file_kind with
        Intf -> process_intf ()
      | Impl -> process_impl ()
  with
    exc ->
      Format.set_formatter_out_channel stderr;
      Format.open_vbox 0;
      let exc =
        match exc with
          Stdpp.Exc_located ((bp, ep), exc) -> print_location (bp, ep); exc
        | _ -> exc
      in
      report_error exc; Format.close_box (); Format.print_newline (); exit 2
;;

Odyl_main.name := "camlp4";;
Odyl_main.go := go;;
