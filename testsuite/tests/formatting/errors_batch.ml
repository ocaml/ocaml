(* TEST
   include ocamlcommon
*)

let () =
  let open Location in
  (* Some dummy locations for demo purposes *)
  let pos = Lexing.{
    pos_fname = "hello.ml";
    pos_lnum = 18;
    pos_bol = 15;
    pos_cnum = 35;
  } in
  let loc1 = {
    loc_start = pos; loc_end = { pos with pos_cnum = 42 };
    loc_ghost = false
  } in
  let loc2 = {
    loc_start = { pos with pos_lnum = 20; pos_bol = 0; pos_cnum = 4 };
    loc_end = { pos with pos_lnum = 20; pos_bol = 0; pos_cnum = 8 };
    loc_ghost = false
  } in
  let loc3 = {
    loc_start = { pos with pos_lnum = 20; pos_bol = 0; pos_cnum = 6 };
    loc_end = { pos with pos_lnum = 20; pos_bol = 0; pos_cnum = 8 };
    loc_ghost = false
  } in
  let report = {
    kind = Report_error;
    main = msg ~loc:loc1 "%a" Format.pp_print_text
        "These are the contents of the main error message. \
         It is very long and should wrap across several lines.";
    sub = [
      msg ~loc:loc2 "A located first sub-message.";
      msg ~loc:loc3 "%a" Format.pp_print_text
        "Longer sub-messages that do not fit on the \
         same line as the location get indented.";
      msg "@[<v>This second sub-message does not have \
           a location;@,ghost locations of submessages are \
           not printed.@]";
    ]
  } in
  print_report Format.std_formatter report
