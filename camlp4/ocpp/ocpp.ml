(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

value buff = ref (String.create 80);
value store len x =
  do {
    if len >= String.length buff.val then
      buff.val := buff.val ^ String.create (String.length buff.val)
    else ();
    buff.val.[len] := x;
    succ len
  }
;
value get_buff len = String.sub buff.val 0 len;

value rec copy_strip_locate cs =
  match cs with parser
  [ [: `'$' :] -> maybe_locate cs
  | [: `c :] -> do { print_char c; copy_strip_locate cs }
  | [: :] -> () ]
and maybe_locate cs =
  match cs with parser
  [ [: `'1'..'9' :] -> locate cs
  | [: :] -> do { print_char '$'; copy_strip_locate cs } ]
and locate cs =
  match cs with parser
  [ [: `'0'..'9' :] -> locate cs
  | [: `':' :] -> inside_locate cs
  | [: :] -> raise (Stream.Error "colon char expected") ]
and inside_locate cs =
  match cs with parser
  [ [: `'$' :] -> copy_strip_locate cs
  | [: `'\\'; `c :] -> do { print_char c; inside_locate cs }
  | [: `c :] -> do { print_char c; inside_locate cs }
  | [: :] -> raise (Stream.Error "end of file in locate directive") ]
;

value nowhere = {
  Lexing.pos_fname = "";
  Lexing.pos_lnum = 0;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = 0
}
;

value quot name pos str =
  let pos = Reloc.shift_pos pos nowhere in
  let exp =
    try
      match Quotation.find name with
      [ Quotation.ExStr f -> f
      | _ -> raise Not_found ]
    with
    [ Not_found ->
        Stdpp.raise_with_loc (pos, Reloc.shift_pos  (String.length str) pos) Not_found ]
  in
  let new_str =
    try exp True str with
    [ Stdpp.Exc_located (p1, p2) exc ->
        Stdpp.raise_with_loc (Reloc.adjust_loc pos (p1, p2)) exc
    | exc -> Stdpp.raise_with_loc (pos, Reloc.shift_pos (String.length str) pos) exc ]
  in
  let cs = Stream.of_string new_str in copy_strip_locate cs
;

value rec ident len =
  parser
  [ [: `('A'..'Z' | 'a'..'z' | '0'..'9' | '_' | ''' as c); s :] ->
      ident (store len c) s
  | [: :] -> get_buff len ]
;

value rec copy cs =
  match cs with parser
  [ [: `'<' :] -> maybe_quot cs
  | [: `'"' :] -> do { print_char '"'; inside_string cs }
  | [: `c :] -> do { print_char c; copy cs }
  | [: :] -> () ]
and maybe_quot cs =
  match cs with parser
  [ [: `'<' :] ep -> inside_quot "" ep 0 cs
  | [: `':'; i = ident 0; `'<' ? "less char expected" :] ep ->
      inside_quot i ep 0 cs
  | [: :] -> do { print_char '<'; copy cs } ]
and inside_quot name pos len cs =
  match cs with parser
  [ [: `'>' :] -> maybe_end_quot name pos len cs
  | [: `c :] -> inside_quot name pos (store len c) cs
  | [: :] -> raise (Stream.Error "end of file in quotation") ]
and maybe_end_quot name pos len cs =
  match cs with parser
  [ [: `'>' :] -> do { quot name pos (get_buff len); copy cs }
  | [: :] -> inside_quot name pos (store len '>') cs ]
and inside_string cs =
  match cs with parser
  [ [: `'"' :] -> do { print_char '"'; copy cs }
  | [: `c :] -> do { print_char c; inside_string cs }
  | [: :] -> raise (Stream.Error "end of file in string") ]
;

value copy_quot cs = do { copy cs; flush stdout; };

value loc_fmt =
  match Sys.os_type with
  [ "MacOS" ->
      format_of_string "File \"%s\"; line %d; characters %d to %d\n### "
  | _ ->
      format_of_string "File \"%s\", line %d, characters %d-%d:\n" ]
;

value print_location loc file =
  let (fname, line, c1, c2) = Stdpp.line_of_loc file loc in
  do { Printf.eprintf loc_fmt file line c1 c2; flush stderr; }
;

value file = ref "";
Arg.parse [] (fun x -> file.val := x) "ocpp <objects> <file>";

value main () =
  try
    if file.val <> "" then
      copy_quot (Stream.of_channel (open_in_bin file.val))
    else ()
  with exc ->
    do {
      print_newline ();
      flush stdout;
      let exc =
        match exc with
        [ Stdpp.Exc_located loc exc -> do { print_location loc file.val; exc }
        | exc -> exc ]
      in
      raise exc
    }
;

Odyl_main.name.val := "ocpp";
Odyl_main.go.val := main;
