(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type glue = [ LO | RO | LR | NO ];
type pretty =
  [ S of glue and string
  | Hbox of Stream.t pretty
  | HVbox of Stream.t pretty
  | HOVbox of Stream.t pretty
  | HOVCbox of Stream.t pretty
  | Vbox of Stream.t pretty
  | BEbox of Stream.t pretty
  | BEVbox of Stream.t pretty
  | LocInfo of (int * int) and pretty ]
;
type prettyL =
  [ SL of int and glue and string
  | HL of list prettyL
  | BL of list prettyL
  | PL of list prettyL
  | QL of list prettyL
  | VL of list prettyL
  | BE of list prettyL
  | BV of list prettyL
  | LI of (int * int) and prettyL ]
;
type warnloc = (int * int) -> unit;

value quiet = ref True;
value maxl = ref 20;
value dt = ref 2;
value tol = ref 1;
value sp = ref ' ';
value locf = ref (fun _ -> ());
value prompt = ref "";
value print_char_fun = ref (output_char stdout);
value print_string_fun = ref (output_string stdout);
value print_newline_fun = ref (fun () -> output_char stdout '\n');

value print_char c = print_char_fun.val c;
value print_string s = print_string_fun.val s;
value print_newline () = print_newline_fun.val ();

value rec print_spaces =
  fun
  [ 0 -> ()
  | n -> do { print_char sp.val; print_spaces (n - 1) } ]
;

value string_np pos np = pos + np;

value trace_ov pos =
  if not quiet.val && pos > maxl.val then do {
    prerr_string "<W> prettych: overflow (length = ";
    prerr_int pos;
    prerr_endline ")"
  }
  else ()
;

value tolerate tab pos spc = pos + spc <= tab + dt.val + tol.val;

value h_print_string pos spc np x =
  let npos = string_np (pos + spc) np in
  do { print_spaces spc; print_string x; npos }
;

value n_print_string pos spc np x =
  do { print_spaces spc; print_string x; string_np (pos + spc) np }
;

value rec hnps ((pos, spc) as ps) =
  fun
  [ SL np RO _ -> (string_np pos np, 1)
  | SL np LO _ -> (string_np (pos + spc) np, 0)
  | SL np NO _ -> (string_np pos np, 0)
  | SL np LR _ -> (string_np (pos + spc) np, 1)
  | HL x -> hnps_list ps x
  | BL x -> hnps_list ps x
  | PL x -> hnps_list ps x
  | QL x -> hnps_list ps x
  | VL [x] -> hnps ps x
  | VL [] -> ps
  | VL x -> (maxl.val + 1, 0)
  | BE x -> hnps_list ps x
  | BV x -> (maxl.val + 1, 0)
  | LI _ x -> hnps ps x ]
and hnps_list ((pos, _) as ps) pl =
  if pos > maxl.val then (maxl.val + 1, 0)
  else
    match pl with
    [ [p :: pl] -> hnps_list (hnps ps p) pl
    | [] -> ps ]
;

value rec first =
  fun
  [ SL _ _ s -> Some s
  | HL x -> first_in_list x
  | BL x -> first_in_list x
  | PL x -> first_in_list x
  | QL x -> first_in_list x
  | VL x -> first_in_list x
  | BE x -> first_in_list x
  | BV x -> first_in_list x
  | LI _ x -> first x ]
and first_in_list =
  fun
  [ [p :: pl] ->
      match first p with
      [ Some p -> Some p
      | None -> first_in_list pl ]
  | [] -> None ]
;

value first_is_too_big tab p =
  match first p with
  [ Some s -> tab + String.length s >= maxl.val
  | None -> False ]
;

value too_long tab x p =
  if first_is_too_big tab p then False
  else
    let (pos, spc) = hnps x p in
    pos > maxl.val
;

value rec hprint_pretty pos spc =
  fun
  [ SL np RO x -> (h_print_string pos 0 np x, 1)
  | SL np LO x -> (h_print_string pos spc np x, 0)
  | SL np NO x -> (h_print_string pos 0 np x, 0)
  | SL np LR x -> (h_print_string pos spc np x, 1)
  | HL x -> hprint_box pos spc x
  | BL x -> hprint_box pos spc x
  | PL x -> hprint_box pos spc x
  | QL x -> hprint_box pos spc x
  | VL [x] -> hprint_pretty pos spc x
  | VL [] -> (pos, spc)
  | VL x -> hprint_box pos spc x
  | BE x -> hprint_box pos spc x
  | BV x -> invalid_arg "hprint_pretty"
  | LI loc x -> do { locf.val loc; hprint_pretty pos spc x } ]
and hprint_box pos spc =
  fun
  [ [p :: pl] ->
      let (pos, spc) = hprint_pretty pos spc p in
      hprint_box pos spc pl
  | [] -> (pos, spc) ]
;

value rec print_pretty tab pos spc =
  fun
  [ SL np RO x -> (n_print_string pos 0 np x, 1)
  | SL np LO x -> (n_print_string pos spc np x, 0)
  | SL np NO x -> (n_print_string pos 0 np x, 0)
  | SL np LR x -> (n_print_string pos spc np x, 1)
  | HL x as p -> print_horiz tab pos spc x
  | BL x as p -> print_horiz_vertic tab pos spc (too_long tab (pos, spc) p) x
  | PL x as p -> print_paragraph tab pos spc (too_long tab (pos, spc) p) x
  | QL x as p -> print_sparagraph tab pos spc (too_long tab (pos, spc) p) x
  | VL x -> print_vertic tab pos spc x
  | BE x as p -> print_begin_end tab pos spc (too_long tab (pos, spc) p) x
  | BV x -> print_beg_end tab pos spc x
  | LI loc x -> do { locf.val loc; print_pretty tab pos spc x } ]
and print_horiz tab pos spc =
  fun
  [ [p :: pl] ->
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
         [ [] -> True
         | _ -> False ]
      then
        (npos, nspc)
      else print_horiz tab npos nspc pl
  | [] -> (pos, spc) ]
and print_horiz_vertic tab pos spc ov pl =
  if ov then print_vertic tab pos spc pl else hprint_box pos spc pl
and print_vertic tab pos spc =
  fun
  [ [p :: pl] ->
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
         [ [] -> True
         | _ -> False ]
      then
        (npos, nspc)
      else if tolerate tab npos nspc then do {
        print_spaces nspc; print_vertic_rest (npos + nspc) pl
      }
      else do {
        print_newline ();
        print_string prompt.val;
        print_spaces (tab + dt.val);
        print_vertic_rest (tab + dt.val) pl
      }
  | [] -> (pos, spc) ]
and print_vertic_rest tab =
  fun
  [ [p :: pl] ->
      let (pos, spc) = print_pretty tab tab 0 p in
      if match pl with
         [ [] -> True
         | _ -> False ]
      then
        (pos, spc)
      else do {
        print_newline ();
        print_string prompt.val;
        print_spaces tab;
        print_vertic_rest tab pl
      }
  | [] -> (tab, 0) ]
and print_paragraph tab pos spc ov pl =
  if ov then print_parag tab pos spc pl else hprint_box pos spc pl
and print_parag tab pos spc =
  fun
  [ [p :: pl] ->
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
         [ [] -> True
         | _ -> False ]
      then
        (npos, nspc)
      else if npos == tab then print_parag_rest tab tab 0 pl
      else if too_long tab (pos, spc) p then do {
        print_newline ();
        print_string prompt.val;
        print_spaces (tab + dt.val);
        print_parag_rest (tab + dt.val) (tab + dt.val) 0 pl
      }
      else if tolerate tab npos nspc then do {
        print_spaces nspc; print_parag_rest (npos + nspc) (npos + nspc) 0 pl
      }
      else print_parag_rest (tab + dt.val) npos nspc pl
  | [] -> (pos, spc) ]
and print_parag_rest tab pos spc =
  fun
  [ [p :: pl] ->
      let (pos, spc) =
        if pos > tab && too_long tab (pos, spc) p then do {
          print_newline (); print_string prompt.val; print_spaces tab; (tab, 0)
        }
        else (pos, spc)
      in
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
         [ [] -> True
         | _ -> False ]
      then
        (npos, nspc)
      else
        let (pos, spc) =
          if npos > tab && too_long tab (pos, spc) p then do {
            print_newline ();
            print_string prompt.val;
            print_spaces tab; (tab, 0)
          }
          else (npos, nspc)
        in
        print_parag_rest tab pos spc pl
  | [] -> (pos, spc) ]
and print_sparagraph tab pos spc ov pl =
  if ov then print_sparag tab pos spc pl else hprint_box pos spc pl
and print_sparag tab pos spc =
  fun
  [ [p :: pl] ->
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
         [ [] -> True
         | _ -> False ]
      then
        (npos, nspc)
      else if tolerate tab npos nspc then do {
        print_spaces nspc; print_sparag_rest (npos + nspc) (npos + nspc) 0 pl
      }
      else print_sparag_rest (tab + dt.val) npos nspc pl
  | [] -> (pos, spc) ]
and print_sparag_rest tab pos spc =
  fun
  [ [p :: pl] ->
      let (pos, spc) =
        if pos > tab && too_long tab (pos, spc) p then do {
          print_newline (); print_string prompt.val; print_spaces tab; (tab, 0)
        }
        else (pos, spc)
      in
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
         [ [] -> True
         | _ -> False ]
      then
        (npos, nspc)
      else print_sparag_rest tab npos nspc pl
  | [] -> (pos, spc) ]
and print_begin_end tab pos spc ov pl =
  if ov then print_beg_end tab pos spc pl else hprint_box pos spc pl
and print_beg_end tab pos spc =
  fun
  [ [p :: pl] ->
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
         [ [] -> True
         | _ -> False ]
      then
        (npos, nspc)
      else if tolerate tab npos nspc then do {
        let nspc = if npos == tab then nspc + dt.val else nspc in
        print_spaces nspc;
        print_beg_end_rest tab (npos + nspc) pl
      }
      else do {
        print_newline ();
        print_string prompt.val;
        print_spaces (tab + dt.val);
        print_beg_end_rest tab (tab + dt.val) pl
      }
  | [] -> (pos, spc) ]
and print_beg_end_rest tab pos =
  fun
  [ [p :: pl] ->
      let (pos, spc) = print_pretty (tab + dt.val) pos 0 p in
      if match pl with
         [ [] -> True
         | _ -> False ]
      then
        (pos, spc)
      else do {
        print_newline ();
        print_string prompt.val;
        print_spaces tab;
        print_beg_end_rest tab tab pl
      }
  | [] -> (pos, 0) ]
;

value string_npos s = String.length s;

value rec conv =
  fun
  [ S g s -> SL (string_npos s) g s
  | Hbox x -> HL (conv_stream x)
  | HVbox x -> BL (conv_stream x)
  | HOVbox x ->
      match conv_stream x with
      [ [(PL _ as x)] -> x
      | x -> PL x ]
  | HOVCbox x -> QL (conv_stream x)
  | Vbox x -> VL (conv_stream x)
  | BEbox x -> BE (conv_stream x)
  | BEVbox x -> BV (conv_stream x)
  | LocInfo loc x -> LI loc (conv x) ]
and conv_stream =
  parser [ [: `p; s :] -> [conv p :: conv_stream s] | [: :] -> [] ]
;

value print_pretty pr_ch pr_str pr_nl pr pr2 m lf p =
  do {
    maxl.val := m;
    print_char_fun.val := pr_ch;
    print_string_fun.val := pr_str;
    print_newline_fun.val := pr_nl;
    prompt.val := pr2;
    locf.val := lf;
    print_string pr;
    let _ = print_pretty 0 0 0 (conv p) in
    ()
  }
;
