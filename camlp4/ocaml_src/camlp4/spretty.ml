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

(* Id *)

type glue = LO | RO | LR | NO;;
type pretty =
    S of glue * string
  | Hbox of pretty Stream.t
  | HVbox of pretty Stream.t
  | HOVbox of pretty Stream.t
  | HOVCbox of pretty Stream.t
  | Vbox of pretty Stream.t
  | BEbox of pretty Stream.t
  | BEVbox of pretty Stream.t
  | LocInfo of (int * int) * pretty
;;
type prettyL =
    SL of int * glue * string
  | HL of prettyL list
  | BL of prettyL list
  | PL of prettyL list
  | QL of prettyL list
  | VL of prettyL list
  | BE of prettyL list
  | BV of prettyL list
  | LI of (int * int) * prettyL
;;
type warnloc = int * int -> unit;;

let quiet = ref true;;
let maxl = ref 20;;
let dt = ref 2;;
let tol = ref 1;;
let sp = ref ' ';;
let locf = ref (fun _ -> ());;
let prompt = ref "";;
let print_char_fun = ref (output_char stdout);;
let print_string_fun = ref (output_string stdout);;
let print_newline_fun = ref (fun () -> output_char stdout '\n');;

let print_char c = !print_char_fun c;;
let print_string s = !print_string_fun s;;
let print_newline () = !print_newline_fun ();;

let rec print_spaces =
  function
    0 -> ()
  | n -> print_char !sp; print_spaces (n - 1)
;;

let string_np pos np = pos + np;;

let trace_ov pos =
  if not !quiet && pos > !maxl then
    begin
      prerr_string "<W> prettych: overflow (length = ";
      prerr_int pos;
      prerr_endline ")"
    end
;;

let tolerate tab pos spc = pos + spc <= tab + !dt + !tol;;

let h_print_string pos spc np x =
  let npos = string_np (pos + spc) np in
  print_spaces spc; print_string x; npos
;;

let n_print_string pos spc np x =
  print_spaces spc; print_string x; string_np (pos + spc) np
;;

let rec hnps (pos, spc as ps) =
  function
    SL (np, RO, _) -> string_np pos np, 1
  | SL (np, LO, _) -> string_np (pos + spc) np, 0
  | SL (np, NO, _) -> string_np pos np, 0
  | SL (np, LR, _) -> string_np (pos + spc) np, 1
  | HL x -> hnps_list ps x
  | BL x -> hnps_list ps x
  | PL x -> hnps_list ps x
  | QL x -> hnps_list ps x
  | VL [x] -> hnps ps x
  | VL [] -> ps
  | VL x -> !maxl + 1, 0
  | BE x -> hnps_list ps x
  | BV x -> !maxl + 1, 0
  | LI (_, x) -> hnps ps x
and hnps_list (pos, _ as ps) pl =
  if pos > !maxl then !maxl + 1, 0
  else
    match pl with
      p :: pl -> hnps_list (hnps ps p) pl
    | [] -> ps
;;

let rec first =
  function
    SL (_, _, s) -> Some s
  | HL x -> first_in_list x
  | BL x -> first_in_list x
  | PL x -> first_in_list x
  | QL x -> first_in_list x
  | VL x -> first_in_list x
  | BE x -> first_in_list x
  | BV x -> first_in_list x
  | LI (_, x) -> first x
and first_in_list =
  function
    p :: pl ->
      begin match first p with
        Some p -> Some p
      | None -> first_in_list pl
      end
  | [] -> None
;;

let first_is_too_big tab p =
  match first p with
    Some s -> tab + String.length s >= !maxl
  | None -> false
;;

let too_long tab x p =
  if first_is_too_big tab p then false
  else let (pos, spc) = hnps x p in pos > !maxl
;;

let rec hprint_pretty pos spc =
  function
    SL (np, RO, x) -> h_print_string pos 0 np x, 1
  | SL (np, LO, x) -> h_print_string pos spc np x, 0
  | SL (np, NO, x) -> h_print_string pos 0 np x, 0
  | SL (np, LR, x) -> h_print_string pos spc np x, 1
  | HL x -> hprint_box pos spc x
  | BL x -> hprint_box pos spc x
  | PL x -> hprint_box pos spc x
  | QL x -> hprint_box pos spc x
  | VL [x] -> hprint_pretty pos spc x
  | VL [] -> pos, spc
  | VL x -> hprint_box pos spc x
  | BE x -> hprint_box pos spc x
  | BV x -> invalid_arg "hprint_pretty"
  | LI (loc, x) -> !locf loc; hprint_pretty pos spc x
and hprint_box pos spc =
  function
    p :: pl ->
      let (pos, spc) = hprint_pretty pos spc p in hprint_box pos spc pl
  | [] -> pos, spc
;;

let rec print_pretty tab pos spc =
  function
    SL (np, RO, x) -> n_print_string pos 0 np x, 1
  | SL (np, LO, x) -> n_print_string pos spc np x, 0
  | SL (np, NO, x) -> n_print_string pos 0 np x, 0
  | SL (np, LR, x) -> n_print_string pos spc np x, 1
  | HL x as p -> print_horiz tab pos spc x
  | BL x as p -> print_horiz_vertic tab pos spc (too_long tab (pos, spc) p) x
  | PL x as p -> print_paragraph tab pos spc (too_long tab (pos, spc) p) x
  | QL x as p -> print_sparagraph tab pos spc (too_long tab (pos, spc) p) x
  | VL x -> print_vertic tab pos spc x
  | BE x as p -> print_begin_end tab pos spc (too_long tab (pos, spc) p) x
  | BV x -> print_beg_end tab pos spc x
  | LI (loc, x) -> !locf loc; print_pretty tab pos spc x
and print_horiz tab pos spc =
  function
    p :: pl ->
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
           [] -> true
         | _ -> false then
        npos, nspc
      else print_horiz tab npos nspc pl
  | [] -> pos, spc
and print_horiz_vertic tab pos spc ov pl =
  if ov then print_vertic tab pos spc pl else hprint_box pos spc pl
and print_vertic tab pos spc =
  function
    p :: pl ->
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
           [] -> true
         | _ -> false then
        npos, nspc
      else if tolerate tab npos nspc then
        begin print_spaces nspc; print_vertic_rest (npos + nspc) pl end
      else
        begin
          print_newline ();
          print_string !prompt;
          print_spaces (tab + !dt);
          print_vertic_rest (tab + !dt) pl
        end
  | [] -> pos, spc
and print_vertic_rest tab =
  function
    p :: pl ->
      let (pos, spc) = print_pretty tab tab 0 p in
      if match pl with
           [] -> true
         | _ -> false then
        pos, spc
      else
        begin
          print_newline ();
          print_string !prompt;
          print_spaces tab;
          print_vertic_rest tab pl
        end
  | [] -> tab, 0
and print_paragraph tab pos spc ov pl =
  if ov then print_parag tab pos spc pl else hprint_box pos spc pl
and print_parag tab pos spc =
  function
    p :: pl ->
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
           [] -> true
         | _ -> false then
        npos, nspc
      else if npos == tab then print_parag_rest tab tab 0 pl
      else if too_long tab (pos, spc) p then
        begin
          print_newline ();
          print_string !prompt;
          print_spaces (tab + !dt);
          print_parag_rest (tab + !dt) (tab + !dt) 0 pl
        end
      else if tolerate tab npos nspc then
        begin
          print_spaces nspc; print_parag_rest (npos + nspc) (npos + nspc) 0 pl
        end
      else print_parag_rest (tab + !dt) npos nspc pl
  | [] -> pos, spc
and print_parag_rest tab pos spc =
  function
    p :: pl ->
      let (pos, spc) =
        if pos > tab && too_long tab (pos, spc) p then
          begin
            print_newline (); print_string !prompt; print_spaces tab; tab, 0
          end
        else pos, spc
      in
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
           [] -> true
         | _ -> false then
        npos, nspc
      else
        let (pos, spc) =
          if npos > tab && too_long tab (pos, spc) p then
            begin
              print_newline (); print_string !prompt; print_spaces tab; tab, 0
            end
          else npos, nspc
        in
        print_parag_rest tab pos spc pl
  | [] -> pos, spc
and print_sparagraph tab pos spc ov pl =
  if ov then print_sparag tab pos spc pl else hprint_box pos spc pl
and print_sparag tab pos spc =
  function
    p :: pl ->
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
           [] -> true
         | _ -> false then
        npos, nspc
      else if tolerate tab npos nspc then
        begin
          print_spaces nspc;
          print_sparag_rest (npos + nspc) (npos + nspc) 0 pl
        end
      else print_sparag_rest (tab + !dt) npos nspc pl
  | [] -> pos, spc
and print_sparag_rest tab pos spc =
  function
    p :: pl ->
      let (pos, spc) =
        if pos > tab && too_long tab (pos, spc) p then
          begin
            print_newline (); print_string !prompt; print_spaces tab; tab, 0
          end
        else pos, spc
      in
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
           [] -> true
         | _ -> false then
        npos, nspc
      else print_sparag_rest tab npos nspc pl
  | [] -> pos, spc
and print_begin_end tab pos spc ov pl =
  if ov then print_beg_end tab pos spc pl else hprint_box pos spc pl
and print_beg_end tab pos spc =
  function
    p :: pl ->
      let (npos, nspc) = print_pretty tab pos spc p in
      if match pl with
           [] -> true
         | _ -> false then
        npos, nspc
      else if tolerate tab npos nspc then
        let nspc = if npos == tab then nspc + !dt else nspc in
        print_spaces nspc; print_beg_end_rest tab (npos + nspc) pl
      else
        begin
          print_newline ();
          print_string !prompt;
          print_spaces (tab + !dt);
          print_beg_end_rest tab (tab + !dt) pl
        end
  | [] -> pos, spc
and print_beg_end_rest tab pos =
  function
    p :: pl ->
      let (pos, spc) = print_pretty (tab + !dt) pos 0 p in
      if match pl with
           [] -> true
         | _ -> false then
        pos, spc
      else
        begin
          print_newline ();
          print_string !prompt;
          print_spaces tab;
          print_beg_end_rest tab tab pl
        end
  | [] -> pos, 0
;;

let string_npos s = String.length s;;

let rec conv =
  function
    S (g, s) -> SL (string_npos s, g, s)
  | Hbox x -> HL (conv_stream x)
  | HVbox x -> BL (conv_stream x)
  | HOVbox x ->
      begin match conv_stream x with
        [PL _ as x] -> x
      | x -> PL x
      end
  | HOVCbox x -> QL (conv_stream x)
  | Vbox x -> VL (conv_stream x)
  | BEbox x -> BE (conv_stream x)
  | BEVbox x -> BV (conv_stream x)
  | LocInfo (loc, x) -> LI (loc, conv x)
and conv_stream (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some p -> Stream.junk strm__; conv p :: conv_stream strm__
  | _ -> []
;;

let print_pretty pr_ch pr_str pr_nl pr pr2 m lf p =
  maxl := m;
  print_char_fun := pr_ch;
  print_string_fun := pr_str;
  print_newline_fun := pr_nl;
  prompt := pr2;
  locf := lf;
  print_string pr;
  let _ = print_pretty 0 0 0 (conv p) in ()
;;
