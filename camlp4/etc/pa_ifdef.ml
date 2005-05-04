(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id$ *)

(* This module is deprecated since version 3.07; use pa_macro.ml instead *)

value _ =
  prerr_endline "Warning: pa_ifdef is deprecated since OCaml 3.07. Use pa_macro instead."
;

type item_or_def 'a =
  [ SdStr of 'a | SdDef of string | SdUnd of string | SdNop ]
;

value list_remove x l =
  List.fold_right (fun e l -> if e = x then l else [e :: l]) l []
;

value defined = ref ["OCAML_308"; "OCAML_307"; "OCAML_305"; "CAMLP4_300"; "NEWSEQ"];
value define x = defined.val := [x :: defined.val];
value undef x = defined.val := list_remove x defined.val;

EXTEND
  GLOBAL: Pcaml.expr Pcaml.str_item Pcaml.sig_item;
  Pcaml.expr: LEVEL "top"
    [ [ "ifdef"; c = UIDENT; "then"; e1 = Pcaml.expr; "else";
         e2 = Pcaml.expr ->
          if List.mem c defined.val then e1 else e2
      | "ifndef"; c = UIDENT; "then"; e1 = Pcaml.expr; "else";
         e2 = Pcaml.expr ->
          if List.mem c defined.val then e2 else e1 ] ]
  ;
  Pcaml.str_item: FIRST
    [ [ x = def_undef_str ->
          match x with
          [ SdStr si -> si
          | SdDef x -> do { define x; <:str_item< declare end >> }
          | SdUnd x -> do { undef x; <:str_item< declare end >> }
          | SdNop -> <:str_item< declare end >> ] ] ]
  ;
  def_undef_str:
    [ [ "ifdef"; c = UIDENT; "then"; e1 = str_item_def_undef;
        "else"; e2 = str_item_def_undef ->
          if List.mem c defined.val then e1 else e2
      | "ifdef"; c = UIDENT; "then"; e1 = str_item_def_undef ->
          if List.mem c defined.val then e1 else SdNop
      | "ifndef"; c = UIDENT; "then"; e1 = str_item_def_undef;
        "else"; e2 = str_item_def_undef ->
          if List.mem c defined.val then e2 else e1
      | "ifndef"; c = UIDENT; "then"; e1 = str_item_def_undef ->
          if List.mem c defined.val then SdNop else e1
      | "define"; c = UIDENT -> SdDef c
      | "undef"; c = UIDENT -> SdUnd c ] ]
  ;
  str_item_def_undef:
    [ [ d = def_undef_str -> d
      | si = Pcaml.str_item -> SdStr si ] ]
  ;
  Pcaml.sig_item: FIRST
    [ [ x = def_undef_sig ->
          match x with
          [ SdStr si -> si
          | SdDef x -> do { define x; <:sig_item< declare end >> }
          | SdUnd x -> do { undef x; <:sig_item< declare end >> }
          | SdNop -> <:sig_item< declare end >> ] ] ]
  ;
  def_undef_sig:
    [ [ "ifdef"; c = UIDENT; "then"; e1 = sig_item_def_undef;
        "else"; e2 = sig_item_def_undef ->
          if List.mem c defined.val then e1 else e2
      | "ifdef"; c = UIDENT; "then"; e1 = sig_item_def_undef ->
          if List.mem c defined.val then e1 else SdNop
      | "ifndef"; c = UIDENT; "then"; e1 = sig_item_def_undef;
        "else"; e2 = sig_item_def_undef ->
          if List.mem c defined.val then e2 else e1
      | "ifndef"; c = UIDENT; "then"; e1 = sig_item_def_undef ->
          if List.mem c defined.val then SdNop else e1
      | "define"; c = UIDENT -> SdDef c
      | "undef"; c = UIDENT -> SdUnd c ] ]
  ;
  sig_item_def_undef:
    [ [ d = def_undef_sig -> d
      | si = Pcaml.sig_item -> SdStr si ] ]
  ;
END;

Pcaml.add_option "-D" (Arg.String define)
  "<string> Define for ifdef instruction."
;
Pcaml.add_option "-U" (Arg.String undef)
  "<string> Undefine for ifdef instruction."
;
