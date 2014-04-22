(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                              Leo White                              *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Documentation
open Format

let fmt_style_kind f x =
  match x with
  | SK_bold -> fprintf f "SK_bold"
  | SK_italic -> fprintf f "SK_italic"
  | SK_emphasize -> fprintf f "SK_emphasize"
  | SK_center -> fprintf f "SK_center"
  | SK_left -> fprintf f "SK_left"
  | SK_right -> fprintf f "SK_right"
  | SK_superscript -> fprintf f "SK_superscript"
  | SK_subscript -> fprintf f "SK_subscript"
  | SK_custom s -> fprintf f "SK_custom %s" s

let fmt_ref_kind f x =
  match x with
  | RK_element -> fprintf f "RK_element"
  | RK_module -> fprintf f "RK_module"
  | RK_module_type -> fprintf f "RK_module_type"
  | RK_class -> fprintf f "RK_class"
  | RK_class_type -> fprintf f "RK_class_type"
  | RK_value -> fprintf f "RK_value"
  | RK_type -> fprintf f "RK_type"
  | RK_exception -> fprintf f "RK_exception"
  | RK_attribute -> fprintf f "RK_attribute"
  | RK_method -> fprintf f "RK_method"
  | RK_section -> fprintf f "RK_section"
  | RK_recfield -> fprintf f "RK_recfield"
  | RK_const -> fprintf f "RK_const"
  | RK_link -> fprintf f "RK_link"
  | RK_custom s -> fprintf f "RK_custom %s" s

let fmt_see_ref f x =
  match x with
  | See_url s -> fprintf f "See_url %s" s
  | See_file s -> fprintf f "See_file %s" s
  | See_doc s -> fprintf f "See_doc %s" s

let line i f s =
  fprintf f "%s" (String.make ((2*i) mod 72) ' ');
  fprintf f s

let list i f ppf l =
  match l with
  | [] -> line i ppf "[]\n";
  | _ :: _ ->
     line i ppf "[\n";
     List.iter (f (i+1) ppf) l;
     line i ppf "]\n"

let option i f ppf x =
  match x with
  | None -> line i ppf "None\n";
  | Some x ->
      line i ppf "Some\n";
      f (i+1) ppf x

let string i ppf s = line i ppf "\"%s\"\n" s

let special_ref_kind i ppf x =
  line i ppf "special_ref_kind\n";
  let i = i+1 in
  match x with
  | SRK_module_list sl ->
      line i ppf "SRK_module_list\n";
      list i string ppf sl
  | SRK_index_list -> line i ppf "SRK_index_list\n"

let rec text_element i ppf x =
  line i ppf "text_element\n";
  let i = i+1 in
  match x with
  | Raw s ->
      line i ppf "Raw\n";
      string i ppf s
  | Code s ->
      line i ppf "Code\n";
      string i ppf s
  | PreCode s ->
      line i ppf "PreCode\n";
      string i ppf s
  | Verbatim s ->
      line i ppf "Verbatim\n";
      string i ppf s
  | Style(sk, txt) ->
      line i ppf "Style %a\n" fmt_style_kind sk;
      text i ppf txt
  | List txtl ->
      line i ppf "List\n";
      list i text ppf txtl
  | Enum txtl ->
      line i ppf "Enum\n";
      list i text ppf txtl
  | Newline ->
      line i ppf "Newline\n"
  | Block txt ->
      line i ppf "Block\n";
      text i ppf txt
  | Title(n, so, txt) ->
      line i ppf "Title %d\n" n;
      option i string ppf so;
      text i ppf txt
  | Ref(rk, s, txto) ->
      line i ppf "Ref %a\n" fmt_ref_kind rk;
      string i ppf s;
      option i text ppf txto
  | Special_ref srk ->
      line i ppf "Special\n";
      special_ref_kind i ppf srk
  | Target(so, s) ->
      line i ppf "Target\n";
      option i string ppf so;
      string i ppf s

and text i ppf x =
  line i ppf "text\n";
  list (i+1) text_element ppf x

let tag i ppf x =
  line i ppf "tag\n";
  let i = i+1 in
  match x with
    Author s ->
      line i ppf "Author\n";
      string i ppf s
  | Version s ->
      line i ppf "Version\n";
      string i ppf s
  | See(sr, txt) ->
      line i ppf "See %a\n" fmt_see_ref sr;
      text i ppf txt
  | Since s ->
      line i ppf "Since\n";
      string i ppf s
  | Before(s, txt) ->
      line i ppf "Before\n";
      string i ppf s;
      text i ppf txt
  | Deprecated txt ->
      line i ppf "Deprecated\n";
      text i ppf txt
  | Param(s, txt) ->
      line i ppf "Param\n";
      string i ppf s;
      text i ppf txt
  | Raised_exception(s, txt) ->
      line i ppf "Raised_exception\n";
      string i ppf s;
      text i ppf txt
  | Return_value txt ->
      line i ppf "Return_value\n";
      text i ppf txt
  | Custom(s, txt) ->
      line i ppf "Custom %s\n" s;
      text i ppf txt

let documentation i ppf x =
  match x with
  | Cinfo(txt, tags) ->
      line i ppf "Cinfo\n";
      text (i+1) ppf txt;
      list (i+1) tag ppf tags
  | Cstop ->
      line i ppf "Cstop\n"
