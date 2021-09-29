(* ------------ Ocaml Web-manual -------------- *)

(* Copyright San Vu Ngoc, 2020

   file: common.ml

   This file contains functions that are used by process_api.ml and
   process_manual.ml *)

open Soup
open Printf

let debug = not (Array.mem "quiet" Sys.argv)

let dbg =
  let printf = Printf.(if debug then kfprintf else ikfprintf) in
  let flush =
    if debug then
      fun ch -> output_char ch '\n'; flush ch
    else
      ignore
  in
  fun fmt -> printf flush stdout fmt

let ( // ) = Filename.concat

let process_dir = Filename.current_dir_name

(* Output directory *)
let web_dir = Filename.parent_dir_name // "webman"

(* Output for manual *)
let docs_maindir = web_dir // "manual"
let docs_file = ( // ) docs_maindir

(* Ouput for API *)
let api_dir = web_dir // "api"

(* How to go from manual to api *)
let api_page_url = "../api"

(* How to go from api to manual *)
 let manual_page_url = "../manual"

(* Set this to the directory where to find the html sources of all versions: *)
let html_maindir = "../htmlman"

(* Where to get the original html files *)
let html_file = ( // ) html_maindir

let releases_url = "https://ocaml.org/releases/"

let favicon = "favicon.ico"

(**** utilities ****)

let flat_option f o = Option.bind o f

let (<<) f g x = f (g x)

let string_of_opt = Option.value ~default:""

let starts_with substring s =
  let l = String.length substring in
  l <= String.length s &&
  String.sub s 0 l = substring

(**** html processing ****)

(* Return next html element. *)
let rec next node =
  match next_element node with
  | Some n -> n
  | None -> match parent node with
    | Some p -> next p
    | None -> raise Not_found

let logo_html url =
  "<nav class=\"toc brand\"><a class=\"brand\" href=\"" ^ url ^
  "\" ><img src=\"colour-logo.svg\" class=\"svg\" alt=\"OCaml\" /></a></nav>"
  |> parse

let wrap_body ~classes soup =
  let body = soup $ "body" in
  set_name "div" body;
  List.iter (fun c -> add_class c body) classes;
  wrap body (create_element "body");
  body

(* Add favicon *)
let add_favicon head =
  parse ({|<link rel="shortcut icon" type="image/x-icon" href="|} ^
         favicon ^ {|">|})
  |> append_child head

(* Update html <head> element with javascript and favicon *)
let update_head ?(search = false) soup =
  let head = soup $ "head" in
  if search then begin
    create_element "script" ~attributes:["src","search.js"]
    |> append_child head
  end;
  create_element "script" ~attributes:["src","scroll.js"]
  |> append_child head;
  create_element "script" ~attributes:["src","navigation.js"]
  |> append_child head;
  add_favicon head

(* Add version number *)
let add_version_link nav text url =
  let vnum = create_element "div" ~class_:"toc_version" in
  let a = create_element "a" ~inner_text:text
      ~attributes:["href", url; "id", "version-select"] in
  append_child vnum a;
  prepend_child nav vnum

let add_sidebar_button body =
  let btn = create_element "div" ~id:"sidebar-button" in
  create_element "span" ~inner_text:"☰"
  |> prepend_child btn;
  prepend_child body btn

(* Detect OCaml version from VERSION file *)
let find_version () =
  let pp = Filename.parent_dir_name in
  let version_file = pp // pp // pp // "VERSION" in
  let major, minor = Scanf.bscanf (Scanf.Scanning.from_file version_file) "%u.%u" (fun x y -> x,y) in
  sprintf "%u.%u" major minor

(*
   Local Variables:
   compile-command:"dune build"
   End:
*)
