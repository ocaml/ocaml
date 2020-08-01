open Soup
open Printf

let debug = not (Array.mem "quiet" Sys.argv)

let pr = if debug then print_endline else fun _ -> ()

let ( // ) = Filename.concat

let process_dir = Filename.current_dir_name

(* Output directory *)
let web_dir = "../webman"

(* Output for manual *)
let docs_maindir = with_dir web_dir "manual"
let docs_file = with_dir docs_maindir

(* Ouput for API *)
let api_dir = with_dir web_dir "api"

(* How to go from manual to api *)
let api_page_url = "../api"

(* How to go from api to manual *)
 let manual_page_url = "../manual"

(* Set this to the directory where to find the html sources of all versions: *)
let html_maindir = "../htmlman"

(* Where to get the original html files *)
let html_file = with_dir html_maindir

let releases_url = "https://ocaml.org/releases/"

(**** utilities ****)

let do_option f = function
  | None -> ()
  | Some x -> f x

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

let flat_option f = function
  | None -> None
  | Some x -> f x

let (<<) f g x = f (g x)

let string_of_opt = function
  | None -> ""
  | Some s -> s

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
  "\" ><img src=\"colour-logo-gray.svg\" class=\"svg\" alt=\"OCaml\" /></a></nav>"
  |> parse

let wrap_body ~classes soup =
  let body = soup $ "body" in
  set_name "div" body;
  List.iter (fun c -> add_class c body) classes;
  wrap body (create_element "body");
  body

(* Add version number *)
let add_version_link nav text url =
  let vnum = create_element "div" ~class_:"toc_version" in
  let a = create_element "a" ~inner_text:text
      ~attributes:["href", url; "id", "version-select"] in
  append_child vnum a;
  prepend_child nav vnum

(* Some wrappers around linux system commands*)
let sys_cp file dst =
  if Sys.command (sprintf "cp %s %s" file dst) <> 0
  then failwith ("Could not copy " ^ file)

let sys_mkdir dir =
  if Sys.command (sprintf "mkdir -p %s" dir) <> 0
  then failwith ("Could not create directory" ^ dir)

let sys_mv file dst =
  if Sys.command (sprintf "mv %s %s" file dst) <> 0
  then failwith ("Could not move " ^ file)

(* Compile scss with sass *)
let compile_css src dst =
  let src = with_dir process_dir src in
  if Sys.command (sprintf "sass %s > %s" src dst) <> 0
  then sprintf "Could not compile %s to %s. Is sass installed?" src dst
       |> failwith

(* Detect OCaml version from version.tex *)
let find_version () =
  let versiontex = "../version.tex" in
  Scanf.bscanf (Scanf.Scanning.from_file versiontex)
    "\\def\\ocamlversion{%s@}" (fun a -> a)

(*
   Local Variables:
   compile-command:"dune build"
   End:
*)
