(* Post-processing the HTML of the OCaml Manual.

   San Vu Ngoc, 2020

   * Processed parts: ["tutorials"; "refman"; "commands"; "library" ]

   * The "API" side is treated by another script.

   requires Lambdasoup

   cd ..; make web

 *)

open Soup
open Printf
open Common

(* Alternative formats for the manual: *)
let archives =
  ["refman-html.tar.gz"; "refman.txt"; "refman.pdf"; "refman.info.tar.gz"]

(* Remove number: "Chapter 1  The core language" ==> "The core language" *)
let remove_number s =
  Str.global_replace (Str.regexp ".+  ") "" s

(* Scan index.html and return the list of chapters (title, file) *)
let index part =
  sprintf "Reading part [%s]" part |> pr;
  let html = read_file (html_file "index.html") in
  let soup = parse html in
  (* Foreword. We do nothing. *)
  (* We select the list of (html files, titles) for Part [part] *)
  let part =
    let a = match select_one ("a[id=\"p:" ^ part ^ "\"]") soup with
      | Some node -> node
      | None -> R.select_one ("a[name=\"p:" ^ part ^ "\"]") soup in
    let ul = next a in
    assert (name ul = "ul");
    ul $$ "a"
    |> fold (fun list a ->
        (R.leaf_text a |> remove_number, R.attribute "href" a) :: list) []
    |> List.rev in
  part


(* This string is updated by [extract_date] *)
let copyright_text = ref "Copyright © 2020 Institut National de Recherche en Informatique et en Automatique"

let copyright () =
  "<div class=\"copyright\">" ^ !copyright_text ^ "</div>"
  |> parse

let load_html file =
  pr file;
  (* First we perform some direct find/replace in the html string. *)
  let html =
    read_file (html_file file)
    (* Normalize non-break spaces: *)
    |> Str.global_replace (Str.regexp_string "&#XA0;") " "
    |> Str.global_replace (Str.regexp "Chapter \\([0-9]+\\)")
      (if file = "index.html" then "<span>\\1.</span>"
      else "<span>Chapter \\1</span>")

    (* I think it would be good to replace "chapter" by "tutorial" for part
       I. The problem of course is how we number chapters in the other parts. *)

    (* |> Str.global_replace (Str.regexp_string "chapter") "tutorial"
     * |> Str.global_replace (Str.regexp_string "Chapter") "Tutorial" *)

    (* Remove the chapter number in local links, it makes the TOC unnecessarily
       unfriendly. *)
    |> Str.global_replace (Str.regexp ">[0-9]+\\.\\([0-9]+\\) ") ">\\1 "
    |> Str.global_replace (Str.regexp "[0-9]+\\.\\([0-9]+\\.[0-9]+\\) ")
      "\\1 "

    (* The API (libref and compilerlibref directories) should be separate
       entities, to better distinguish them from the manual. *)
    |> Str.global_replace (Str.regexp_string "\"libref/")
      (sprintf "\"%s/" api_page_url)
    |> Str.global_replace (Str.regexp_string "\"compilerlibref/")
      (sprintf "\"%s/compilerlibref/" api_page_url)
  in

  (* For the main index file, we do a few adjustments *)
  let html = if file = "index.html"
    then Str.global_replace (Str.regexp "Part \\([I|V]+\\)<br>")
        "<span>\\1. </span>" html
    else html in

  (* Set utf8 encoding directly in the html string *)
  let charset_regexp = Str.regexp "charset=\\([-A-Za-z0-9]+\\)\\(\\b\\|;\\)" in
  match Str.search_forward charset_regexp html 0 with
  | exception Not_found -> pr "Warning, no charset found in html."; html
  | _ -> match (String.lowercase_ascii (Str.matched_group 1 html)) with
    | "utf-8" -> pr "Charset is UTF-8; good."; html
    | "us-ascii" -> pr "Charset is US-ASCII. We change it to UTF-8";
      Str.global_replace charset_regexp "charset=UTF-8\\2" html
    | _ -> pr "Warning, charset not recognized."; html

(* Save new html file *)
let save_to_file soup file =
  let new_html = to_string soup in
  write_file (docs_file file) new_html

(* Remove first three links "Previous, Up, Next" *)
let remove_navigation soup =
  do_option delete (soup $? "hr");
  ["Previous"; "Up"; "Next"]
  |> List.iter (fun s ->
      soup $$ ("img[alt=\"" ^ s ^ "\"]")
      |> iter (do_option delete << parent))

(* Create a new file by cloning the structure of "soup", and inserting the
   content of external file (hence preserving TOC and headers) *)
let clone_structure soup xfile =
  let xternal = parse (load_html xfile) in
  remove_navigation xternal;
  do_option delete (xternal $? "hr");
  let xbody = xternal $ "body" in
  let clone = parse (to_string soup) in
  let header = clone $ "header" in
  insert_after header xbody;
  create_element ~id:"start-section" "a"
  |> insert_after header;
  next_siblings xbody
  |> iter delete;
  insert_after xbody (copyright ());
  set_name "section" xbody;
  set_attribute "id" "section" xbody;
  save_to_file clone xfile

(* Extract the date (and copyright) from the maintitle block in "index.html" *)
let extract_date maintitle =
  let months = ["January"; "February"; "March"; "April";
                "May"; "June"; "July"; "August"; "September";
                "October"; "November"; "December"] in
  let txts = texts maintitle
             |> List.map String.trim in
  copyright_text := List.hd (List.rev txts);
  txts
  |> List.filter (fun s -> List.exists (fun month -> starts_with month s) months)
  |> function | [s] -> Some s
              | _ -> pr "Warning, date not found"; None

(* Special treatment of the main index.html file *)
let convert_index version soup =
  (* Remove "translated from LaTeX" *)
  soup $$ "blockquote" |> last |> do_option delete;
  let title_selector = if float_of_string version < 4.07
    then "div.center" else "div.maintitle" in
  let maintitle = soup $ title_selector in
  sprintf "<div class=\"maintitle\"><h1><span>The OCaml system</span>  release %s </h1><h3>%s</h3></div>"
    version (extract_date maintitle |> string_of_opt)
  |> parse
  |> insert_after maintitle ;
  delete maintitle;
  let body = soup $ ".index" in
  {|<span class="authors">Xavier Leroy,<br> Damien Doligez, Alain Frisch, Jacques Garrigue, Didier Rémy and Jérôme Vouillon</span>|}
  |> parse
  |> append_child body

(* This is the main script for processing a specified file. [convert] has to be
   run for each "entry" [file] of the manual, making a "Chapter".  (the list of
   [chapters] corresponds to a "Part" of the manual) *)
let convert version chapters (title, file) =
  pr ((html_file file) ^ " ==> " ^ (docs_file file));

  (* Parse html *)
  let soup = parse (load_html file) in

  (* Change title *)
  let title_tag = soup $ "title" in
  let new_title = create_element "title" ~inner_text:("OCaml - " ^ title) in
  replace title_tag new_title;

  (* Wrap body. TODO use set_name instead *)
  let c = if file = "index.html" then ["manual"; "content"; "index"]
    else ["manual"; "content"] in
  let body = wrap_body ~classes:c soup in

   remove_navigation soup;

  if file = "index.html" then convert_index version soup;

  (* Create left sidebar for TOC.  *)
  let toc = match soup $? "ul" with
    | None -> None (* can be None, eg chapters 15,19...*)
    | Some t -> if classes t <> [] (* as in libthreads.html or parsing.html *)
      then (sprintf "We don't promote <UL> to TOC for file %s" file |> pr; None)
      else Some t in
  let nav = create_element "nav" ~class_:"toc" in
  let () = match toc with
    | None -> prepend_child body nav
    | Some toc -> wrap toc nav in
  let nav = soup $ "nav" in
  wrap nav (create_element "header");
  begin match toc with
    | None -> sprintf "No TOC for %s" file |> pr
    | Some toc -> begin
        (* TOC - Create a title entry in the menu *)
        let a = create_element "a" ~inner_text:title
            ~attributes:["href", "#"] in
        let li = create_element "li" ~class_:"top" in
        append_child li a;
        prepend_child toc li;

        (* index of keywords *)
        if file = "index.html"
        then begin
          let keywords =
            body $$ "ul"
            |> fold (fun key ul ->
                match key with
                | None -> begin
                    match ul $$ "li" |> last with
                    | None -> None
                    | Some l -> begin match l $ "a" |> leaf_text with
                        | Some text -> sprintf "[%s]" text |> pr;
                          if text = "Index of keywords"
                          then l $ "a" |> attribute "href" else None
                        | None -> None
                      end
                  end
                | _ -> key) None in
          begin match keywords with
            | None -> pr "Could not find Index of keywords"
            | Some keywords ->
              let a = create_element "a" ~inner_text:"Index of keywords"
                  ~attributes:["href", keywords] in
              let li = create_element "li" in
              (append_child li a;
               append_child toc li)
          end;
          (* Link to APIs *)
          let a = create_element "a" ~inner_text:"OCaml API"
              ~attributes:["href", api_page_url ^ "/index.html"] in
          let li = create_element "li" in
          (append_child li a;
           append_child toc li);
          let a = create_element "a" ~inner_text:"OCaml Compiler API"
              ~attributes:["href", api_page_url ^ "/compilerlibref/index.html"] in
          let li = create_element "li" in
          (append_child li a;
           append_child toc li)
        end
      end
  end;

  (* Add back link to "OCaml Manual" *)
  if file <> "index.html" then begin
    let toc_title = create_element "div" ~class_:"toc_title" in
    let a = create_element "a" ~inner_text:"< The OCaml Manual"
        ~attributes:["href", "index.html"] in
    append_child toc_title a;
    prepend_child nav toc_title
  end;

  (* Add version number *)
  let version_text = if file = "index.html" then "Select another version"
    else "Version " ^ version in
  add_version_link nav version_text releases_url;

  (* Create new menu *)
  let menu = create_element "ul" ~class_:"part_menu" in
  List.iter (fun (title, href) ->
      let a = create_element "a" ~inner_text:title ~attributes:["href", href] in
      let li = if href = file
        then create_element "li" ~class_:"active"
        else create_element "li" in
      append_child li a;
      append_child menu li) chapters;
  (* let body = soup $ "div.content" in *)
  prepend_child body menu;

  (* Add logo *)
  begin match soup $? "header" with
    | None -> sprintf "Warning: no <header> for %s" file |> pr
    | Some header -> prepend_child header (logo_html "https://ocaml.org/")
  end;

  (* Move authors to the end. Versions >= 4.05 use c009. *)
  ["span.c009"]
  |> List.iter (fun selector ->
      soup $? selector
      |> do_option (fun authors ->
          match leaf_text authors with
          | None -> ()
          | Some s ->
            match Str.search_forward (Str.regexp "(.+written by.+)") s 0 with
            | exception Not_found -> ()
            | _ ->
              pr "Moving authors";
              delete authors;
              add_class "authors" authors;
              append_child body authors));

  (* Get the list of external files linked by the current file *)
  let xfiles = match toc with
    | None -> []
    | Some toc ->
      toc $$ "li"
      |> fold (fun list li ->
          let rf = li $ "a" |> R.attribute "href" in
          sprintf "TOC reference = %s" rf |> pr;
          if not (String.contains rf '#') &&
             not (starts_with ".." rf) &&
             not (starts_with "http" rf)
          then begin
            li $ "a" |> set_attribute "href" (rf ^ "#start-section");
            rf::list
          end else list) []
  in

  (* Add copyright *)
  append_child body (copyright ());

  (* Generate external files *)
  List.iter (clone_structure soup) xfiles;

  (* And finally save *)
  save_to_file soup file

(* Completely process the given version of the manual. Returns the names of the
   main html files. *)
let process version =
  print_endline (sprintf "\nProcessing version %s into %s...\n" version docs_maindir);

  pr (sprintf "Current directory is: %s" (Sys.getcwd ()));
  sys_mkdir docs_maindir;

  pr "* Generating css";
  compile_css "scss/manual.scss" (docs_file "manual.css");

  pr "* Copying logo";
  ["colour-logo-gray.svg"]
  |> List.iter (fun file ->
      pr file;
      sys_cp (process_dir // "images" // file) (docs_file file)
    );

  (* special case of the "index.html" file *)
  convert version [] ("The OCaml Manual", "index.html");

  let parts = ["tutorials"; "refman"; "commands"; "library" ] in
  let main_files = List.fold_left (fun list part ->
      pr "* Scanning index";
      let chapters = index part in

      pr "* Processing chapters";
      List.iter (convert version chapters) chapters;
      (snd (List.hd chapters)) :: list) [] parts in

  main_files

(******************************************************************************)

let () =
  let _list = process (find_version ()) in
  print_endline "DONE."

(*
   Local Variables:
   compile-command:"dune build"
   End:
*)
