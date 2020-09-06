(* ------------ Ocaml Web-manual -------------- *)

(* Copyright San Vu Ngoc, 2020

   file: process_api.ml

   Post-processing the HTML of the OCaml Manual.

   (The "API" side is treated by process_api.ml) *)

open Soup
open Printf
open Common

(* How the main index.html page will be called: *)
let index_title = "Home"

(* Alternative formats for the manual: *)
let archives =
  ["refman-html.tar.gz"; "refman.txt"; "refman.pdf"; "refman.info.tar.gz"]

(* Remove number: "Chapter 1  The core language" ==> "The core language" *)
let remove_number s =
  Re.Str.(global_replace (regexp ".+  ") "" s)

let toc_get_title li =
  let r = Re.Str.regexp ".+  " in
  let a = li $ "a[href]" in
  let title = trimmed_texts a |> String.concat " "
              |> Re.Str.global_replace r "" in
  let file = R.attribute "href" a
             |> String.split_on_char '#'
             |> List.hd in
  file, title

let register_toc_entry toc_table name li =
  let file, title = toc_get_title li in
  dbg "%s : %s" name title;
  if not (Hashtbl.mem toc_table file)
  then begin
    Hashtbl.add toc_table file title;
    dbg "Registering %s => %s" file title
  end;
  file, title

(* Scan manual001.html and return two things:
   1. [toc_table]: a table with (file ==> title)
   2. [all_chapters]: the list of parts: (part_title, chapters), where
   chapters is a list of (title, file) *)
let parse_toc () =
  let toc_table = Hashtbl.create 50 in
  Hashtbl.add toc_table "manual001.html" "Contents";
  Hashtbl.add toc_table "foreword.html" "Foreword";
  Hashtbl.add toc_table "manual071.html" "Keywords";

  let soup = read_file (html_file "manual001.html") |> parse in
  let toc = soup $ "ul.toc" in
  let all_chapters =
    toc $$ ">li.li-toc" (* Parts *)
    |> fold (fun all_chapters li ->
        let _file, title = toc_get_title li in
        dbg "Part: %s " title;
        let chapters =
          li $$ ">ul >li.li-toc" (* Chapters *)
          |> fold (fun chapters li ->
              let file, title = register_toc_entry toc_table "  Chapters" li in
              li $$ ">ul >li.li-toc" (* Sections *)
              |> iter (ignore << (register_toc_entry toc_table "    Section"));
              (file,title) :: chapters) []
        |> List.rev in
        if chapters = [] then all_chapters
        else (title, chapters) :: all_chapters) [] in
  toc_table, all_chapters

(* This string is updated by [extract_date] *)
let copyright_text = ref "Copyright © 2020 Institut National de Recherche en Informatique et en Automatique"

let copyright () =
  "<div class=\"copyright\">" ^ !copyright_text ^ "</div>"
  |> parse

let load_html file =
  dbg "%s" file;
  (* First we perform some direct find/replace in the html string. *)
  let html =
    read_file (html_file file)
    (* Normalize non-break spaces: *)
    |> Re.Str.(global_replace (regexp_string "&#XA0;") " ")
    |> Re.Str.(global_replace (regexp "Chapter \\([0-9]+\\)"))
      (if file = "index.html" then "<span>\\1.</span>"
       else "<span>Chapter \\1</span>")

    (* I think it would be good to replace "chapter" by "tutorial" for part
       I. The problem of course is how we number chapters in the other parts. *)

    (* |> Re.Str.global_replace (Re.Str.regexp_string "chapter") "tutorial"
     * |> Re.Str.global_replace (Re.Str.regexp_string "Chapter") "Tutorial" *)

    (* Remove the chapter number in local links, it makes the TOC unnecessarily
       unfriendly. *)
    |> Re.Str.(global_replace (regexp ">[0-9]+\\.\\([0-9]+\\) ") ">\\1 ")
    |> Re.Str.(global_replace (regexp "[0-9]+\\.\\([0-9]+\\.[0-9]+\\) "))
      "\\1 "

    (* The API (libref and compilerlibref directories) should be separate
       entities, to better distinguish them from the manual. *)
    |> Re.Str.(global_replace (regexp_string "\"libref/"))
      (sprintf "\"%s/" api_page_url)
    |> Re.Str.(global_replace (regexp_string "\"compilerlibref/")
                 (sprintf "\"%s/compilerlibref/" api_page_url))
  in

  (* For the main index file, we do a few adjustments *)
  let html = if file = "index.html"
    then Re.Str.(global_replace (regexp "Part \\([I|V]+\\)<br>")
                   "<span>\\1. </span>" html)
    else html in

  (* Set utf8 encoding directly in the html string *)
  let charset_regexp = Re.Str.regexp "charset=\\([-A-Za-z0-9]+\\)\\(\\b\\|;\\)" in
  match Re.Str.search_forward charset_regexp html 0 with
  | exception Not_found -> dbg "Warning, no charset found in html."; html
  | _ -> match (String.lowercase_ascii (Re.Str.matched_group 1 html)) with
    | "utf-8" -> dbg "Charset is UTF-8; good."; html
    | "us-ascii" -> dbg "Charset is US-ASCII. We change it to UTF-8";
        Re.Str.global_replace charset_regexp "charset=UTF-8\\2" html
    | _ -> dbg "Warning, charset not recognized."; html

(* Save new html file *)
let save_to_file soup file =
  let new_html = to_string soup in
  write_file (docs_file file) new_html

(* Find title associated with file *)
let file_title file toc =
  if file = "index.html" then Some index_title
  else Hashtbl.find_opt toc file

(* Replace three links "Previous, Up, Next" at the end of the file by more
   useful titles, and insert then in a div contained, keeping only 2 of them:
   either (previous, next) or (previous, up) or (up, next). Remove them at the
   top of the file, where they are not needed because we have the TOC. *)
let update_navigation soup toc =
  do_option delete (soup $? "hr");
  let container, count =
    ["Previous"; "Up"; "Next"]
    |> List.fold_left (fun (container, count) s ->
        let imgs = soup $$ ("img[alt=\"" ^ s ^ "\"]") in
        (* In principle [imgs] will contain either 0 or 2 elements. We delete
           the first one. *)
        do_option (delete << R.parent) (first imgs);
        (* Now, if there is a second element, we update (or create) an "div"
           container to insert the element, and increase [count]. [count] is
           used to make sure we keep only 2 links, not 3. *)
        imgs |> fold (fun (container, count) img ->
            let a = R.parent img in
            let file = R.attribute "href" a in
            let title = match file_title file toc with
              | Some f -> begin match s with
                  | "Previous" -> "« " ^ f
                  | "Next" -> f ^ " »"
                  | "Up" -> f
                  | _ -> failwith "This should not happen"
                end
              | None -> dbg "Unknown title for file %s" file; s in
            let txt = create_text title in
            add_class (String.lowercase_ascii s) a;
            replace img txt;
            let div = match container with
              | Some c ->
                  if count = 2 then delete (children c |> R.last);
                  (* : we delete the "Up" link *)
                  append_child c a; c
              | None ->
                  let c = create_element ~class_:"bottom-navigation" "div" in
                  wrap a c; c in
            Some div, count+1) (container, count)) (None, 0) in

  match container with
  | None -> print_endline "No Navigation"
  | Some div ->
      if count = 2 then begin
        add_class "previous" (div $$ "a" |> R.first);
        add_class "next" (div $$ "a" |> R.last);
      end

(* Create a new file by cloning the structure of "soup", and inserting the
   content of external file (hence preserving TOC and headers) *)
let clone_structure soup toc xfile =
  let xternal = parse (load_html xfile) in
  update_navigation xternal toc;
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
              | _ -> dbg "Warning, date not found"; None

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
   run for each "entry" [file] of the manual, making a "Chapter". (The list of
   [chapters] corresponds to a "Part" of the manual.) *)
let convert version chapters toc_table (file, title) =
  dbg "%s ==> %s" (html_file file) (docs_file file);

  (* Parse html *)
  let soup = parse (load_html file) in

  (* Change title *)
  let title_tag = soup $ "title" in
  let new_title = create_element "title" ~inner_text:("OCaml - " ^ title) in
  replace title_tag new_title;

  (* Add javascript *)
  let head = soup $ "head" in
  create_element "script" ~attributes:["src","scroll.js"]
  |> append_child head;

  (* Wrap body. TODO use set_name instead *)
  let c = if file = "index.html" then ["manual"; "content"; "index"]
    else ["manual"; "content"] in
  let body = wrap_body ~classes:c soup in

  update_navigation soup toc_table;

  if file = "index.html" then convert_index version soup;

  (* Create left sidebar for TOC.  *)
  let toc = match soup $? "ul" with
    | None -> None (* can be None, eg chapters 15,19...*)
    | Some t -> if classes t <> [] (* as in libthreads.html or parsing.html *)
        then (dbg "We don't promote <UL> to TOC for file %s" file; None)
        else Some t in
  let nav = create_element "nav" ~class_:"toc" in
  let () = match toc with
    | None -> prepend_child body nav
    | Some toc -> wrap toc nav in
  let nav = soup $ "nav" in
  wrap nav (create_element "header");
  begin match toc with
  | None -> dbg "No TOC for %s" file
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
                      | Some text -> dbg "[%s]" text;
                          if text = "Index of keywords"
                          then l $ "a" |> attribute "href" else None
                      | None -> None
                    end
                end
              | _ -> key) None in
        begin match keywords with
        | None -> dbg "Could not find Index of keywords"
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
  List.iter (fun (href, title) ->
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
  | None -> dbg "Warning: no <header> for %s" file
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
              match Re.Str.(search_forward (regexp "(.+written by.+)") s 0) with
              | exception Not_found -> ()
              | _ ->
                  dbg "Moving authors";
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
            dbg "TOC reference = %s" rf;
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
  List.iter (clone_structure soup toc_table) xfiles;

  (* And finally save *)
  save_to_file soup file

(* Completely process the given version of the manual. Returns the names of the
   main html files. *)
let process version =
  print_endline (sprintf "\nProcessing version %s into %s...\n" version docs_maindir);

  dbg "Current directory is: %s" (Sys.getcwd ());

  dbg "* Scanning index";
  let toc_table, all_chapters = parse_toc () in

  (* special case of the "index.html" file: *)
  convert version [] toc_table ("index.html", "The OCaml Manual");

  let main_files = List.fold_left (fun list (part_title, chapters) ->
      dbg "* Processing chapters for %s" part_title;
      List.iter (convert version chapters toc_table) chapters;
      (fst (List.hd chapters)) :: list) [] all_chapters in

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
