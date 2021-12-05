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

let preg_anyspace =
  String.concat "\\|"
    ["\u{00a0}"; (* NO-BREAK SPACE *)
     "\u{2000}"; (* EN QUAD *)
     "\u{2001}"; (* EM QUAD *)
     "\u{2002}"; (* EN SPACE *)
     "\u{2003}"; (* EM SPACE *)
     "\u{2004}"; (* THREE-PER-EM SPACE *)
     "\u{2005}"; (* FOUR-PER-EM SPACE *)
     "\u{2006}"; (* SIX-PER-EM SPACE *)
     "\u{2007}"; (* FIGURE SPACE *)
     "\u{2008}"; (* PUNCTUATION SPACE *)
     "\u{2009}"; (* THIN SPACE *)
     "\u{200a}"; (* HAIR SPACE *)
     "\u{202f}"; (* NARROW NO-BREAK SPACE *)
    ]
  |> sprintf "\\(%s\\)+"

(* WARNING these are sensitive to Hevea fluctuations: *)
(* "long" space is either " " (hevea 2.32) or "\u{2003}" (hevea 2.35) *)
let preg_emspace = "\\(\u{2003}\\| \\)"
(* What hevea inserts between "Chapter" and the chapter number: *)
let preg_chapter_space = "\\(\u{2004}\u{200d}\\|" ^ preg_anyspace ^ "\\)"
let writtenby_css = "span.font-it" (* "span.c009" for hevea 2.32 *)

(* Remove number: "Chapter 1  The core language" ==> "The core language" *)
let remove_number s =
  Re.Str.(global_replace (regexp (".+" ^ preg_emspace)) "" s)

let toc_get_title li =
  let a = li $ "a[href]" in
  let title = trimmed_texts a |> String.concat " "
              |> remove_number in
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


(* New UTF8 space chars have been introduced in Hevea 2.35. In Hevea 2.32, only
   html nb_spaces "&#XA0;" were used. With 2.35 we have
   'Chapter\u2004\u200d2\u2003The module system'. The \u200d is Zero Width
   Joiner and should probably not be used here, see
   https://github.com/maranget/hevea/pull/61 *)

let reg_chapter = Re.Str.regexp
    ("Chapter" ^ preg_chapter_space ^ "\\([0-9]+\\)" ^ preg_anyspace)

let load_html file =
  dbg "%s" file;
  (* First we perform some direct find/replace in the html string. *)
  let html =
    read_file (html_file file)
    (* Normalize non-break spaces to the utf8 \u00A0: *)
    |> Re.Str.(global_replace (regexp_string "&#XA0;") " ")
    |> Re.Str.(global_replace reg_chapter)
      (if file = "index.html" then {|<span class="number">\3.</span> |}
       else {|<span class="chapter-number">Chapter \3</span> |})

    (* I think it would be good to replace "chapter" by "tutorial" for part
       I. The problem of course is how we number chapters in the other parts. *)

    (* |> Re.Str.global_replace (Re.Str.regexp_string "chapter") "tutorial"
     * |> Re.Str.global_replace (Re.Str.regexp_string "Chapter") "Tutorial" *)

    (* Remove the chapter number in local links, it makes the TOC unnecessarily
       unfriendly. *)
    |> Re.Str.(global_replace
                 (regexp (">[0-9]+\\.\\([0-9]+\\)" ^ preg_anyspace)))
      {|><span class="number">\1</span> |}
    |> Re.Str.(global_replace
                 (regexp ("[0-9]+\\.\\([0-9]+\\(\\.[0-9]+\\)+\\)" ^ preg_anyspace)))
      {|<span class="number">\1</span> |}

    (* The API (libref and compilerlibref directories) should be separate
       entities, to better distinguish them from the manual. *)
    |> Re.Str.(global_replace (regexp_string "\"libref/"))
      (sprintf "\"%s/" api_page_url)
    |> Re.Str.(global_replace (regexp_string "\"compilerlibref/")
                 (sprintf "\"%s/compilerlibref/" api_page_url))
  in

  (* For the main index file, we do a few adjustments *)
  let html = if file = "index.html"
    then Re.Str.(global_replace
                   (regexp ("Part" ^ preg_chapter_space ^ "\\([I|V]+\\)<br>\n"))
                   {|<span class="number">\3.</span> |} html)
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

(* Replace the images of one of the "previous, next, up" link by the title of
   the reference. *)
let nav_replace_img_by_text toc alt a img =
  let file = R.attribute "href" a in
  let title = match file_title file toc with
    | Some f -> begin match alt with
        | "Previous" -> "« " ^ f
        | "Next" -> f ^ " »"
        | "Up" -> f
        | _ -> failwith "This should not happen"
            end
    | None -> dbg "Unknown title for file %s" file; file in
  let txt = create_text title in
  replace img txt;
  add_class (String.lowercase_ascii alt) a

(* Replace three links "Previous, Up, Next" at the end of the file by more
   useful titles, and insert then in a div container, keeping only 2 of them:
   either (previous, next) or (previous, up) or (up, next). Remove them at the
   top of the file, where they are not needed because we have the TOC. *)
let update_navigation soup toc =
  Option.iter delete (soup $? "hr");
  let links =
    ["Previous"; "Up"; "Next"]
    |> List.map (fun alt -> alt, to_list (soup $$ ("img[alt=\"" ^ alt ^ "\"]")))
    (* In principle [imgs] will contain either 0 or 2 elements. *)
    |> List.filter (fun (_alt, imgs) -> List.length imgs = 2)
    (* We delete the first link, and replace image by text *)
    |> List.map (fun (alt, imgs) ->
        delete (R.parent (List.hd imgs));
        let img = List.hd (List.rev imgs) in
        let a = R.parent img in
        nav_replace_img_by_text toc alt a img;
        a) in
  if links <> [] then begin
    (* We keep only 2 links: first and last *)
    let a1, a2 = match links with
      | [prev;up;next] -> delete up; (prev, next)
      | [a;b] -> (a,b)
      | _ -> failwith "Navigation link should have at least 2 elements" in
    add_class "previous" a1;
    add_class "next" a2;
    (* some elements can have both previous and up classes, for instance. This
       helps css styling. *)
    let container = create_element ~class_:"bottom-navigation" "div" in
    wrap a1 container;
    append_child container a2
  end


(* extract the cut point (just after title) and the header of soup:
   "insert_xfile_content" needs them to insert external files after the cut point,
   and include the TOC. *)
let make_template soup =
  let header = soup $ "header" in
  let title = match soup $? "div.maintitle" with
    | Some div -> div (* This is the case for "index.html" *)
    | None -> soup $ "h1" in
  title, header

(* Create a new file by keeping only the head/headers parts of "soup", deleting
   everything after the title, and inserting the content of external file (hence
   preserving TOC and headers) (WARNING: this mutates soup) *)
let insert_xfile_content soup (title, header) toc xfile =
  let xternal = parse (load_html xfile) in
  update_navigation xternal toc;
  Option.iter delete (xternal $? "hr");
  let xbody = xternal $ "body" in
  insert_after title xbody;
  create_element ~id:"start-section" "a"
  |> insert_after title;
  insert_after title header;
  next_siblings xbody
  |> iter delete;
  insert_after xbody (copyright ());
  set_name "section" xbody;
  set_attribute "id" "section" xbody;
  save_to_file soup xfile

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
  soup $$ "blockquote" |> last |> Option.iter delete;
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

let change_title title soup =
  let title_tag = soup $ "title" in
  let new_title = create_element "title" ~inner_text:("OCaml - " ^ title) in
  replace title_tag new_title

(* Create left sidebar for TOC.  *)
let make_toc_sidebar ~version ~title file body =
  let toc = match body $? "ul" with
    | None -> None (* can be None, eg chapters 15,19...*)
    | Some t -> if classes t <> [] (* as in libthreads.html or parsing.html *)
        then (dbg "We don't promote <UL> to TOC for file %s" file; None)
        else Some t in

  let () = match body $? "h2.section", toc with
    | None, Some toc ->
        (* If file has "no content" (sections), we clone the toc to leave it in
           the main content. This applies to "index.html" as well. *)
        let original_toc = parse (to_string toc) in
        original_toc $ "ul"
        |> add_class "ul-content";
        insert_after toc original_toc
    | _ -> () in

  let nav = create_element "nav" ~class_:"toc" in
  let () = match toc with
    | None -> prepend_child body nav
    | Some toc -> wrap toc nav in
  let nav = body $ "nav" in
  wrap nav (create_element ~id:"sidebar" "header");
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
  toc

 (* Create menu for all chapters in the part *)
let make_part_menu ~part_title chapters file body =
  let menu = create_element "ul" ~id:"part-menu" in
  List.iter (fun (href, title) ->
      let a = create_element "a" ~inner_text:title ~attributes:["href", href] in
      let li = if href = file
        then create_element "li" ~class_:"active"
        else create_element "li" in
      append_child li a;
      append_child menu li) chapters;
  prepend_child body menu;

  (* Add part_title just before the part-menu *)
  if part_title <> "" then begin
    let nav = create_element ~id:"part-title" "nav" ~inner_text:part_title in
    create_element "span" ~inner_text:"☰"
    |> prepend_child nav;
    prepend_child body nav
  end

(* Add logo *)
let add_logo file soup =
  match soup $? "header" with
  | None -> dbg "Warning: no <header> for %s" file
  | Some header -> prepend_child header (logo_html "https://ocaml.org/")

(* Move authors to the end *)
let move_authors body =
  body $? writtenby_css
  |> Option.iter (fun authors ->
      match leaf_text authors with
      | None -> ()
      | Some s ->
          match Re.Str.(search_forward (regexp "(.+written by.+)") s 0) with
          | exception Not_found -> ()
          | _ ->
              dbg "Moving authors";
              delete authors;
              add_class "authors" authors;
              append_child body authors)

(* Get the list of external files linked by the current file *)
let get_xfiles = function
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

(* This is the main script for processing a specified file. [convert] has to be
   run for each "entry" [file] of the manual, making a "Chapter". (The list of
   [chapters] corresponds to a "Part" of the manual.) *)
let convert version (part_title, chapters) toc_table (file, title) =
  dbg "%s ==> %s" (html_file file) (docs_file file);

  (* Parse html *)
  let soup = parse (load_html file) in

  (* Change title, add javascript and favicon *)
  change_title title soup;
  update_head soup;

  (* Wrap body. *)
  let c = if file = "index.html" then ["manual"; "content"; "index"]
    else ["manual"; "content"] in
  let body = wrap_body ~classes:c soup in

  if file = "index.html" then convert_index version soup;

  (* Make sidebar *)
  let toc = make_toc_sidebar ~version ~title file body in

  (* Make top menu for chapters *)
  make_part_menu ~part_title chapters file body;

  (* Add side-bar button before part_title *)
  add_sidebar_button body;

  (* Add logo *)
  add_logo file soup;

  (* Move authors to the end *)
  move_authors body;

  (* Bottom navigation links *)
  update_navigation soup toc_table;

  (* Add copyright *)
  append_child body (copyright ());

  (* Save html *)
  save_to_file soup file;

  (* Finally, generate external files to be converted (this should be done at
     the end because it deeply mutates the original soup) *)
  let xfiles = get_xfiles toc in
  let template = make_template soup in
  List.iter (insert_xfile_content soup template toc_table) xfiles


(* Completely process the given version of the manual. Returns the names of the
   main html files. *)
let process version =
  print_endline (sprintf "\nProcessing version %s into %s...\n" version docs_maindir);

  dbg "Current directory is: %s" (Sys.getcwd ());

  dbg "* Scanning index";
  let toc_table, all_chapters = parse_toc () in

  (* special case of the "index.html" file: *)
  convert version ("", []) toc_table ("index.html", "The OCaml Manual");

  let main_files = List.fold_left (fun list (part_title, chapters) ->
      dbg "* Processing chapters for %s" part_title;
      List.iter (convert version (part_title, chapters) toc_table) chapters;
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
