(* ------------ Ocaml Web-manual -------------- *)

(* Copyright San Vu Ngoc, 2020

   file: process_api.ml

   Post-processing the HTML of the OCaml API.  *)

open Soup
open Printf
open Common

let compiler_libref = ref false
(* set this to true to process compilerlibref instead of libref *)

let dst_dir = ref ""
let src_dir = ref ""

let set_dirs () =
  let libref = if !compiler_libref then "compilerlibref" else "libref" in
  dst_dir :=
    if !compiler_libref then api_dir // libref else api_dir;
  src_dir := html_maindir // libref

let () = set_dirs ()

let favicon () = if !compiler_libref then "../favicon.ico" else "favicon.ico"

(* HTML code for the search widget. We don't add the "onchange" event because it
   forces to click twice to an external link after entering text. *)
let search_widget with_description =
  sprintf "<div class=\"api_search\"><input type=\"text\" name=\"apisearch\" id=\"api_search\"
	 oninput    = \"mySearch(%b);\"
         onkeypress = \"this.oninput();\"
         onclick    = \"this.oninput();\"
	 onpaste    = \"this.oninput();\">
<img src=\"search_icon.svg\" alt=\"Search\" class=\"svg\" onclick=\"mySearch(%b)\">%s</div>
<div id=\"search_results\"></div>" with_description with_description
    (if with_description
     then {|<span class="search_comment">(search values, type signatures, and descriptions - case sensitive)<div class="search_help"><ul><li>You may search bare values, like <code>map</code>, or indicate the module, like <code>List.map</code>, or type signatures, like <code>int -> float</code>.</li><li>To combine several keywords, just separate them by a space - except if you want to search type signatures: then you must use <strong>two</strong> spaces as separators.</li><li>You may use the special chars <code>^</code> and <code>$</code> to indicate where the matched string should start or end, respectively.</li></ul></div></span>|}
     else "")
  |> parse

let process ?(search=true) ~version file out =

  dbg "Processing %s..." file;
  let html = read_file file in
  let soup = parse html in

  (* Add javascript files *)
  let head = soup $ "head" in
  if search then begin
    create_element "script" ~attributes:["src","search.js"]
    |> append_child head
  end;
  create_element "script" ~attributes:["src","scroll.js"]
  |> append_child head;

  (* Add favicon *)
  let head = soup $ "head" in
  parse ({|<link rel="shortcut icon" type="image/x-icon" href="|} ^ favicon () ^ {|">|})
  |> append_child head;

  (* Add api wrapper *)
  let body = wrap_body ~classes:["api"] soup in

  (* Delete previous/up/next links *)
  body $? "div.navbar"
  |> do_option delete;

  (* Create TOC with H2 and H3 elements *)
  (* Cf Scanf for an example with H3 elements *)
  let header = create_element "header" in
  prepend_child body header;
  let nav = create_element "nav" ~class_:"toc" in
  append_child header nav;
  let ul = create_element "ul" in
  append_child nav ul;
  (* Create a "li" element inside "ul" from a header "h" (h2 or h3 typically) *)
  let li_of_h ul h =
    let li_current = create_element "li" in
    append_child ul li_current;
    let () = match attribute "id" h with
      | Some id ->
          let href = "#" ^ id in
          let a = create_element "a" ~inner_text:(texts h |> String.concat "")
              ~attributes:["href", href] in
          append_child li_current a
      | None -> () in
    li_current in

  descendants body
  |> elements
  |> fold (fun (li_current, h3_current) h -> match name h with
      | "h2" ->
          li_of_h ul h, None
      | "h3" -> begin match h3_current with
          | Some h3 ->
              li_of_h h3 h, h3_current
          | None ->
              let h3 = create_element "ul" in
              append_child ul li_current;
              append_child li_current h3;
              li_of_h h3 h, Some h3
        end
      | _ -> li_current, h3_current) (create_element "li", None);
  |> ignore;

  let title = soup $ "title" |> R.leaf_text in
  let href = let base = Filename.basename file in
    if String.sub base 0 5 = "type_"
    then String.sub base 5 (String.length base - 5) else "#top" in
  let a = create_element "a" ~inner_text:title ~attributes:["href", href] in
  let div = create_element ~class_:"toc_title" "div" in
  append_child div a;
  prepend_child nav div;

  (* In case of indexlist, add it to TOC *)
  (* This only happens for "index.html" *)
  let () = match body $? "ul.indexlist" with
    | Some uli ->
        delete uli;
        append_child ul uli;
        unwrap uli;
        if search then search_widget true |> prepend_child body;
        create_element "h1" ~inner_text:
          (sprintf "The OCaml %sAPI"
             (if !compiler_libref then "Compiler " else ""))
        |> prepend_child body;
    | None ->
        if search then search_widget false |> prepend_child nav;
        (* Add "general index" link to all other files *)
        create_element "a" ~inner_text:"< General Index"
          ~attributes:["href", "index.html"]
        |> prepend_child nav in

  (* Add version number *)
  add_version_link nav ((if !compiler_libref then "Compiler " else "") ^
                        "API Version " ^ version) releases_url;

  (* Add logo *)
  prepend_child header (logo_html
                          ((if !compiler_libref then "../" else "") ^
                           (manual_page_url ^ "/index.html")));

  dbg "Saving %s..." out;

  (* Save new html file *)
  let new_html = to_string soup in
  write_file out new_html

let process ?(overwrite=false) ~version file out =
  if overwrite || not (Sys.file_exists out)
  then Ok (process ~version file out)
  else Error (sprintf "File %s already exists." out)

let all_html_files () =
  Sys.readdir !src_dir |> Array.to_list
  |> List.filter (fun s -> Filename.extension s = ".html")


(* Generate the index.js file for searching with the quick search widget *)
(* The idea is to parse the file "index_values.html" to extract, for each entry
   of this index, the following information (list of 8 strings):

   [Module name; href URL of the Module (in principle an html file);
   Value name; href URL of the value;
   short description (html format); short description in txt format;
   type signature (html format); type signature in txt format]

   The "txt format" versions are used for searching, the "html version"
   for display.
   The signature is not in the "index_values.html" file, we have to look for it
   by following the value href.

   The index_values.html file has the following structure:

   (...)

   <table>

   (...)

   <tr><td><a href="List.html#VALappend">append</a> [<a href="List.html">List</a>]</td>
   <td><div class="info">
   <p>Concatenate two lists.</p>

   </div>
   </td></tr>

   (...)

   </table>

   (...)

   So we need to visit "List.html#VALappend", which has the following structure:

   <pre><span id="VALappend"><span class="keyword">val</span> append</span> : <code class="type">'a list -> 'a list -> 'a list</code></pre>

   and we finally return

   ["List"; "List.html";
   "rev_append"; "List.html#VALrev_append";
   "<div class=\"info\">  <p><code class=\"code\"><span class=\"constructor\">List</span>.rev_append&nbsp;l1&nbsp;l2</code> reverses <code class=\"code\">l1</code> and concatenates it to <code class=\"code\">l2</code>.</p> </div>"; "  List.rev_append\194\160l1\194\160l2 reverses l1 and concatenates it to l2. ";
   "<code class=\"type\">'a list -&gt; 'a list -&gt; 'a list</code>"; "'a list -> 'a list -> 'a list"]

*)

let anon_t_regexp = Str.regexp "\\bt\\b"
let space_regexp = Str.regexp " +"
let newline_regexp = Str.regexp_string "\n"

(* Remove "\n" and superfluous spaces in string *)
let one_line s =
  Str.global_replace newline_regexp " " s
  |> Str.global_replace space_regexp " "

(* Look for signature (with and without html formatting);
     [id] is the HTML id of the value. Example:
   # get_sig ~id_name:"VALfloat_of_int" "Stdlib.html";;
   Looking for signature for VALfloat_of_int in Stdlib.html
   Signature=[int -> float]
   - : (string * string) option =
   Some ("<code class=\\\"type\\\">int -&gt; float</code>", "int -> float")
  *)
let get_sig ?mod_name ~id_name file  =
  dbg "Looking for signature for %s in %s" id_name file;
  let soup = read_file (!src_dir // file) |> parse in
  (* Now we jump to the html element with id=id_name. Warning, we cannot use the
     CSS "#id" syntax for searching the id -- like in: soup $ ("#" ^ id) --
     because it can have problematic chars like id="VAL( * )" *)
  let span =  soup $$ "pre span"
              |> filter (fun s -> id s = Some id_name)
              |> first |> require in
  let pre = match parent span with
    | None -> failwith ("Cannot find signature for " ^ id_name)
    | Some pre -> pre in
  let code = pre $ ".type" in
  let sig_txt = texts code
                |> String.concat ""
                |> String.escaped in
  (* We now replace anonymous "t"'s by the qualified "Module.t" *)
  let sig_txt = match mod_name with
    | None -> sig_txt
    | Some mod_name ->
        Str.global_replace anon_t_regexp (mod_name ^ ".t") sig_txt in
  dbg "Signature=[%s]" sig_txt;
  Some (to_string code |> String.escaped, sig_txt)

let parse_pair = function
  | [a; b] -> (a,b)
  | _ -> raise (Invalid_argument "parse_pair")

(* Example: "Buffer.html#VALadd_subbytes" ==> Some "VALadd_subbytes" *)
let get_id ref =
  match String.split_on_char '#' ref with
  | [file; id] -> Some (file, id)
  | _ -> dbg "Could not find id for %s" ref; None

let parse_tdlist ~with_sig tdlist =
  let mdule, value, info = match tdlist with
    | [alist, []] -> let mdule, value = parse_pair alist in
        dbg "%s" (fst value);
        (mdule, value, ("",""))
    | [[], infolist; alist, []] ->
        let mdule, value = parse_pair alist in
        let infohtml, infotext = List.split infolist in
        let infohtml = List.map to_string infohtml
                       |> String.concat " "
                       |> one_line
                       |> String.escaped in
        let infotext = String.concat " " infotext
                       |> one_line
                       |> String.escaped in
        dbg "%s" (fst value);
        (mdule, value, (infohtml, infotext))
    | _ -> raise (Invalid_argument "parse_tdlist") in
  let signature =
    if with_sig then
      get_id (snd value)
      |> flat_option (fun (file,id_name) -> get_sig ~mod_name:(fst mdule) ~id_name file)
    else None in
  (mdule, value, info, signature)

let make_index ?(with_sig = true) () =
  let html = read_file (!src_dir // "index_values.html") in
  let soup = parse html in
  soup $ "table"
  |> select "tr"
  |> fold (fun trlist tr ->
      let tdlist =
        tr $$ "td" (* We scan the row; it should contain 2 <td> entries *)
        |> fold (fun tdlist td -> (* TODO instead of folding we may use directly
                                     the first 2 entries *)
            let alist = td $$ ">a" (* This should return something only with the first <td>. *)
                        |> fold (fun alist a ->
                            (R.leaf_text a, R.attribute "href" a) :: alist
                          ) [] in
            let infolist = td $$ "div.info" (* Second <td> *)
                           |> to_list
                           |> List.map (fun info ->
                               let infotext = texts info
                                              |> String.concat "" in
                               (info, infotext)) in
            if alist = [] && infolist = [] then tdlist
            else (alist, infolist) :: tdlist
          ) [] in
      if tdlist = [] then trlist
      else (parse_tdlist ~with_sig tdlist) :: trlist
    ) []


(* Fast version of make_index using Scanf *)
(******************************************)
module Index = struct

  open Scanf
  let index_len = 2545

  let sid x : string = x

  let rec find ch word =
    if Scanning.end_of_input ch then raise Not_found
    else if bscanf ch "%s " sid <> word then find ch word

  (* Return all words encountered before reaching [word]. All 'spaces' between
     words are replaced by single ' 's. *)
  let concat_before ch word =
    let b = Buffer.create 256 in
    let rec loop () =
      if Scanning.end_of_input ch then raise Not_found;
      let next = bscanf ch "%s " sid in
      if next <> word then begin
        Buffer.add_char b ' ';
        Buffer.add_string b next;
        loop ()
      end else Buffer.contents b in
    loop ()

  let extract_infotext list =
    list |> List.map (fun (a, (val_name, val_ref), info, signature) ->
        let infotext = Soup.texts (Soup.parse info) |> String.concat "" in
        let val_name = Soup.(parse val_name |> R.leaf_text) in
        (* We parse [val_name] simply to replace "&amp;" by "&", etc... it could
           be done much faster manually of course (saving 20% time...): there
           are only a very limited number of such cases. *)
        (a, (val_name, val_ref),
         (String.escaped info, String.escaped infotext), signature))


  (* Remove all <a> tags. Example:
     {|'a <a href="Event.html#TYPEevent">event</a> -> ('a -> 'b) -> 'b <a href="Event.html#TYPEevent">event</a>|} ==> "'a event -> ('a -> 'b) -> 'b event"
  *)
  let strip_tag =
    let reg = Str.regexp {|<[^>]+ [^>]+>\([^<]+\)</[^>]+>|} in
    let reg_br = Str.regexp "<br>" in
    let reg_space = Str.regexp "  +" in
    fun s -> Str.global_replace reg "\\1" s
             |> Str.global_replace reg_br " "
             |> Str.global_replace reg_space " "

  (* Look for signature (with and without html formatting);
     [id] is the HTML id of the value. Example:
     # get_sig ~id:"VALfloat_of_int" "Pervasives.html";;
     - : string option = Some "int -> float"
  *)
  let get_sig ?mod_name ~id file  =
    dbg "Looking for signature for %s in %s" id file;
    let anon_t_regexp = Str.regexp "\\bt\\b" in
    let inch = open_in (!src_dir // file) in
    let reg = Str.regexp_string (sprintf {|id="%s"|} id) in
    (* let reg_type = Str.regexp {|<code class="type">\(.+\)</code>|} in *)
    let rec loop () = try
        let line = input_line inch in
        try let _ = Str.search_forward reg line 0 in
          let code = parse line $ "code.type" in
          let sig_txt = texts code
                        |> String.concat ""
                        |> String.escaped in

          (* We now replace anonymous "t"'s by the qualified "Module.t" *)
          let sig_txt = match mod_name with
            | None -> sig_txt
            | Some mod_name ->
              Str.global_replace anon_t_regexp (mod_name ^ ".t") sig_txt in
          (* let _ = Str.search_forward reg_type line 0 in
           * let the_sig = Str.matched_group 1 line |> strip_a_tag in *)
          dbg "Signature=[%s]" sig_txt;
          close_in inch; Some (to_string code |> String.escaped, sig_txt)
        with
        | Not_found -> loop ()
      with
      | End_of_file -> close_in inch; None in
    loop ()

  (* Example: "Buffer.html#VALadd_subbytes" ==> Some "VALadd_subbytes" *)
  let get_id ref =
    try let i = String.index ref '#' in
      Some (String.sub ref (i+1) (String.length ref - i - 1))
    with Not_found -> dbg "Could not find id for %s" ref; None

  let make ?(with_sig = true) () =
    let ch = Scanning.open_in (!src_dir // "index_values.html") in
    find ch "<table>";
    let rec loop list =
      if try find ch "<tr><td><a"; false with Not_found -> true
      then list
      else
        (* Scan value reference *)
        let val_ref = bscanf ch " href=%S" sid in
        let val_name = bscanf ch ">%s@<" sid in
        find ch  "[<a";

        (* Scan module reference (ie. filename) *)
        let mod_ref = bscanf ch " href=%S" sid in
        let mod_name = bscanf ch ">%s@<" sid in
        bscanf ch "/a>]</td> <td>" ();

        let signature =
          if with_sig then
            get_id val_ref
            |> flat_option (fun id -> get_sig ~mod_name ~id mod_ref)
          else None in

        (* Scan info *)
        let info = match bscanf ch "<%s@>" sid with
          | "/td" -> find ch "</tr>"; ""
          | "div class=\"info\"" ->
            let s = concat_before ch "</td></tr>" in
            "<div class=\"info\">" ^ s
          | s -> raise (Scan_failure s) in
        dbg "%s" val_ref;

        let new_entry = ((mod_name, mod_ref), (val_name, val_ref), info, signature) in
        loop (new_entry :: list) in
    loop []
    |> extract_infotext
end
(******************************************)

let save_index file index =
  let outch = open_out file in
  output_string outch "var GENERAL_INDEX = [\n";
  index
  |> List.iter (fun ((mod_name, mod_ref), (val_name, val_ref),
                     (info_html, info_txt), signature) ->
      fprintf outch {|["%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s"],|}
        mod_name mod_ref val_name val_ref info_html info_txt
        (map_option fst signature |> string_of_opt)
        (map_option snd signature |> string_of_opt);
      output_string outch "\n");
  output_string outch "]\n";
  close_out outch

let process_index ?(fast=true) () =
  dbg "Recreating index file, please wait...";
  let t = Unix.gettimeofday () in
  let index = if fast then Index.make () else make_index () in
  dbg "Index created. Time = %f\n" (Unix.gettimeofday () -. t);
  save_index (!dst_dir // "index.js") index;
  dbg "Index saved. Time = %f\n" (Unix.gettimeofday () -. t)

let process_html overwrite version =
  print_endline (sprintf "\nProcessing version %s into %s...\n" version !dst_dir);
  let processed = ref 0 in
  all_html_files ()
  |> List.iter (fun file ->
      match process ~overwrite ~version
              (!src_dir // file)
              (!dst_dir // file) with
      | Ok () -> incr processed
      | Error s -> dbg "%s" s
    );
  sprintf "Version %s, HTML processing done: %u files have been processed."
    version !processed |> print_endline

let copy_files () =
  let ind = !dst_dir // "index.js" in
  if not (Sys.file_exists ind) then process_index ()

(******************************************************************************)

let () =
  let version = find_version () in
  let args = Sys.argv |> Array.to_list |> List.tl in
  compiler_libref := List.mem "compiler" args;
  set_dirs ();
  let overwrite = List.mem "overwrite" args in
  let makeindex = List.mem "makeindex" args in
  let makehtml = List.mem "html" args || not makeindex in
  if makehtml then process_html overwrite version;
  if makeindex then process_index ();
  copy_files ();
  print_endline "DONE."

(*
   Local Variables:
   compile-command:"dune build"
   End:
*)
