(* ------------ Ocaml Web-manual -------------- *)

(* Copyright San Vu Ngoc, 2020

   file: process_api.ml

   Post-processing the HTML of the OCaml API.  *)

open Soup
open Printf
open Common

let compiler_libref = ref false
(* set this to true to process compilerlibref instead of libref *)

type config = {
  src_dir : string;
  dst_dir : string;
  title : string
}

(* HTML code for the search widget. We don't add the "onchange" event because it
   forces to click twice to an external link after entering text. *)
let search_widget with_description =
  let search_decription = if with_description
    then {|<span class="search_comment">(search values, type signatures, and descriptions - case sensitive)<span id="help_icon" onclick="showHelp()">ⓘ</span><div id="search_help" class="hide"><ul><li>You may search bare values, like <code>map</code>, or indicate the module, like <code>List.map</code>, or type signatures, like <code>int -> float</code>.</li><li>To combine several keywords, just separate them by a space. Quotes can be used to prevent from splitting words at spaces. For instance, <code>int array</code> will search for <code>int</code> and/or <code>array</code>, while <code>"int array"</code> will only list functions whose signature contains the <code>int array</code> type.</li><li>You may use the special chars <code>^</code> and <code>$</code> to indicate where the matched string should start or end, respectively. For instance <code>^zip</code> will not show you the <code>unzip</code> function.</li></ul></div></span>|}
    else "" in
  sprintf {|<div class="api_search"><input type="text" name="apisearch" id="api_search" class="api_search"
	 oninput    = "mySearch(%b);"
         onkeypress = "this.oninput();"
         onclick    = "this.oninput();"
	 onpaste    = "this.oninput();">
<img src="search_icon.svg" alt="Search" class="api_search svg" onclick="mySearch(%b)">%s</div>
<div id="search_results"></div>|} with_description with_description search_decription
  |> parse

(* We save parsed files in a table; this is just for speed optimization,
   especially for make_index (18sec instead of 50sec for the whole index); it
   can be removed.  Although if we really wanted a fast make_index, we would use
   Scanf all over the place ==> 1sec. Warning: the parsed files will be mutated
   by processing, so one should never process the same file twice. *)

let parsed_files = Hashtbl.create 50

let parse_file ?(original=false) file =
  match Hashtbl.find_opt parsed_files file with
  | Some soup ->
      if original then failwith (sprintf "File %s was already processed" file)
      else soup
  | None ->
      let soup = read_file file |> parse in
      Hashtbl.add parsed_files file soup;
      soup

(* Create TOC with H2 and H3 elements *)
(* Cf Scanf for an example with H3 elements *)
let make_toc ~version ~search file config title body =
  let header = create_element ~id:"sidebar" "header" in
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
          (sprintf "The OCaml %sAPI" config.title)
        |> prepend_child body;
    | None ->
        if search then search_widget false |> prepend_child nav;
        (* Add "general index" link to all other files *)
        create_element "a" ~inner_text:"< General Index"
          ~attributes:["href", "index.html"]
        |> prepend_child nav in

  (* Add version number *)
  add_version_link nav (config.title ^ "API Version " ^ version) releases_url;

  (* Add sidebar button for mobile navigation *)
  add_sidebar_button body;

  (* Add logo *)
  prepend_child header (logo_html
                          ((if config.title = "" then "" else "../") ^
                           (manual_page_url ^ "/index.html")))


let process ?(search=true) ~version config file out =

  dbg "Processing %s..." file;
  let soup = parse_file ~original:true file in

  (* Add javascript and favicon *)
  update_head ~search soup;

  (* Add api wrapper *)
  let body = wrap_body ~classes:["api"] soup in

  (* Delete previous/up/next links *)
  body $? "div.navbar"
  |> Option.iter delete;

  (* Add left sidebar with TOC *)
  let title = soup $ "title" |> R.leaf_text in
  make_toc ~version ~search file config title body;

  dbg "Saving %s..." out;

  (* Save new html file *)
  let new_html = to_string soup in
  write_file out new_html

let process ?(overwrite=false) ~version config file out =
  if overwrite || not (Sys.file_exists out)
  then Ok (process ~version config file out)
  else Error (sprintf "File %s already exists." out)

let all_html_files config =
  Sys.readdir config.src_dir |> Array.to_list
  |> List.filter (fun s -> Filename.extension s = ".html")


module Index = struct
  (* Generate the index.js file for searching with the quick search widget *)
  (* The idea is to parse the file "index_values.html" to extract, for each
     entry of this index, the following information (list of 8 strings):

     [Module name; href URL of the Module (in principle an html file); Value
     name; href URL of the value; short description (html format); short
     description in txt format; type signature (html format); type signature in
     txt format]

     The "txt format" versions are used for searching, the "html version" for
     display.  The signature is not in the "index_values.html" file, we have to
     look for it by following the value href.  The index_values.html file has
     the following structure:

     (...)

     <table>

     (...)

     <tr><td><a href="List.html#VALappend">append</a> [<a
     href="List.html">List</a>]</td> <td><div class="info"> <p>Concatenate two
     lists.</p>

     </div> </td></tr>

     (...)

     </table>

     (...)

     So we need to visit "List.html#VALappend", which has the following
     structure:

     <pre><span id="VALappend"><span class="keyword">val</span> append</span> :
     <code class="type">'a list -> 'a list -> 'a list</code></pre>

     and we finally return

     ["List"; "List.html"; "rev_append"; "List.html#VALrev_append"; "<div
     class=\"info\"> <p><code class=\"code\"><span
     class=\"constructor\">List</span>.rev_append&nbsp;l1&nbsp;l2</code>
     reverses <code class=\"code\">l1</code> and concatenates it to <code
     class=\"code\">l2</code>.</p> </div>"; "
     List.rev_append\194\160l1\194\160l2 reverses l1 and concatenates it to
     l2. "; "<code class=\"type\">'a list -&gt; 'a list -&gt; 'a list</code>";
     "'a list -> 'a list -> 'a list"]

  *)

  type item =
    { html : string; txt : string }

  type entry =
    { mdule : item;
      value : item;
      info : item;
      signature : item option }

  let anon_t_regexp = Re.Str.regexp "\\bt\\b"
  let space_regexp = Re.Str.regexp " +"
  let newline_regexp = Re.Str.regexp_string "\n"

  (* Remove "\n" and superfluous spaces in string *)
  let one_line s =
    Re.Str.global_replace newline_regexp " " s
    |> Re.Str.global_replace space_regexp " "
    |> String.trim

  (* Look for signature (with and without html formatting);
     [id] is the HTML id of the value. Example:
     # get_sig ~id_name:"VALfloat_of_int" "Stdlib.html";;
     Looking for signature for VALfloat_of_int in Stdlib.html
     Signature=[int -> float]
     - : (string * string) option =
     Some ("<code class=\\\"type\\\">int -&gt; float</code>", "int -> float")
  *)
  let get_sig ?mod_name ~id_name config file  =
    dbg "Looking for signature for %s in %s" id_name file;
    let soup = parse_file (config.src_dir // file) in
    (* Now we jump to the html element with id=id_name. Warning, we cannot use
       the CSS "#id" syntax for searching the id -- like in: soup $ ("#" ^ id)
       -- because it can have problematic chars like id="VAL( * )" *)
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
          Re.Str.global_replace anon_t_regexp (mod_name ^ ".t") sig_txt in
    dbg "Signature=[%s]" sig_txt;
    Some {html = to_string code |> String.escaped; txt = sig_txt}

  (* Example: "Buffer.html#VALadd_subbytes" ==> Some "VALadd_subbytes" *)
  let get_id ref =
    match String.split_on_char '#' ref with
    | [file; id] -> Some (file, id)
    | _ -> dbg "Could not find id for %s" ref; None

  let make ?(with_sig = true) config =
    let soup = parse_file (config.src_dir // "index_values.html") in
    soup $ "table"
    |> select "tr"
    |> fold (fun index_list tr ->
        let td_list = tr $$ "td" |> to_list in
        match td_list with
        (* We scan the row; it should contain 2 <td> entries, except for
              separators with initials A,B,C,D; etc. *)
        | [td_val; td_info] ->
            let mdule, value  = match td_val $$ ">a" |> to_list with
              | [a_val; a_mod] ->
                  { txt = R.leaf_text a_mod; html = R.attribute "href" a_mod },
                  { txt = R.leaf_text a_val; html = R.attribute "href" a_val }
              | _ -> failwith "Cannot parse value" in
            let info = match td_info $? "div.info" with
              | Some info -> { html = to_string info
                                      |> one_line
                                      |> String.escaped;
                               txt = texts info
                                     |> String.concat ""
                                     |> one_line
                                     |> String.escaped }
              | None -> { html = ""; txt = ""} in
            let signature =
              if with_sig then
                get_id value.html
                |> flat_option (fun (file,id_name) ->
                    assert (file = mdule.html);
                    get_sig config ~mod_name:mdule.txt ~id_name file)
              else None in
            { mdule; value; info; signature } :: index_list
        | _ ->
            dbg "Ignoring row:";
            dbg "%s" (List.map to_string td_list |> String.concat " ");
            index_list)  []

  let save file index =
    let outch = open_out file in
    output_string outch "var GENERAL_INDEX = [\n";
    List.iter (fun item ->
        fprintf outch {|["%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s"],|}
          item.mdule.txt item.mdule.html item.value.txt item.value.html
          item.info.html item.info.txt
          (Option.map (fun i -> i.html) item.signature |> string_of_opt)
          (Option.map (fun i -> i.txt) item.signature |> string_of_opt);
        output_string outch "\n") index;
    output_string outch "]\n";
    close_out outch

  let process config =
    print_endline "Creating index file, please wait...";
    let t = Unix.gettimeofday () in
    let index = make config in
    dbg "Index created. Time = %f\n" (Unix.gettimeofday () -. t);
    save (config.dst_dir // "index.js") index;
    dbg "Index saved. Time = %f\n" (Unix.gettimeofday () -. t)

end (* of Index module *)

let process_html config overwrite version =
  print_endline (sprintf "\nProcessing version %s into %s...\n" version config.dst_dir);
  let processed = ref 0 in
  all_html_files config
  |> List.iter (fun file ->
      match process config ~overwrite ~version
              (config.src_dir // file)
              (config.dst_dir // file) with
      | Ok () -> incr processed
      | Error s -> dbg "%s" s
    );
  sprintf "Version %s, HTML processing done: %u files have been processed."
    version !processed |> print_endline

let copy_files config =
  let ind = config.dst_dir // "index.js" in
  if not (Sys.file_exists ind) then Index.process config

(******************************************************************************)

let () =
  let version = find_version () in
  let args = Sys.argv |> Array.to_list |> List.tl in
  let config = if List.mem "compiler" args
    then { src_dir = html_maindir // "compilerlibref";
           dst_dir = api_dir // "compilerlibref"; title = "Compiler "}
    else { src_dir = html_maindir // "libref";
           dst_dir = api_dir; title = ""} in
  let overwrite = List.mem "overwrite" args in
  let makeindex = List.mem "makeindex" args in
  let makehtml = List.mem "html" args || not makeindex in
  if makehtml then process_html config overwrite version;
  if makeindex then Index.process config;
  copy_files config;
  print_endline "DONE."

(*
   Local Variables:
   compile-command:"dune build"
   End:
*)
