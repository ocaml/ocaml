(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)
(* find font information *)

let debug = ref false
let log s = 
  if !debug then try prerr_endline s with _ -> ()

type ('s, 'i) xlfd = {
    (* some of them are currently not interesting for me *)
    mutable foundry: 's;
    mutable family: 's;
    mutable weight: 's;
    mutable slant: 's;
    mutable setWidth: 's;
    mutable addStyle: 's;
    mutable pixelSize: 'i;
    mutable pointSize: 'i;
    mutable resolutionX: 'i;
    mutable resolutionY: 'i;
    mutable spacing: 's;
    mutable averageWidth: 'i;
    mutable registry: 's;
    mutable encoding: 's
  } 

let copy xlfd = {xlfd with foundry= xlfd.foundry}

let string_of_xlfd s i xlfd =
  let foundry= s xlfd.foundry
  and family= s xlfd.family
  and weight= s xlfd.weight
  and slant= s xlfd.slant
  and setWidth = s xlfd.setWidth
  and addStyle = s xlfd.addStyle
  and pixelSize= i xlfd.pixelSize
  and pointSize = i xlfd.pointSize
  and resolutionX = i xlfd.resolutionX
  and resolutionY = i xlfd.resolutionY
  and spacing= s xlfd.spacing
  and averageWidth = i xlfd.averageWidth
  and registry= s xlfd.registry
  and encoding = s xlfd.encoding in

  "-"^foundry^
  "-"^family^
  "-"^weight^
  "-"^slant^
  "-"^setWidth ^
  "-"^addStyle ^
  "-"^pixelSize^
  "-"^pointSize ^
  "-"^resolutionX ^
  "-"^resolutionY ^
  "-"^spacing^
  "-"^averageWidth ^
  "-"^registry^
  "-"^encoding

exception Parse_Xlfd_Failure of string

let parse_xlfd xlfd_string =
  (* this must not be a pattern *)
  let split_str char_sep str =
    let len = String.length str in
    let rec split beg cur =
      if cur >= len then [String.sub str beg (len - beg)]
      else if char_sep (String.get str cur) 
      then 
        let nextw = succ cur in
        (String.sub str beg (cur - beg))
        ::(split nextw nextw)
      else split beg (succ cur) in
    split 0 0
  in
   match split_str (function '-' -> true | _ -> false) xlfd_string with
   | [ _; foundry; family; weight; slant; setWidth; addStyle; pixelSize;
       pointSize; resolutionX; resolutionY; spacing; averageWidth;
       registry; encoding ] ->
       { foundry= foundry;
         family= family;
         weight= weight;
         slant= slant;
         setWidth= setWidth;
         addStyle= addStyle;
         pixelSize= int_of_string pixelSize;
         pointSize= int_of_string pointSize;
         resolutionX= int_of_string resolutionX;
         resolutionY= int_of_string resolutionY;
         spacing= spacing;
         averageWidth= int_of_string averageWidth;
         registry= registry;
         encoding= encoding;
       } 
   | _ -> raise (Parse_Xlfd_Failure xlfd_string)

type valid_xlfd = (string, int) xlfd

let string_of_valid_xlfd = string_of_xlfd (fun x -> x) string_of_int

type pattern = (string option, int option) xlfd

let empty_pattern =
  { foundry= None;
    family= None;
    weight= None;
    slant= None;
    setWidth= None;
    addStyle= None;
    pixelSize= None;
    pointSize= None;
    resolutionX= None;
    resolutionY= None;
    spacing= None;
    averageWidth= None;
    registry= None;
    encoding= None;
  } 

let string_of_pattern =
  let pat f = function
      Some x -> f x
    | None -> "*"
  in
  let pat_string = pat (fun x -> x) in
  let pat_int = pat string_of_int in
  string_of_xlfd pat_string pat_int

let is_vector_font xlfd =
  (xlfd.pixelSize = 0 && xlfd.resolutionX = 0 && xlfd.resolutionY = 0) ||
  xlfd.spacing <> "c"

let list_fonts dispname pattern =
  let dispopt = match dispname with
    None -> ""
  | Some x -> "-display " ^ x
  in
  let result = List.map parse_xlfd 
      (Shell.subshell ("xlsfonts "^dispopt^" -fn "^string_of_pattern pattern)) 
  in
  if result = [] then raise Not_found 
  else result

let available_pixel_size_aux dispname pattern =
  (* return available pixel size without font resizing *)
  (* to obtain good result, *)
  (* the pattern should contain as many information as possible *)
  let pattern = copy pattern in
  pattern.pixelSize <- None;
  let xlfds = list_fonts dispname pattern in
  let pxszs = Hashtbl.create 107 in
  List.iter (fun xlfd -> 
    Hashtbl.add pxszs xlfd.pixelSize xlfd) xlfds;
  pxszs

let extract_size_font_hash tbl =
  let keys = ref [] in
  Hashtbl.iter (fun k _ -> 
    if not (List.mem k !keys) then keys := k :: !keys) tbl;
  Sort.list (fun (k1,_) (k2,_) -> k1 < k2) 
    (List.map (fun k -> k, Hashtbl.find_all tbl k) !keys)

let available_pixel_size dispname pattern =
  let pxszs = available_pixel_size_aux dispname pattern in
  extract_size_font_hash pxszs

let nearest_pixel_size dispname vector_ok pattern =
  (* find the font with the nearest pixel size *)
  log ("\n*** "^string_of_pattern pattern);  
  let pxlsz = 
    match pattern.pixelSize with
      None -> raise (Failure "invalid pixelSize pattern")
    | Some x -> x
  in
  let tbl = available_pixel_size_aux dispname pattern in
  let newtbl = Hashtbl.create 107 in
  Hashtbl.iter (fun s xlfd ->
    if vector_ok then
      if s = 0 then begin
        if is_vector_font xlfd then begin
          log (Printf.sprintf "%s is vector" (string_of_valid_xlfd xlfd));
          xlfd.pixelSize <- pxlsz;
          Hashtbl.add newtbl pxlsz xlfd
        end
      end else Hashtbl.add newtbl s xlfd
    else if not (is_vector_font xlfd) && s <> 0 then
      Hashtbl.add newtbl s xlfd) tbl;
  
  let size_font_table = extract_size_font_hash newtbl in

  let diff = ref 10000 in
  let min = ref None in
  List.iter (fun (s,xlfds) ->
    let d = abs(s - pxlsz) in
    if d < !diff then begin 
      min := Some (s,xlfds); 
      diff := d 
    end) size_font_table;
  (* if it contains more than one font, just return the first *)
  match !min with
  | None -> raise Not_found
  | Some(s, xlfds) ->
     log (Printf.sprintf "Size %d is selected" s);
     List.iter (fun xlfd -> log (string_of_valid_xlfd xlfd)) xlfds;
     List.hd xlfds
