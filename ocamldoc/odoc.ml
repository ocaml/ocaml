(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Main module for bytecode. *)

open Config
open Clflags
open Misc
open Format
open Typedtree

module M = Odoc_messages

let print_DEBUG s = print_string s ; print_newline () 

(* we check if we must load a module given on the command line *)
let arg_list = Array.to_list Sys.argv
let (cmo_or_cma_opt, paths) =
  let rec iter (f_opt, inc) = function
      [] | _ :: [] -> (f_opt, inc)
    | "-g" :: file :: q when
        ((Filename.check_suffix file "cmo") or
         (Filename.check_suffix file "cma")) &
        (f_opt = None) ->
          iter (Some file, inc) q
    | "-i" :: dir :: q ->
        iter (f_opt, inc @ [dir]) q
    | _ :: q ->
        iter (f_opt, inc) q
  in
  iter (None, []) arg_list

let _ = print_DEBUG "Fin analyse des arguments pour le dynamic load"

(** Return the real name of the file to load, 
   searching it in the paths if it is
   a simple name and not in the current directory. *)
let get_real_filename name =
   if Filename.basename name <> name then
     name
   else
     (
      let paths = Filename.current_dir_name :: paths @ [Odoc_config.custom_generators_path] in
      try
	let d = List.find
	    (fun d -> Sys.file_exists (Filename.concat d name))
	    paths
	in
	Filename.concat d name
      with
	Not_found ->
	  failwith (M.file_not_found_in_paths paths name)
     )

let _ =
  match cmo_or_cma_opt with
    None ->
      ()
  | Some file ->
      (* initializations for dynamic loading *)
      Dynlink.init ();
      Dynlink.allow_unsafe_modules true;
      try
        let real_file = get_real_filename file in
        ignore(Dynlink.loadfile real_file)
      with
        Dynlink.Error e -> 
          prerr_endline (Odoc_messages.load_file_error file (Dynlink.error_message e)) ;
          exit 1
      | Not_found ->
          prerr_endline (Odoc_messages.load_file_error file "Not_found");
          exit 1  
      | Sys_error s
      |	Failure s ->
          prerr_endline (Odoc_messages.load_file_error file s);
          exit 1  

let _ = print_DEBUG "Fin du chargement dynamique éventuel"

let default_html_generator = new Odoc_html.html
let default_latex_generator = new Odoc_latex.latex
let default_texi_generator = new Odoc_texi.texi
let default_man_generator = new Odoc_man.man
let default_dot_generator = new Odoc_dot.dot
let _ = Odoc_args.parse
    (default_html_generator :> Odoc_args.doc_generator)
    (default_latex_generator :> Odoc_args.doc_generator)
    (default_texi_generator :> Odoc_args.doc_generator)
    (default_man_generator :> Odoc_args.doc_generator)
    (default_dot_generator :> Odoc_args.doc_generator)


let loaded_modules =
  List.flatten 
    (List.map 
       (fun f ->
         Odoc_info.verbose (Odoc_messages.loading f);
         try 
           let l = Odoc_analyse.load_modules f in
           Odoc_info.verbose Odoc_messages.ok;
           l
         with Failure s -> 
           prerr_endline s ; 
           incr Odoc_global.errors ;
           []
       )
       !Odoc_args.load
    )

let modules = Odoc_analyse.analyse_files ~init: loaded_modules !Odoc_args.files

let _ =
  match !Odoc_args.dump with
    None -> ()
  | Some f ->
      try Odoc_analyse.dump_modules f modules
      with Failure s -> 
        prerr_endline s ;
        incr Odoc_global.errors

let _ = 
  match !Odoc_args.doc_generator with
    None ->
      ()
  | Some gen -> 
      Odoc_info.verbose Odoc_messages.generating_doc;
      gen#generate modules;
      Odoc_info.verbose Odoc_messages.ok

let _ = 
  if !Odoc_global.errors > 0 then
  (
   prerr_endline (Odoc_messages.errors_occured !Odoc_global.errors) ;
   exit 1
  )
  else
    exit 0
  

(* eof $Id$ *)
