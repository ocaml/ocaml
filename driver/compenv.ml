(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*      Fabrice Le Fessant, équipe Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Clflags

let output_prefix name =
  let oname =
    match !output_name with
    | None -> name
    | Some n -> if !compile_only then (output_name := None; n) else name in
  Misc.chop_extension_if_any oname

let print_version_and_library compiler =
  Printf.printf "The OCaml %s, version " compiler;
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let print_version_string () =
  print_string Config.version; print_newline(); exit 0

let print_standard_library () =
  print_string Config.standard_library; print_newline(); exit 0

let fatal err =
  prerr_endline err;
  exit 2

let extract_output = function
  | Some s -> s
  | None ->
      fatal "Please specify the name of the output file, using option -o"

let default_output = function
  | Some s -> s
  | None -> Config.default_executable_name

(* Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory (unless the -nostdlib option is given).
 *)

let implicit_modules = ref []
let first_include_dirs = ref []
let last_include_dirs = ref []
let first_ccopts = ref []
let last_ccopts = ref []
let first_ppx = ref []
let last_ppx = ref []

(* Note: this function is duplicated in optcompile.ml *)
let check_unit_name ppf filename name =
  try
    begin match name.[0] with
    | 'A'..'Z' -> ()
    | _ ->
       Location.print_warning (Location.in_file filename) ppf
        (Warnings.Bad_module_name name);
       raise Exit;
    end;
    for i = 1 to String.length name - 1 do
      match name.[i] with
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> ()
      | _ ->
         Location.print_warning (Location.in_file filename) ppf
           (Warnings.Bad_module_name name);
         raise Exit;
    done;
  with Exit -> ()
;;







type readenv_position =
  Before_args | Before_compile | Before_link

(* Syntax of OCAMLCOMPPARAM: (name=VALUE)(,name=VALUE)* where
   VALUE=expression without ,
*)
exception SyntaxError of string

(*
let parse_args s =
  let len = String.length s in
  let rec iter0 i pos0 =
    if i = len then
      if i = pos0 then []
      else raise (SyntaxError "End of line while expecting char '='")
    else
    let c = s.[i] in
    let pos1 = i+1 in
    if c = '=' then
      iter1 pos1 pos1 (String.sub s pos0 (i-pos0))
    else iter0 pos1 pos0

  and iter1 i pos0 name =
    if i = len then [name, ""]
    else
      let c = s.[i] in
      let pos1 = i+1 in
      match c with
        '"' ->
        iter3 pos1 (Buffer.create 50) name
      | ',' ->
        (name, "") :: iter0 pos1 pos1
      | _ ->
        iter2 pos1 pos0 name

  and iter2 i pos0 name =
    if i = len then [name, String.sub s pos0 (len-pos0)]
    else
      let pos1 = i+1 in
      match s.[i] with
      | ',' ->
        (name, String.sub s pos0 (i-pos0)) :: iter0 pos1 pos1
      | _ -> iter2 pos1 pos0 name

  and iter3 i b name =
    if i = len then
      raise (SyntaxError "End of line while expecting '\"'")
    else
      let pos1 = i+1 in
      match s.[i] with
      | '"' ->
        if pos1 = len then
          [name, Buffer.contents b]
        else begin
          let pos2 = pos1+1 in
          match s.[pos1] with
          | '"' ->
            Buffer.add_char b '"';
            iter3 pos2 b name
          | ',' ->
            (name, Buffer.contents b) :: iter0 pos2 pos2
          | _ ->
            raise (SyntaxError "Syntax error while expecting ',' after '\"'")
        end
      | c ->
        Buffer.add_char b c;
        iter3 pos1 b name

  in
  iter0 0 0
*)

let parse_args s =
  let args = Misc.split s ',' in
  let rec iter is_after args before after =
    match args with
      [] ->
      if not is_after then
        raise (SyntaxError "no '_' separator found")
      else
      (List.rev before, List.rev after)
    | "_" :: _ when is_after -> raise (SyntaxError "too many '_' separators")
    | "_" :: tail -> iter true tail before after
    | arg :: tail ->
      let binding = try
        Misc.cut_at arg '='
      with Not_found ->
        raise (SyntaxError ("missing '=' in " ^ arg))
      in
      if is_after then
        iter is_after tail before (binding :: after)
      else
        iter is_after tail (binding :: before) after
  in
  iter false args [] []

let setter f name options s =
  try
    let bool = match s with
      | "0" -> false
      | "1" -> true
      | _ -> raise Not_found
    in
    List.iter (fun b -> b := f bool) options
  with Not_found ->
    Printf.eprintf "Warning: bad value for %S in OCAMLPARAM\n%!" name

let set name options s =
  setter (fun b -> b) name options s

let clear name options s =
  setter (fun b -> not b) name options s


let read_OCAMLPARAM position =
  try
    let s = Sys.getenv "OCAMLPARAM" in
    let (before, after) =
      try
        parse_args s
      with SyntaxError s ->
        fatal (Printf.sprintf "Illegal syntax of OCAMLPARAM: %s" s)
    in
    List.iter (fun (name, v) ->
      match name with

      | "g" -> set "g" [ Clflags.debug ] v
      | "p" -> set "p" [ Clflags.gprofile ] v
      | "bin-annot" -> set "bin-annot" [ Clflags.binary_annotations ] v
      | "annot" -> set "annot" [ Clflags.annotations ] v
      | "absname" -> set "absname" [ Location.absname ] v
      | "compat-32" -> set "compat-32" [ bytecode_compatible_32 ] v
      | "noassert" -> set "noassert" [ noassert ] v
      | "noautolink" -> set "noautolink" [ no_auto_link ] v
      | "nostdlib" -> set "nostdlib" [ no_std_include ] v
      | "linkall" -> set "linkall" [ link_everything ] v
      | "nolabels" -> set "nolabels" [ classic ] v
      | "principal" -> set "principal"  [ principal ] v
      | "rectypes" -> set "rectypes" [ recursive_types ] v
      | "strict-sequence" -> set "strict-sequence" [ strict_sequence ] v
      | "thread" -> set "thread" [ use_threads ] v
      | "unsafe" -> set "unsafe" [ fast ] v
      | "verbose" -> set "verbose" [ verbose ] v
      | "nopervasives" -> set "nopervasives" [ nopervasives ] v
      | "slash" -> set "slash" [ force_slash ] v (* for ocamldep *)

      | "compact" -> clear "compact" [ optimize_for_speed ] v
      | "no-app-funct" -> clear "no-app-funct" [ applicative_functors ] v
      | "nodynlink" -> clear "nodynlink" [ dlcode ] v
      | "short-paths" -> clear "short-paths" [ real_paths ] v

      | "pp" -> preprocessor := Some v
      | "runtime-variant" -> runtime_variant := v
      | "open" -> implicit_modules := Misc.split v ','
      | "cc" -> c_compiler := Some v

      (* assembly sources *)
      |  "s" ->
        set "s" [ Clflags.keep_asm_file ; Clflags.keep_startup_file ] v
      |  "S" -> set "S" [ Clflags.keep_asm_file ] v
      |  "dstartup" -> set "dstartup" [ Clflags.keep_startup_file ] v

      (* warn-errors *)
      | "we" | "warn-error" -> Warnings.parse_options true v
      (* warnings *)
      |  "w"  ->               Warnings.parse_options false v
      (* warn-errors *)
      | "wwe" ->               Warnings.parse_options false v

      (* inlining *)
      | "inline" -> begin try
          inline_threshold := 8 * int_of_string v
        with _ ->
          Printf.eprintf
            "Warning: discarding non integer value of inline from OCAMLCOMPPARAM\n%!"
        end

      | "intf-suffix" -> Config.interface_suffix := v

      | "I" -> begin
          match position with
          | Before_args -> first_include_dirs := v :: !first_include_dirs
          | Before_link | Before_compile ->
            last_include_dirs := v :: !last_include_dirs
        end

      | "cclib" ->
        begin
          match position with
          | Before_compile -> ()
          | Before_link | Before_args ->
            ccobjs := Misc.rev_split_words v @ !ccobjs
        end

      | "ccopts" ->
        begin
          match position with
          | Before_link | Before_compile ->
            last_ccopts := v :: !last_ccopts
          | Before_args ->
            first_ccopts := v :: !first_ccopts
        end

      | "ppx" ->
        begin
          match position with
          | Before_link | Before_compile ->
            last_ppx := v :: !last_ppx
          | Before_args ->
            first_ppx := v :: !first_ppx
        end

      | _ ->
        Printf.eprintf
            "Warning: discarding value of variable %S in OCAMLCOMPPARAM\n%!"
            name
    ) (match position with
        Before_args -> before
      | Before_compile | Before_link -> after)
  with Not_found -> ()

let readenv position =
  last_include_dirs := [];
  last_ccopts := [];
  last_ppx := [];
  read_OCAMLPARAM position;
  all_ccopts := !last_ccopts @ !first_ccopts;
  all_ppx := !last_ppx @ !first_ppx

