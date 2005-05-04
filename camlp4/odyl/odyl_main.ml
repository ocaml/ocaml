(* camlp4r pa_macro.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

value go = ref (fun () -> ());
value name = ref "odyl";

value first_arg_no_load () =
  loop 1 where rec loop i =
    if i < Array.length Sys.argv then
      match Sys.argv.(i) with
      [ "-I" -> loop (i + 2)
      | ("-nolib" | "-where" | "-version") -> loop (i + 1)
      | "--" -> i + 1
      | s ->
          if Filename.check_suffix s ".cmo"
          || Filename.check_suffix s ".cma"
          then loop (i + 1)
          else i ]
    else i
;

Arg.current.val := first_arg_no_load () - 1;

(* Load files in core *)

value find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else
    let rec try_dir =
      fun
      [ [] -> raise Not_found
      | [dir :: rem] ->
          let fullname = Filename.concat dir name in
          if Sys.file_exists fullname then fullname else try_dir rem ]
    in
    try_dir path
;

exception Error of string and string;

value nolib = ref False;
value initialized = ref False;
value path = ref ([] : list string);

value loadfile file =
  IFDEF OPT THEN
    raise (Error file "native-code program cannot do a dynamic load")
  ELSE do {
    if not initialized.val then do {
      IFDEF OPT THEN ()
      ELSE do { Dynlink.init (); Dynlink.allow_unsafe_modules True }
      END;
      initialized.val := True
    }
    else ();
    let path =
      if nolib.val then path.val
      else [Odyl_config.standard_library :: path.val]
    in
    let fname =
      try find_in_path (List.rev path) file with
      [ Not_found -> raise (Error file "file not found in path") ]
    in
    try Dynlink.loadfile fname with
    [ Dynlink.Error e -> raise (Error fname (Dynlink.error_message e)) ]
  }
  END
;

value directory d = path.val := [d :: path.val];
