(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


(* The trace *)

open Trace

external current_environment: unit -> Obj.t = "caml_get_current_environment"

let tracing_function_ptr =
  get_code_pointer
    (Obj.repr (fun arg -> Trace.print_trace (current_environment()) arg))

let dir_trace ppf lid =
  match Env.find_value_by_name lid !Topcommon.toplevel_env with
  | (path, desc) -> begin
      (* Check if this is a primitive *)
      match desc.val_kind with
      | Val_prim _ ->
          Format.fprintf ppf
            "%a is an external function and cannot be traced.@."
          Printtyp.longident lid
      | _ ->
          let clos = Toploop.eval_value_path !Topcommon.toplevel_env path in
          (* Nothing to do if it's not a closure *)
          if Obj.is_block clos
          && (Obj.tag clos = Obj.closure_tag || Obj.tag clos = Obj.infix_tag)
          && (match
                Types.get_desc
                  (Ctype.expand_head !Topcommon.toplevel_env desc.val_type)
              with Tarrow _ -> true | _ -> false)
          then begin
          match is_traced clos with
          | Some opath ->
              Format.fprintf ppf "%a is already traced (under the name %a).@."
              Printtyp.path path
              Printtyp.path opath
          | None ->
              (* Instrument the old closure *)
              traced_functions :=
                { path = path;
                  closure = clos;
                  actual_code = get_code_pointer clos;
                  instrumented_fun =
                    instrument_closure
                      !Topcommon.toplevel_env lid ppf desc.val_type }
                :: !traced_functions;
              (* Redirect the code field of the closure to point
                 to the instrumentation function *)
              set_code_pointer clos tracing_function_ptr;
              Format.fprintf ppf "%a is now traced.@." Printtyp.longident lid
          end else
            Format.fprintf ppf "%a is not a function.@." Printtyp.longident lid
    end
  | exception Not_found ->
      Format.fprintf ppf "Unbound value %a.@." Printtyp.longident lid

let dir_untrace ppf lid =
  match Env.find_value_by_name lid !Topcommon.toplevel_env with
  | (path, _desc) ->
      let rec remove = function
      | [] ->
          Format.fprintf ppf "%a was not traced.@." Printtyp.longident lid;
          []
      | f :: rem ->
          if Path.same f.path path then begin
            set_code_pointer f.closure f.actual_code;
            Format.fprintf ppf "%a is no longer traced.@."
              Printtyp.longident lid;
            rem
          end else f :: remove rem in
      traced_functions := remove !traced_functions
  | exception Not_found ->
      Format.fprintf ppf "Unbound value %a.@." Printtyp.longident lid

let dir_untrace_all ppf () =
  List.iter
    (fun f ->
      set_code_pointer f.closure f.actual_code;
      Format.fprintf ppf "%a is no longer traced.@." Printtyp.path f.path)
    !traced_functions;
  traced_functions := []

let _ = Topcommon.add_directive "trace"
    (Directive_ident (dir_trace Format.std_formatter))
    {
      section = Topdirs.section_trace;
      doc = "All calls to the function \
          named function-name will be traced.";
    }

let _ = Topcommon.add_directive "untrace"
    (Directive_ident (dir_untrace Format.std_formatter))
    {
      section = Topdirs.section_trace;
      doc = "Stop tracing the given function.";
    }

let _ = Topcommon.add_directive "untrace_all"
    (Directive_none (dir_untrace_all Format.std_formatter))
    {
      section = Topdirs.section_trace;
      doc = "Stop tracing all functions traced so far.";
    }


(* --- *)


let preload_objects = ref []

(* Position of the first non expanded argument *)
let first_nonexpanded_pos = ref 0

let current = ref (!Arg.current)

let argv = ref Sys.argv

(* Test whether the option is part of a responsefile *)
let is_expanded pos = pos < !first_nonexpanded_pos

let expand_position pos len =
  if pos < !first_nonexpanded_pos then
    (* Shift the position *)
    first_nonexpanded_pos := !first_nonexpanded_pos + len
  else
    (* New last position *)
    first_nonexpanded_pos := pos + len + 2

let prepare ppf =
  Topcommon.set_paths ();
  try
    let res =
      let objects =
        List.rev (!preload_objects @ !Compenv.first_objfiles)
      in
      List.for_all (Topeval.load_file false ppf) objects
    in
    Topcommon.run_hooks Topcommon.Startup;
    res
  with x ->
    try Location.report_exception ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

let input_argument name =
  let filename = Toploop.filename_of_input name in
  let ppf = Format.err_formatter in
  if Filename.check_suffix filename ".cmo"
          || Filename.check_suffix filename ".cma"
  then preload_objects := filename :: !preload_objects
  else if is_expanded !current then begin
    (* Script files are not allowed in expand options because otherwise the
       check in override arguments may fail since the new argv can be larger
       than the original argv.
    *)
    Printf.eprintf "For implementation reasons, the toplevel does not support\
   \ having script files (here %S) inside expanded arguments passed through the\
   \ -args{,0} command-line option.\n" filename;
    raise (Compenv.Exit_with_status 2)
  end else begin
      let newargs = Array.sub !argv !current
                              (Array.length !argv - !current)
      in
      Compenv.readenv ppf Before_link;
      Compmisc.read_clflags_from_env ();
      if prepare ppf && Toploop.run_script ppf name newargs
      then raise (Compenv.Exit_with_status 0)
      else raise (Compenv.Exit_with_status 2)
    end

let file_argument x = input_argument (Toploop.File x)

let wrap_expand f s =
  let start = !current in
  let arr = f s in
  expand_position start (Array.length arr);
  arr

module Options = Main_args.Make_bytetop_options (struct
    include Main_args.Default.Topmain
    let _stdin () = input_argument Toploop.Stdin
    let _args = wrap_expand Arg.read_arg
    let _args0 = wrap_expand Arg.read_arg0
    let anonymous s = file_argument s
    let _eval s = input_argument (Toploop.String  s)
end)

let main () =
  let ppf = Format.err_formatter in
  let program = "ocaml" in
  let display_deprecated_script_alert =
    Array.length !argv >= 2 && Topcommon.is_command_like_name !argv.(1)
  in
  Topcommon.update_search_path_from_env ();
  Compenv.readenv ppf Before_args;
  if display_deprecated_script_alert then
    Location.deprecated_script_alert program;
  Clflags.add_arguments __LOC__ Options.list;
  Compenv.parse_arguments ~current argv file_argument program;
  Compenv.readenv ppf Before_link;
  Compmisc.read_clflags_from_env ();
  if not (prepare ppf) then raise (Compenv.Exit_with_status 2);
  Compmisc.init_path ();
  Toploop.loop Format.std_formatter

let main () =
  match main () with
  | exception Compenv.Exit_with_status n -> n
  | () -> 0
