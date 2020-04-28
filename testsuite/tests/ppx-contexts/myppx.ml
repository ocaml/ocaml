(* A simple PPX *)

open Ast_mapper

let () =
  let quote_strings li =
    List.map (Printf.sprintf "%S") li |> String.concat " " in
  let quote_option = function
    | None -> "None"
    | Some s -> Printf.sprintf "Some(%S)" s in
  register "test" (fun _ ->
      Printf.eprintf "<ppx-context>\n";
      Printf.eprintf "tool_name: %S\n"
        (tool_name ());
      (*
         (* Note: we do not test include_dirs, load_path
            as they produce non-portable paths *)
      Printf.eprintf "include_dirs: [%s]\n"
        (quote_strings !Clflags.include_dirs);
      Printf.eprintf "load_path: [%s]\n"
        (quote_strings !Config.load_path);
      *)
      Printf.eprintf "open_modules: [%s]\n"
        (quote_strings !Clflags.open_modules);
      Printf.eprintf "for_package: %S\n"
        (quote_option !Clflags.for_package);
      Printf.eprintf "use_debug: %B\n"
        !Clflags.debug;
      Printf.eprintf "use_threads: %B\n"
        !Clflags.use_threads;
      Printf.eprintf "recursive_types: %B\n"
        !Clflags.recursive_types;
      Printf.eprintf "principal: %B\n"
        !Clflags.principal;
      Printf.eprintf "transparent_modules: %B\n"
        !Clflags.transparent_modules;
      Printf.eprintf "unboxed_types: %B\n"
        !Clflags.unboxed_types;
      Printf.eprintf "</ppx-context>\n";
      flush stderr;
      default_mapper);
