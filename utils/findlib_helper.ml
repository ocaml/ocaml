open Clflags
open Findlib

let check_package pkg =
  (* may raise No_such_package *)
  ignore (Findlib.package_directory pkg : string)

let package_exists pkg =
  match package_directory pkg with
  | _ -> true
  | exception No_such_package _ -> false

let get_packages () =
  let pkgs = !packages in
  List.iter check_package pkgs;
  package_deep_ancestors !predicates pkgs

let process_package_includes () =
  let pkgs = get_packages () in
  let pkgs_dirs = Misc.remove_dups (List.map package_directory pkgs) in
  let stdlibdir = Fl_split.norm_dir Config.standard_library in
  let threads_dir = Filename.concat stdlibdir "threads" in
  let vmthreads_dir = Filename.concat stdlibdir "vmthreads" in
  let exclude_list = [ stdlibdir; threads_dir; vmthreads_dir ] in
  let i_options =
    Misc.Stdlib.List.filter_map
      (fun pkgdir ->
         let npkgdir = Fl_split.norm_dir pkgdir in
         if List.mem npkgdir exclude_list then
           None
         else
           Some (Misc.slashify pkgdir)
      ) pkgs_dirs
  in
  let dll_options = List.map Misc.slashify pkgs_dirs in
  include_dirs := !include_dirs @ i_options;
  dllpaths := !dllpaths @ dll_options

let process_ppx_spec () =
  (* Returns: ppx_commands *)
  (* may raise No_such_package *)
  let ppx_packages = get_packages () in
  let ppx_opts = [] in
  let meta_ppx_opts =
    List.concat
      (List.map
         (fun pname ->
            try
              let opts = package_property !predicates pname "ppxopt" in
              (* Split by whitespace to get (package,options) combinations.
                 Then, split by commas to get individual options. *)
              List.map
                (fun opts ->
                   match Fl_split.in_words opts with
                   | pkg :: ((_ :: _) as opts) ->
                       if not (package_exists pkg) then
                         failwith ("The package named in ppxopt variable does not exist: " ^ pkg ^ " (from " ^ pname ^ ")");
                       let base = package_directory pname in
                       pkg, List.map (resolve_path ~base ~explicit:true) opts
                   | _ ->
                       failwith ("ppxopt variable must include package name, e.g. " ^
                                 "ppxopt=\"foo,-name bar\" (from " ^ pname ^ ")")
                )
                (Fl_split.in_words_ws opts)
            with Not_found -> []
         )
         ppx_packages
      )
  in
  Misc.Stdlib.List.filter_map
    (fun pname ->
       let base = package_directory pname in
       let options =
         try
           List.concat
             (List.map (fun (_, opts) -> opts)
                (List.filter (fun (pname', _) -> pname' = pname)
                   (meta_ppx_opts @ ppx_opts)))
         with Not_found ->
           []
       in
       try
         let preprocessor =
           resolve_path ~base ~explicit:true (package_property !predicates pname "ppx")
         in
         Some (String.concat " " (preprocessor :: options))
       with Not_found ->
         None
    ) ppx_packages

let get_archives () =
  let pkgs = get_packages () in
  List.flatten
    (List.map
       (fun pkg ->
          let al =
            try package_property !predicates pkg "archive"
            with Not_found -> ""
          in
          let pkg_dir = Misc.slashify (package_directory pkg) in
          List.map
            (fun arch -> resolve_path ~base:pkg_dir arch)
            (Fl_split.in_words al)
       ) pkgs
    )

let init () =
  let install_dir = Filename.dirname Config.standard_library in
  let meta_dir = "none" in
  let search_path = [install_dir] in
  init_manually ~stdlib:Config.standard_library ~install_dir ~meta_dir ~search_path ()
