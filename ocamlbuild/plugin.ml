(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open My_std
open Format
open Log
open Pathname.Operators
open Tags.Operators
open Rule
open Tools
open Command
;;

module Make(U:sig end) =
  struct
    let plugin                = "myocamlbuild"
    let plugin_file           = plugin^".ml"
    let plugin_config_file    = plugin^"_config.ml"
    let plugin_config_file_interface = plugin^"_config.mli"

    let we_have_a_config_file = sys_file_exists plugin_config_file
    let we_need_a_plugin      = !Options.plugin && sys_file_exists plugin_file
    let we_have_a_plugin      = sys_file_exists ((!Options.build_dir/plugin)^(!Options.exe))
    let we_have_a_config_file_interface = sys_file_exists plugin_config_file_interface

    let up_to_date_or_copy fn =
      let fn' = !Options.build_dir/fn in
      Pathname.exists fn &&
        begin
          Pathname.exists fn' && Pathname.same_contents fn fn' ||
          begin
            Shell.cp fn fn';
            false
          end
        end

    let profiling = Tags.mem "profile" (tags_of_pathname plugin_file)

    let debugging = Tags.mem "debug" (tags_of_pathname plugin_file)

    let rebuild_plugin_if_needed () =
      let a = up_to_date_or_copy plugin_file in
      let b = (not we_have_a_config_file) or up_to_date_or_copy plugin_config_file in
      let c = (not we_have_a_config_file_interface) or up_to_date_or_copy plugin_config_file_interface in
      if a && b && c && we_have_a_plugin then
        () (* Up to date *)
           (* FIXME: remove ocamlbuild_config.ml in _build/ if removed in parent *)
      else begin
        if !Options.native_plugin
            && not (sys_file_exists ((!Ocamlbuild_where.libdir)/"ocamlbuildlib.cmxa")) then
          begin
            Options.native_plugin := false;
            eprintf "Warning: Won't be able to compile a native plugin"
          end;
        let plugin_config =
          if we_have_a_config_file then
            if we_have_a_config_file_interface then
              S[P plugin_config_file_interface; P plugin_config_file]
            else P plugin_config_file
          else N in
        let cma, cmo, more_options, compiler =
          if !Options.native_plugin then
            "cmxa", "cmx", (if profiling then A"-p" else N), !Options.ocamlopt
          else
            "cma", "cmo", (if debugging then A"-g" else N), !Options.ocamlc
        in
        let ocamlbuildlib, ocamlbuild, libs =
          if (not !Options.native_plugin) && !*My_unix.is_degraded then
            "ocamlbuildlightlib", "ocamlbuildlight", N
          else
            "ocamlbuildlib", "ocamlbuild", A("unix"-.-cma)
        in
        let ocamlbuildlib = ocamlbuildlib-.-cma in
        let ocamlbuild = ocamlbuild-.-cmo in
        let dir = !Ocamlbuild_where.libdir in
        if not (sys_file_exists (dir/ocamlbuildlib)) then 
          failwith (sprintf "Cannot find %S in ocamlbuild -where directory" ocamlbuildlib);
        let dir = if Pathname.is_implicit dir then Pathname.pwd/dir else dir in
        let cmd =
          Cmd(S[compiler; A"-I"; P dir; libs; more_options;
                P(dir/ocamlbuildlib); plugin_config; P plugin_file;
                P(dir/ocamlbuild); A"-o"; Px (plugin^(!Options.exe))])
        in
        Shell.chdir !Options.build_dir;
        Shell.rm_f (plugin^(!Options.exe));
        Command.execute cmd
      end

    let execute_plugin_if_needed () =
      if we_need_a_plugin then
        begin
          rebuild_plugin_if_needed ();
          Shell.chdir Pathname.pwd;
          if not !Options.just_plugin then
            let runner = if !Options.native_plugin then N else !Options.ocamlrun in
            let spec = S[runner; P(!Options.build_dir/plugin^(!Options.exe));
                         A"-no-plugin"; atomize (List.tl (Array.to_list Sys.argv))] in
            let () = Log.finish () in
            raise (Exit_silently_with_code (sys_command (Command.string_of_command_spec spec)))
        end
      else
        ()
  end
;;

let execute_plugin_if_needed () =
  let module P = Make(struct end) in
  P.execute_plugin_if_needed ()
;;
