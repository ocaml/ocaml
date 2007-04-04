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

(* $Id$ *)
(* Original author: Nicolas Pouillard *)
open My_std
open Pathname.Operators
open Tags.Operators
open Tools
open Command
open Ocaml_utils

let ocamlyacc mly env _build =
  let mly = env mly in
  let ocamlyacc = if !Options.ocamlyacc = N then V"OCAMLYACC" else !Options.ocamlyacc in
  Cmd(S[ocamlyacc; T(tags_of_pathname mly++"ocaml"++"parser"++"ocamlyacc");
        flags_of_pathname mly; Px mly])

let ocamllex mll env _build =
  let mll = env mll in
  Cmd(S[!Options.ocamllex; T(tags_of_pathname mll++"ocaml"++"lexer"++"ocamllex");
        flags_of_pathname mll; Px mll])

let infer_interface ml mli env build =
  let ml = env ml and mli = env mli in
  let tags = tags_of_pathname ml++"ocaml" in
  Ocaml_compiler.prepare_compile build ml;
  Cmd(S[!Options.ocamlc; ocaml_ppflags tags; ocaml_include_flags ml; A"-i";
        T(tags++"infer_interface"); P ml; Sh">"; Px mli])

let menhir mly env build =
  let mly = env mly in
  let menhir = if !Options.ocamlyacc = N then V"MENHIR" else !Options.ocamlyacc in
  Ocaml_compiler.prepare_compile build mly;
  Cmd(S[menhir;
        A"--ocamlc"; Quote(S[!Options.ocamlc; ocaml_include_flags mly]);
        T(tags_of_pathname mly++"ocaml"++"parser"++"menhir");
        A"--infer"; flags_of_pathname mly; Px mly])

let ocamldoc_c tags arg odoc =
  let tags = tags++"ocaml" in
  Cmd (S [!Options.ocamldoc; A"-dump"; Px odoc; T(tags++"doc");
          ocaml_ppflags tags; flags_of_pathname arg;
          ocaml_include_flags arg; P arg])

let ocamldoc_l_dir tags deps _docout docdir =
  Seq[Cmd (S[A"rm"; A"-rf"; Px docdir]);
      Cmd (S[A"mkdir"; A"-p"; Px docdir]);
      Cmd (S [!Options.ocamldoc;
              S(List.map (fun a -> S[A"-load"; P a]) deps);
              T(tags++"doc"++"docdir"); A"-d"; Px docdir])]

let ocamldoc_l_file tags deps docout _docdir =
  Seq[Cmd (S[A"rm"; A"-rf"; Px docout]);
      Cmd (S[A"mkdir"; A"-p"; Px (Pathname.dirname docout)]);
      Cmd (S [!Options.ocamldoc;
              S(List.map (fun a -> S[A"-load"; P a]) deps);
              T(tags++"doc"++"docfile"); A"-o"; Px docout])]

let document_ocaml_interf mli odoc env build =
  let mli = env mli and odoc = env odoc in
  Ocaml_compiler.prepare_compile build mli;
  ocamldoc_c (tags_of_pathname mli++"interf") mli odoc

let document_ocaml_project ?(ocamldoc=ocamldoc_l_file) odocl docout docdir env build =
  let odocl = env odocl and docout = env docout and docdir = env docdir in
  let contents = string_list_of_file odocl in
  let include_dirs = Pathname.include_dirs_of (Pathname.dirname odocl) in
  let to_build =
    List.map begin fun module_name ->
      expand_module include_dirs module_name ["odoc"]
    end contents in
  let module_paths = List.map Outcome.good (build to_build) in
  let tags = (Tags.union (tags_of_pathname docout) (tags_of_pathname docdir))++"ocaml" in
  ocamldoc tags module_paths docout docdir
