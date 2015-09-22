(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

open Ocamlbuild_plugin;;
let version = "0.1";;
dispatch begin function
  | After_rules ->
      rule "myconfig.ml"
        ~prod:"myconfig.ml"
        begin fun _ _ ->
          Echo(["let version = \""; version; "\";;\n"], "myconfig.ml")
        end;

      copy_rule "copy byte-code executables" "%(path).byte" "%(path:not <**/*.*>)";
      copy_rule "copy native executables" "%(path).native" "%(path:not <**/*.*>).opt";
      copy_rule "copy binaries to bin" "%(basename).%(extension)"
                                       "bin/%(basename).%(extension:<{byte,native}>)";
  | _ -> ()
end
