open Ocamlbuild_plugin;;
let version = "0.1";;
dispatch begin function
  | After_rules ->
      file_rule "myconfig.ml"
        ~prod:"myconfig.ml"
        ~cache:(fun _ _ -> version)
        begin fun _ oc ->
          Printf.fprintf oc "let version = %S;;\n%!" version
        end;

      copy_rule "copy byte-code executables" "%(path).byte" "%(path:not <**/*.*>)";
      copy_rule "copy native executables" "%(path).native" "%(path:not <**/*.*>).opt";
      copy_rule "copy binaries to bin" "%(basename).%(extension)"
                                       "bin/%(basename).%(extension:<{byte,native}>)";
  | _ -> ()
end
