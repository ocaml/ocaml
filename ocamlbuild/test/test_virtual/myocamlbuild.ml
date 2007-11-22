open Ocamlbuild_plugin;;
dispatch begin function
  | After_rules ->
      rule "copy foo"
        ~prod:"bar"
        ~dep:"foo.otarget"
        begin fun _env _build ->
          cp "foo" "bar"
        end
  | _ -> ()
end
