open Ocamlbuild_plugin;;
open Command;;

let cc = A"cc";;
let ar = A"ar";;

dispatch begin function
| After_rules ->
    let libasmrun = !*Ocamlbuild_pack.Ocaml_utils.stdlib_dir/"libasmrun.a" in

    flag ["ocaml"; "link"; "output_obj"] (A"-output-obj");

    rule "output C obj"
      ~deps:["%.cmx"; "%.o"]
      ~prod:"%caml.o"
      (Ocamlbuild_pack.Ocaml_compiler.native_link "%.cmx" "%caml.o");

    rule "build C lib"
      ~deps:["%wrap.o"; "%caml.o"]
      ~prod:"lib%.a"
      begin fun env _ ->
        let wrap_o = env "%wrap.o" and caml_o = env "%caml.o"
        and lib_a = env "lib%.a" in
        Seq[cp libasmrun lib_a;
            Cmd(S[ar; A"r"; Px lib_a; P caml_o; P wrap_o])]
      end;
    rule "build main"
      ~deps:["libfib.a"; "main.o"]
      ~prod:"main"
      begin fun _ _ ->
        Cmd(S[cc; P"main.o"; P"libfib.a"; A"-o"; Px"main"])
      end;
| _ -> ()
end

