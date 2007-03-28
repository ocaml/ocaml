open Ocamlbuild_plugin;;
open Command;;

let cc = A"cc";;
let ar = A"ar";;

dispatch begin function
| After_rules ->
    let libasmrun = !*Ocamlbuild_pack.Ocaml_utils.stdlib_dir/"libasmrun.a" in

    rule "output C obj"
      ~dep:"%.ml"
      ~prod:"%caml.o"
      begin fun env _ ->
        let caml_o = env "%caml.o" and ml = env "%.ml" in
        Cmd(S[!Options.ocamlopt; A"-output-obj"; P ml; A"-o"; Px caml_o])
      end;
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

