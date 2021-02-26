(* TEST
   flags = "-dparsetree"
   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* Structures *)
{%%M.foo| <hello>{x} |}
{%%M.foo bar| <hello>{|x|} |bar}

(* Signatures *)
module type S = sig
  {%%M.foo| <hello>{x} |}
  {%%M.foo bar| <hello>{|x|} |bar}
end

(* Expressions/Pattern/Types *)
let {%M.foo| <hello>{x} |}
  : {%M.foo| <hello>{x} |}
  = {%M.foo| <hello>{x} |}
let {%M.foo bar| <hello>{|x|} |bar}
  : {%M.foo bar| <hello>{|x|} |bar}
  = {%M.foo bar| <hello>{|x|} |bar}

(* Multiline *)
{%%M.foo|
 <hello>
   {x}
 </hello>
|}
