(* TEST_BELOW
Filler_text_added_
to_preserve_locations_while_tran
slating_from_old_syntax__Filler_
text_added_to_pre
serve_locations_while_translati
*)

(* Structures *)
{%%M.foo| <hello>{x} |}
{%%M.foo  | <hello>{x} |}
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

let {%M.foo | <hello>{u|x|u} |}
  : {%M.foo | <hello>{u|x|u} |}
  = {%M.foo | <hello>{u|x|u} |}


(* Multiline *)
{%%M.foo|
 <hello>
   {x}
 </hello>
|}

(* Double quotes inside quoted strings inside comments *)
(* {|"|}, and *)
(* [%foo {|"|}], and *)
(* {%foo|"|} should be valid inside comments *)

(* Comment delimiters inside quoted strings inside comments: *)
(* {|*)|}, and *)
(* [%foo {bar|*)|bar}], and *)
(* {%foo bar|*)|bar} should be valid inside comments *)

(* TEST
 flags = "-dparsetree";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
