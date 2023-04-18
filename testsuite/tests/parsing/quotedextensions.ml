(* TEST_BELOW
Lorem_ipsum_dolor_
sit_amet,consectetur_adipiscing_
elit.Sed_non_risus.Lorem_ipsum_d
olor_sit_amet,con
sectetur_adipiscing_elit.Sed_no
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

(* Double quotes inside quoted strings inside comments *)
(* {|"|}, and *)
(* [%foo {|"|}], and *)
(* {%foo|"|} should be valid inside comments *)

(* Comment delimiters inside quoted strings inside comments: *)
(* {|*)|}, and *)
(* [%foo {bar|*)|bar}], and *)
(* {%foo bar|*)|bar} should be valid inside comments *)

(* TEST
{
  flags = "-dparsetree";
  ocamlc_byte_exit_status = "2";
  setup-ocamlc.byte-build-env;

  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
