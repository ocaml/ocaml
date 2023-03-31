(* TEST
   ocamllex_flags = " -q "
*)

(* line directive + cr/lf *)
# 1 "line_directive.mll.in"

(* line directive + cr + cr/lf *)
# 2 "line_directive.mll.in"

rule token = parse
  | _ { }
