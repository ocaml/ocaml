open Camlp4.PreCast ;
module G = Camlp4.PreCast.Gram ;

value exp = G.Entry.mk "exp" ;
value prog = G.Entry.mk "prog" ;

EXTEND G
exp:
[ "apply"
[ e1 = SELF; e2 = exp LEVEL "simple"; e3 = SELF ->
    let p = Loc.dump in
    let () =
    Format.eprintf "e1:   %a,@.e2:   %a,@.e3:   %a,@._loc: %a@."
      p e1 p e2 p e3 p _loc
    in
    _loc
  ]
| "simple"
[ x = LIDENT; y = LIDENT ->
  let () = Format.eprintf "reduce expr simple (%S, %S) at %a@." x y Loc.dump _loc in _loc ]
];
prog: [[ e = exp; `EOI -> e ]];
END ;

(* and the following function: *)

value parse_string entry s =
try
  print_endline s;
  G.parse_string entry (Loc.mk "<string>") s
with [ Loc.Exc_located loc exn ->
begin
  print_endline (Loc.to_string loc);
  print_endline (Printexc.to_string exn);
  failwith "Syntax Error"
end ] ;

parse_string prog "f1 f2 x1 x2 y1 y2";
