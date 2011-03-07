open Camlp4.PreCast ;
module G = Camlp4.PreCast.Gram ;

value a = G.Entry.mk "a" ;
value a_eoi = G.Entry.mk "a_eoi" ;

EXTEND G
a: [[ "one" -> 1 | x = a; "plus"; y = a -> x+y ]];
a_eoi: [[ x = a; `EOI -> x ]];
END ;

(* and the following function: *)

value parse_string entry s o =
try
  Printf.eprintf "Parsing %S\n%!" s;
  assert (o = Some (G.parse_string entry (Loc.mk "<string>") s))
with [ Loc.Exc_located loc exn when o <> None ->
begin
  print_endline (Loc.to_string loc);
  print_endline (Printexc.to_string exn);
  assert (o = None)
end
| exn when o = None -> Printf.eprintf "Fail as expected\n%!"
| exn -> begin
  Printf.eprintf "Unexpected exception: \n%!";
  print_endline (Printexc.to_string exn);
  assert (o = None)
end ] ;

(* The following is correct: *)

parse_string a_eoi "one plus one" (Some 2);

(* While all of the following inputs should be rejected because they are not *)
(* legal according to the grammar: *)

parse_string a_eoi "one plus" None;
(* - : int = 1 *)
parse_string a_eoi "one plus plus" None;
(* - : int = 1 *)
parse_string a_eoi "one plus one plus" None;
(* - : int = 2 *)
parse_string a_eoi "one plus one plus plus" None;
(* - : int = 2 *)

(* Curiously, you may only repeat the operator twice. If you specify it three
times, gramlib complains.                                                     *)

parse_string a_eoi "one plus plus plus" None ;
(* File "<string>", line 1, characters 9-13 *)
(* Stream.Error("EOI expected after [a] (in [a_eoi])") *)
(* Exception: Failure "Syntax Error". *)
parse_string a_eoi "one plus one plus plus plus" None ;
(* File "<string>", line 1, characters 18-22 *)
(* Stream.Error("EOI expected after [a] (in [a_eoi])") *)
(* Exception: Failure "Syntax Error". *)
