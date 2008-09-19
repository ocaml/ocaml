open Camlp4.PreCast ;
module G = Camlp4.PreCast.Gram;

value ab_eoi = G.Entry.mk "ab_eoi" ;
value a_or_ab = G.Entry.mk "a_or_ab" ;
value a_or_ab_eoi = G.Entry.mk "a_or_ab_eoi" ;
value c_a_or_ab_eoi = G.Entry.mk "c_a_or_ab_eoi" ;

EXTEND G
ab_eoi: [[ "a"; "b"; `EOI -> () ]];
a_or_ab: [[ "a" -> () | "a"; "b" -> () ]];
a_or_ab_eoi: [[ a_or_ab; `EOI -> () ]];
c_a_or_ab_eoi: [[ "c"; a_or_ab; `EOI -> () ]];
END ;

value parse_string entry s =
try
  G.parse_string entry (Loc.mk "<string>") s
with [ Loc.Exc_located loc exn ->
begin
  print_endline (Loc.to_string loc);
  print_endline (Printexc.to_string exn);
  (* failwith "Syntax Error" *)
end ] ;

(* Consider the following syntax errors: *)
parse_string ab_eoi "a c" ;
(* File "<string>", line 1, characters 2-3
Stream.Error("illegal begin of ab_eoi")
Exception: Failure "Syntax Error".
--> "Illegal begin": at least the first symbol was correct
--> nevertheless, the reported position is correct
--> The message used to be: "b" then EOI expected after "a" in [ab_eoi]    *)

parse_string a_or_ab_eoi "a c" ;
(* File "<string>", line 1, characters 0-1
Stream.Error("illegal begin of a_or_ab_eoi")
Exception: Failure "Syntax Error".
--> "Illegal begin": at least the first non-terminal was correct
--> the reported position is weird
--> I think the message used to be either: "b" expected after "a" in
[a_or_ab]
or: EOI expected after [a_or_ab] in [a_or_ab_eoi]                       *)

parse_string c_a_or_ab_eoi "c a c" ;
(* File "<string>", line 1, characters 2-3
Stream.Error("[a_or_ab] expected after \"c\" (in [c_a_or_ab_eoi])")
Exception: Failure "Syntax Error".
--> "[a_or_ab] expected": this is very confusing: there is a valid a_or_ab
there, namely "a"                                                             *)
