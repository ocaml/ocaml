open Camlp4.PreCast;

value rec mk_tuple _loc t n =
  if n <= 1 then t else <:ctyp< $t$ * $mk_tuple _loc t (n - 1)$ >>;

value ctyp_eoi = Gram.Entry.mk "ctyp eoi";

EXTEND Gram
  ctyp_eoi: [[ t = Syntax.ctyp; `EOI -> t ]];
END;

value exp _loc _ s =
  Scanf.sscanf s " %d | %[^!]" begin fun n s ->
    let t = Syntax.Gram.parse_string ctyp_eoi _loc(* not accurate *) s in
    <:ctyp< $tup:mk_tuple _loc t n$ >>
  end;

Quotation.add "power" Quotation.DynAst.ctyp_tag exp;
