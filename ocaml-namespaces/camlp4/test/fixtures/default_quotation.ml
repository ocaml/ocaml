#default_quotation "expr";
open Camlp4.PreCast;
fun [ << $x$ - $y$ >> when x = y -> << 0 >>
    | e -> e ];
