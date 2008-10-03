open Format;
open Camlp4.PreCast;

module FV = Camlp4.Struct.FreeVars.Make Ast;

#default_quotation "expr";

value print_set f s = do {
  fprintf f "@[<2>{ ";
  FV.S.iter (fprintf f "%s@ ") s;
  fprintf f "}@]";
};

module PP = Camlp4.Printers.OCamlr.Make Syntax;
value print_expr = (new PP.printer ())#expr;

value print_status f st = pp_print_string f (if st then "PASS" else "FAIL");

value _loc = Loc.ghost;

value atoms e =
  let o = object
    inherit Ast.fold as super;
    value accu = FV.S.empty;
    method accu = accu;
    method expr =
      fun
      [ << $lid:s$ >> -> {< accu = FV.S.add s accu >}
      | e -> super#expr e ];
  end in (o#expr e)#accu;

value fv e ref =
  let s = FV.free_vars FV.S.empty e in
  let ref = atoms ref in
  let st = FV.S.equal s ref in do {
  printf "%a: @[<hv0>fv << %a >> = %a"
         print_status st
         print_expr e print_set s;
  if st then () else printf "@ ref = %a@ diff = %a"
    print_set ref print_set (FV.S.diff ref s);
  printf "@]@ ";
};

printf "@[<v0>";

fv << x >> << x >>;
fv << x y >> << x y >>;
fv << fun x -> x y >> << y >>;
fv << fun y -> fun x -> x y >> <<>>;
fv << let x = 42 and y = 44 in x y z >> << z >>;
fv << let z = g in let x = 42 and y = 44 in x y z >> << g >>;
fv << let rec f x = g (x + 1) and g y = f (y - 1) in fun x -> g x * f x >> << (+) (-) ( * ) >>;
fv << let rec f x = g (x + 1) and g y = f (g (y - 1)) in fun x -> g x * f x >> << (+) (-) ( * ) >>;

fv << let i = 42 in let module M = struct value f x = y x; end in M.h >> << y >>;

fv << fun [ A x -> x y ] >> << y >>;

fv << fun [ A x -> x y | _ -> x ] >> << x y >>;

fv << fun [ { x = A z; y = y } as q -> x z y a q ] >> << x a >>;

fv << let module M = struct value a = 42; value b = a + 1; end in () >> <<(+)>>;

fv << let module M = struct value rec a = 42; value b = a + 1; end in () >> <<(+)>>;

fv << let rec f x = x and g = x in y >> << x y >>;
fv << let f x = x in x >> << x >>;
fv << let f x = x and g x = x in x >> << x >>;
fv << let (x, y) = (42, 44) in x y z >> << z >>;

printf "@]@.";
