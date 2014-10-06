open Flambdapasses
open Symbol
open Abstract_identifiers
open Flambda
open Test_utils

let check expr =
  Flambdacheck.check
    ~current_compilation_unit:compilation_unit
    expr

let expr1 = tuple [int 1;int 2]
let expr2 =
  fif (fbool true)
    (tuple [int 1;int 2])
    (tuple [int 3;int 4])
let expr3 =
  fif (fbool true)
    (fseq [
        int 33;
        tuple [int 4;int 5];
      ])
    (expr2)

let launch (s,e) =
  let e' = Flambdaifstaticraise.if_static_raise_pass.pass e compilation_unit in
  Format.printf "%s@ orig:@ %a@.converted:@ %a@."
    s
    Printflambda.flambda e
    Printflambda.flambda e';
  check e;
  check e'

let run () =
  List.iter launch
    [ "1", expr1;
      "2", expr2;
      "3", expr3; ]
