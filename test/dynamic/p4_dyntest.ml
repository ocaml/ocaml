(* ocamlc -pp "camlp4o q_MLast.cmo pa_extend.cmo" -I +camlp4 -c p4_dyntest.ml *)
(*
  let counter = ref 0;;
  DYNTEST <VALUE> as <TYPE> to <TYPE> for <BOOL> in counter;;
  SUMMARY in counter;;
 *)

open Pcaml

let string_of_type loc ty = <:expr< "{{TYPE}}" >>

EXTEND
  expr: LEVEL "top"
    [ [ "DYNTEST"; v = expr; "as"; from_ty = ctyp; "to"; to_ty = ctyp;
        "for"; b = expr; "in"; c = expr ->
          let vv = <:expr<(v : $from_ty$)>> in
          <:expr<
            let success =
              let v = $v$ in
              try let _ = $MLast.ExDco (loc, MLast.ExDyn (loc, vv), to_ty)$ in True
              with [ Dynamics.Type_error _ -> False ]
            and expected = $b$
            and counter = $c$
            in do {
            print_string $string_of_type loc from_ty$;
            print_string " ---> ";
            print_string $string_of_type loc to_ty$;
            print_string (if success then "  [OK]" else "  [FAIL]");
            if success <> expected then do {
              print_string " !!!!!!!!!!!!!!!!";
              incr counter
            } else ();
            print_newline ()
            }
          >>
      | "SUMMARY"; "in"; c = expr ->
          <:expr<
            let counter = $c$ in
            if counter.val = 0 then do {
              print_endline "All tests had the expected results. Good!";
              exit 0
            } else do {
              print_int counter.val;
              print_endline " tests had the wrong result.";
              exit 1
            }
          >>
     ] ]
  ;
END;;
