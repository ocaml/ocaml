#ppxs_clear;;
#dump_parsetree false;;
#dump_source false;;
#directory "../../../utils";;
#load "misc.cmo";;
#load "terminfo.cmo";;
#load "warnings.cmo";;
#load "config.cmo";;
#load "identifiable.cmo";;
#load "numbers.cmo";;
#load "arg_helper.cmo";;
#load "clflags.cmo";;
#directory "../../..//parsing";;
#load "location.cmo";;
#load "docstrings.cmo";;
#load "ast_helper.cmo";;
#load "ast_mapper.cmo";;
#load "longident.cmo";;
#load "pprintast.cmo";;
(* #directory "compilerlibs";; *)
(* #load "ocamlcommon.cma";; *)
(* #directory "../parsing";; *)
(* #load "longident.cmo";; *)
(* #load "ast_mapper.cmo";; *)


let mapper1 =
  let open Ast_mapper in
  let open Parsetree in

  { default_mapper
    with structure = fun mapper item ->
      print_endline "got a structure";
      default_mapper.structure mapper item
    }
;;

let mapper2 =
  let open Ast_mapper in
  let open Parsetree in

  { default_mapper with
     expr = fun mapper e ->
       begin
         print_endline "got an expr";
         Pprintast.expression Format.std_formatter e;
         Format.print_flush ();
         print_endline "";
         match e with
         | {pexp_desc=Pexp_constant (Pconst_char 'a');_} ->
             {e with pexp_desc=Pexp_constant (Pconst_char 'A')}
         | e -> default_mapper.expr mapper e
       end
    }
;;

#plugin_ppx mapper1;;
#plugin_ppx mapper2;;
print_char 'a';;
