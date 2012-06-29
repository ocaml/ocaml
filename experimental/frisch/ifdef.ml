(* This filter implements the following rewriting on module expressions:

   IFDEF(X)(<m1>)(<m2>)
               ---> <m1>      if the environment variable X is defined
               ---> <m2>      otherwise

   And, on expressions:

   GETENV X    ---> the string literal representing the compile-time value
                    of environment variable X
*)

open Ast_mapper
open Parsetree
open Longident
open Location

let getenv s = try Sys.getenv s with Not_found -> ""

let ifdef =
  object(this)
    inherit Ast_mapper.create as super

    method! module_expr = function
      | {pmod_desc = Pmod_apply(
         {pmod_desc = Pmod_apply(
          {pmod_desc = Pmod_apply(
           {pmod_desc = Pmod_ident {txt = Lident "IFDEF"}},
           {pmod_desc = Pmod_ident {txt = Lident sym}}
          )},
          body_def)},
         body_not_def)} ->
           if getenv sym <> "" then
             this # module_expr body_def
           else
             this # module_expr body_not_def

      | {pmod_desc = Pmod_ident {txt = Lident "IFDEF"}; pmod_loc = loc} ->
          Format.printf "%a@.Improper use of IFDEF. The correct form is: IFDEF(<var_name:uident>)(<then:modtype>)(<body:modtype>)@."
            Location.print_loc loc;
          exit 2
      | x -> super # module_expr x

    method! expr = function
      | {pexp_desc = Pexp_construct (
         {txt = Lident "GETENV"},
         Some {pexp_loc = loc; pexp_desc = Pexp_construct (
               {txt = Lident sym},
               None,
               _
              )},
         _
        )} ->
          E.strconst ~loc (getenv sym)
      | x -> super # expr x
  end

let () = ifdef # main
