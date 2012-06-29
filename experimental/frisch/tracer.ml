open Ast_mapper
open Longident
open Location
open Parsetree

(* To define a concrete AST rewriter, we can inherit from the generic
   mapper, and redefine the cases we are interested in.  In the
   example below, we insert in the AST some debug statements around
   each module structure. We also keep track of the current "path" in
   the compilation unit.  *)

let trace s =
  SI.eval E.(app (lid "Pervasives.print_endline") [strconst s])

let tracer =
  object
    inherit Ast_mapper.create as super
    val path = ""

    method! implementation input_name structure =
      let path = String.capitalize (Filename.chop_extension input_name) in
      {< path = path >} # default_implementation input_name structure

    method! str_module ~loc s m =
      {< path = path ^ "." ^ s.txt >} # default_str_module ~loc s m

    method! structure l =
      trace (Printf.sprintf "Entering module %s" path) ::
      (super # structure l) @
      [ trace (Printf.sprintf "Leaving module %s" path) ]
  end

let () = tracer # main
