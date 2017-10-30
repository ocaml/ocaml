
(* Print all fully qualified names in expressions *)

open Asttypes
open Longident
open Typedtree


open Tast_mapper

let tast_mapper =
  { default with
    value_bindings = (fun mapper (flg, xs) -> (flg, xs @ (List.map (mapper.value_binding mapper) xs)));
    value_binding = (fun _ vb ->
      let new_desc = 
        match vb.vb_pat.pat_desc with
        | Tpat_var (ident, str) -> Tpat_var (Ident.create (ident.Ident.name ^ "XXX"), {str with txt = str.txt ^ "XXX"})
        | p -> p
      in
      {vb with vb_pat = { vb.vb_pat with pat_desc = new_desc } })
 }

let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

let () =
  Typemod.ImplementationHooks.add_hook "pptx_example"
    (fun
      (hook_info : Misc.hook_info)
      ((ast, _, coercion) : Typedtree.structure * _ * Typedtree.module_coercion) ->
        let () = print_endline "plugin is called" in
        let ast = tast_mapper.structure tast_mapper ast in
        let ast = 
          Untypeast.untype_structure ast |>
          print_if Format.std_formatter Clflags.dump_source Pprintast.structure
        in
        
        let (ast, sign, _env) = Typemod.type_structure (Compmisc.initial_env ()) ast Location.none in
        let sign = print_if Format.std_formatter (ref true) Printtyp.signature sign in
        (ast, sign, coercion)
    );
(*
  Typemod.InterfaceHooks.add_hook "pptx_example"
    (fun (hook_info : Misc.hook_info)
      (ast : Typedtree.signature) ->

        ast); *)
  ()
