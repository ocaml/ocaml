
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

let () =
  Typemod.ImplementationHooks.add_hook "pptx_example"
    (fun
      (hook_info : Misc.hook_info)
      ((ast, coercion) : Typedtree.structure * Typedtree.module_coercion) ->
        let ast = tast_mapper.structure tast_mapper ast in
        (ast, coercion)
    );
(*
  Typemod.InterfaceHooks.add_hook "pptx_example"
    (fun (hook_info : Misc.hook_info)
      (ast : Typedtree.signature) ->

        ast); *)
  ()
