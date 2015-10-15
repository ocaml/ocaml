(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

type effects = No_effects | Only_generative_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects

let for_primitive (prim : Lambda.primitive)
      ~second_arg_is_definitely_not_zero =
  match prim with
  | Pignore -> No_effects, No_coeffects
  | Pmakeblock _
  | Pmakearray _ -> Only_generative_effects, No_coeffects
  | Pduprecord _ ->
    Only_generative_effects,
      Has_coeffects  (* Might read a mutable record field. *)
  | Pccall { prim_name =
      ( "caml_format_float" | "caml_format_int" | "caml_int32_format"
      | "caml_nativeint_format" | "caml_int64_format" ) } ->
    No_effects, No_coeffects
  | Plazyforce
  | Pccall _ -> Arbitrary_effects, Has_coeffects
  | Praise _ -> Arbitrary_effects, No_coeffects
  | Pnot
  | Pnegint
  | Paddint
  | Psubint
  | Pmulint
  | Pandint
  | Porint
  | Pxorint
  | Plslint
  | Plsrint
  | Pasrint
  | Pintcomp _ -> No_effects, No_coeffects
  | Pdivint
  | Pmodint ->
    if second_arg_is_definitely_not_zero then
      No_effects, No_coeffects  (* Will not raise [Division_by_zero]. *)
    else
      Arbitrary_effects, No_coeffects  (* May raise [Division_by_zero]. *)
  | Poffsetint _ -> No_effects, No_coeffects
  | Poffsetref _ -> Arbitrary_effects, Has_coeffects
  | Pintoffloat
  | Pfloatofint
  | Pnegfloat
  | Pabsfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Pfloatcomp _ -> No_effects, No_coeffects
  | Pstringlength
  | Parraylength _ ->
    No_effects, Has_coeffects  (* That old chestnut: [Obj.truncate]. *)
  | Pisint
  | Pisout
  | Pbittest
  | Pbintofint _ 
  | Pintofbint _ 
  | Pcvtbint _ 
  | Pnegbint _ 
  | Paddbint _ 
  | Psubbint _ 
  | Pmulbint _ 
  | Pdivbint _ 
  | Pmodbint _ 
  | Pandbint _ 
  | Porbint _ 
  | Pxorbint _ 
  | Plslbint _ 
  | Plsrbint _ 
  | Pasrbint _ 
  | Pbintcomp _ -> No_effects, No_coeffects
  | Pbigarraydim _ ->
    No_effects, Has_coeffects  (* Some people resize bigarrays in place. *)
  | Pfield _
  | Pfloatfield _
  | Pgetglobal _
  | Pgetglobalfield _
  | Parrayrefu _
  | Pstringrefu
  | Pstring_load_16 true 
  | Pstring_load_32 true 
  | Pstring_load_64 true 
  | Pbigarrayref (true, _, _, _)
  | Pbigstring_load_16 true 
  | Pbigstring_load_32 true 
  | Pbigstring_load_64 true -> 
    No_effects, Has_coeffects
  | Parrayrefs _
  | Pstringrefs
  | Pstring_load_16 false 
  | Pstring_load_32 false 
  | Pstring_load_64 false 
  | Pbigarrayref (false, _, _, _)
  | Pbigstring_load_16 false 
  | Pbigstring_load_32 false 
  | Pbigstring_load_64 false -> 
    Arbitrary_effects, Has_coeffects  (* May trigger a bounds check exception. *)
  | Psetfield _
  | Psetfloatfield _
  | Psetglobal _
  | Psetglobalfield _
  | Parraysetu _
  | Parraysets _
  | Pstringsetu
  | Pstringsets
  | Pstring_set_16 _ 
  | Pstring_set_32 _ 
  | Pstring_set_64 _ 
  | Pbigarrayset _
  | Pbigstring_set_16 _ 
  | Pbigstring_set_32 _ 
  | Pbigstring_set_64 _ ->
    (* Whether or not some of these are "unsafe" is irrelevant; they always
       have an effect. *)
    Arbitrary_effects, No_coeffects
  | Pctconst _ -> No_effects, No_coeffects
  | Pbswap16
  | Pbbswap _ -> No_effects, No_coeffects
  | Pint_as_pointer -> No_effects, No_coeffects
  | Ploc _ ->
    Misc.fatal_error "[Ploc] should have been eliminated by [Translcore]"
  | Pidentity
  | Prevapply _
  | Pdirapply _
  | Psequand
  | Psequor ->
    Misc.fatal_errorf "The primitive %a should have been eliminated by the \
        [Closure_conversion] pass."
      Printlambda.primitive prim
