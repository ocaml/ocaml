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

type effects = No_effects | Has_effects
type coeffects = No_coeffects | Has_coeffects

let for_primitive (prim : Lambda.primitive)
      ~second_arg_is_definitely_not_zero =
  match prim with
  | Pidentity -> No_effects, No_coeffects
  | Pignore -> No_effects, No_coeffects
  | Prevapply _ ->
  | Pdirapply _ -> 
  | Ploc _ -> 
  | Pgetglobal _ -> No_effects, Has_coeffects
  | Psetglobal _ -> Has_effects, No_coeffects
  | Pgetglobalfield _ -> No_effects, Has_coeffects
  | Psetglobalfield _ -> Has_effects, No_coeffects
  | Pmakeblock _ -> No_effects, No_coeffects
  | Pfield _ -> No_effects, Has_coeffects
  | Psetfield _ -> Has_effects, No_coeffects
  | Pfloatfield _ -> 
  | Psetfloatfield _ -> 
  | Pduprecord _ -> 
  | Plazyforce
  | Pccall _ -> Has_effects, Has_coeffects
  | Praise _ -> Has_effects, No_coeffects
  | Psequand | Psequor | Pnot
  | Pnegint | Paddint | Psubint | Pmulint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp _ -> No_effects, No_coeffects
  | Pdivint | Pmodint ->
    if second_arg_is_definitely_not_zero then
      No_effects, No_coeffects  (* will not raise [Division_by_zero] *)
    else
      Has_effects, No_coeffects  (* may raise [Division_by_zero] *)
  | Poffsetint _ -> 
  | Poffsetref _ -> 
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp _ -> 
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
  | Pmakearray _ -> 
  | Parraylength _ -> 
  | Parrayrefu _ -> 
  | Parraysetu _ -> 
  | Parrayrefs _ -> 
  | Parraysets _ -> 
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
  | Pbigarrayref _ -> 
  | Pbigarrayset _ -> 
  | Pbigarraydim _ -> 
  | Pstring_load_16 _ -> 
  | Pstring_load_32 _ -> 
  | Pstring_load_64 _ -> 
  | Pstring_set_16 _ -> 
  | Pstring_set_32 _ -> 
  | Pstring_set_64 _ -> 
  | Pbigstring_load_16 _ -> 
  | Pbigstring_load_32 _ -> 
  | Pbigstring_load_64 _ -> 
  | Pbigstring_set_16 _ -> 
  | Pbigstring_set_32 _ -> 
  | Pbigstring_set_64 _ -> 
  | Pctconst _ -> 
  | Pbswap16
  | Pbbswap _ -> No_effects, No_coeffects
  | Pint_as_pointer -> No_effects, No_coeffects
