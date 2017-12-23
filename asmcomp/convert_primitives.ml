(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let convert (prim : Lambda.primitive) : Clambda_primitives.primitive =
  match prim with
  | Pbytes_to_string -> Pbytes_to_string
  | Pbytes_of_string -> Pbytes_of_string
  | Pmakeblock (tag, mutability, shape) ->
      Pmakeblock (tag, mutability, shape)
  | Pfield field -> Pfield field
  | Pfield_computed -> Pfield_computed
  | Psetfield (field, imm_or_pointer, init_or_assign) ->
      Psetfield (field, imm_or_pointer, init_or_assign)
  | Psetfield_computed (imm_or_pointer, init_or_assign) ->
      Psetfield_computed (imm_or_pointer, init_or_assign)
  | Pfloatfield field -> Pfloatfield field
  | Psetfloatfield (field, init_or_assign) ->
      Psetfloatfield (field, init_or_assign)
  | Pduprecord (repr, size) -> Pduprecord (repr, size)
  | Pccall prim -> Pccall prim
  | Praise kind -> Praise kind
  | Psequand -> Psequand
  | Psequor -> Psequor
  | Pnot -> Pnot
  | Pnegint -> Pnegint
  | Paddint -> Paddint
  | Psubint -> Psubint
  | Pmulint -> Pmulint
  | Pdivint is_safe -> Pdivint is_safe
  | Pmodint is_safe -> Pmodint is_safe
  | Pandint -> Pandint
  | Porint -> Porint
  | Pxorint -> Pxorint
  | Plslint -> Plslint
  | Plsrint -> Plsrint
  | Pasrint -> Pasrint
  | Pintcomp comp -> Pintcomp comp
  | Poffsetint offset -> Poffsetint offset
  | Poffsetref offset -> Poffsetref offset
  | Pintoffloat -> Pintoffloat
  | Pfloatofint -> Pfloatofint
  | Pnegfloat -> Pnegfloat
  | Pabsfloat -> Pabsfloat
  | Paddfloat -> Paddfloat
  | Psubfloat -> Psubfloat
  | Pmulfloat -> Pmulfloat
  | Pdivfloat -> Pdivfloat
  | Pfloatcomp comp -> Pfloatcomp comp
  | Pstringlength -> Pstringlength
  | Pstringrefu -> Pstringrefu
  | Pstringrefs -> Pstringrefs
  | Pbyteslength -> Pbyteslength
  | Pbytesrefu -> Pbytesrefu
  | Pbytessetu -> Pbytessetu
  | Pbytesrefs -> Pbytesrefs
  | Pbytessets -> Pbytessets
  | Pmakearray (kind, mutability) -> Pmakearray (kind, mutability)
  | Pduparray (kind, mutability) -> Pduparray (kind, mutability)
  | Parraylength kind -> Parraylength kind
  | Parrayrefu kind -> Parrayrefu kind
  | Parraysetu kind -> Parraysetu kind
  | Parrayrefs kind -> Parrayrefs kind
  | Parraysets kind -> Parraysets kind
  | Pisint -> Pisint
  | Pisout -> Pisout
  | Pcvtbint (src, dest) -> Pcvtbint (src, dest)
  | Pnegbint bi -> Pnegbint bi
  | Paddbint bi -> Paddbint bi
  | Psubbint bi -> Psubbint bi
  | Pmulbint bi -> Pmulbint bi
  | Pbintofint bi -> Pbintofint bi
  | Pintofbint bi -> Pintofbint bi
  | Pandbint bi -> Pandbint bi
  | Porbint bi -> Porbint bi
  | Pxorbint bi -> Pxorbint bi
  | Plslbint bi -> Plslbint bi
  | Plsrbint bi -> Plsrbint bi
  | Pasrbint bi -> Pasrbint bi
  | Pbbswap bi -> Pbbswap bi
  | Pdivbint { size; is_safe } -> Pdivbint { size; is_safe }
  | Pmodbint { size; is_safe } -> Pmodbint { size; is_safe }
  | Pbintcomp (bi, comp) -> Pbintcomp (bi, comp)
  | Pbigarrayref (safe, dims, kind, layout) ->
      Pbigarrayref (safe, dims, kind, layout)
  | Pbigarrayset (safe, dims, kind, layout) ->
      Pbigarrayset (safe, dims, kind, layout)
  | Pstring_load_16 unsafe -> Pstring_load_16 unsafe
  | Pstring_load_32 unsafe -> Pstring_load_32 unsafe
  | Pstring_load_64 unsafe -> Pstring_load_64 unsafe
  | Pbytes_load_16 unsafe -> Pbytes_load_16 unsafe
  | Pbytes_load_32 unsafe -> Pbytes_load_32 unsafe
  | Pbytes_load_64 unsafe -> Pbytes_load_64 unsafe
  | Pbytes_set_16 unsafe -> Pbytes_set_16 unsafe
  | Pbytes_set_32 unsafe -> Pbytes_set_32 unsafe
  | Pbytes_set_64 unsafe -> Pbytes_set_64 unsafe
  | Pbigstring_load_16 unsafe -> Pbigstring_load_16 unsafe
  | Pbigstring_load_32 unsafe -> Pbigstring_load_32 unsafe
  | Pbigstring_load_64 unsafe -> Pbigstring_load_64 unsafe
  | Pbigstring_set_16 unsafe -> Pbigstring_set_16 unsafe
  | Pbigstring_set_32 unsafe -> Pbigstring_set_32 unsafe
  | Pbigstring_set_64 unsafe -> Pbigstring_set_64 unsafe
  | Pbigarraydim dim -> Pbigarraydim dim
  | Pbswap16 -> Pbswap16
  | Pint_as_pointer -> Pint_as_pointer
  | Popaque -> Popaque
  | Pctconst cst -> Pctconst cst

  | Pignore
  | Prevapply
  | Pdirapply
  | Pidentity
  | Pgetglobal _
  | Psetglobal _
    ->
      Misc.fatal_errorf "lambda primitive %a can't be converted to clambda primitive"
        Printlambda.primitive prim
