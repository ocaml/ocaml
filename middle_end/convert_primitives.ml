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

let convert_unsafety is_unsafe : Clambda_primitives.is_safe =
  if is_unsafe then
    Unsafe
  else
    Safe

let convert (prim : Lambda.primitive) : Clambda_primitives.primitive =
  match prim with
  | Pmakeblock (tag, mutability, shape) ->
      Pmakeblock (tag, mutability, shape)
  | Pfield (field, imm_or_pointer, mutability) ->
      Pfield (field, imm_or_pointer, mutability)
  | Pfield_computed -> Pfield_computed
  | Psetfield (field, imm_or_pointer, init_or_assign) ->
      Psetfield (field, imm_or_pointer, init_or_assign)
  | Psetfield_computed (imm_or_pointer, init_or_assign) ->
      Psetfield_computed (imm_or_pointer, init_or_assign)
  | Pfloatfield field -> Pfloatfield field
  | Psetfloatfield (field, init_or_assign) ->
      Psetfloatfield (field, init_or_assign)
  | Pduprecord (repr, size) -> Pduprecord (repr, size)
  | Prunstack -> Prunstack
  | Pperform -> Pperform
  | Presume -> Presume
  | Preperform -> Preperform
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
  | Pcompare_ints -> Pcompare_ints
  | Pcompare_floats -> Pcompare_floats
  | Pcompare_bints bi -> Pcompare_bints bi
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
  | Pstring_load_16 is_unsafe ->
      Pstring_load (Sixteen, convert_unsafety is_unsafe)
  | Pstring_load_32 is_unsafe ->
      Pstring_load (Thirty_two, convert_unsafety is_unsafe)
  | Pstring_load_64 is_unsafe ->
      Pstring_load (Sixty_four, convert_unsafety is_unsafe)
  | Pbytes_load_16 is_unsafe ->
      Pbytes_load (Sixteen, convert_unsafety is_unsafe)
  | Pbytes_load_32 is_unsafe ->
      Pbytes_load (Thirty_two, convert_unsafety is_unsafe)
  | Pbytes_load_64 is_unsafe ->
      Pbytes_load (Sixty_four, convert_unsafety is_unsafe)
  | Pbytes_set_16 is_unsafe ->
      Pbytes_set (Sixteen, convert_unsafety is_unsafe)
  | Pbytes_set_32 is_unsafe ->
      Pbytes_set (Thirty_two, convert_unsafety is_unsafe)
  | Pbytes_set_64 is_unsafe ->
      Pbytes_set (Sixty_four, convert_unsafety is_unsafe)
  | Pbigstring_load_16 is_unsafe ->
      Pbigstring_load (Sixteen, convert_unsafety is_unsafe)
  | Pbigstring_load_32 is_unsafe ->
      Pbigstring_load (Thirty_two, convert_unsafety is_unsafe)
  | Pbigstring_load_64 is_unsafe ->
      Pbigstring_load (Sixty_four, convert_unsafety is_unsafe)
  | Pbigstring_set_16 is_unsafe ->
      Pbigstring_set (Sixteen, convert_unsafety is_unsafe)
  | Pbigstring_set_32 is_unsafe ->
      Pbigstring_set (Thirty_two, convert_unsafety is_unsafe)
  | Pbigstring_set_64 is_unsafe ->
      Pbigstring_set (Sixty_four, convert_unsafety is_unsafe)
  | Pbigarraydim dim -> Pbigarraydim dim
  | Pbswap16 -> Pbswap16
  | Pint_as_pointer -> Pint_as_pointer
  | Patomic_load -> Patomic_load
  | Popaque -> Popaque
  | Pdls_get -> Pdls_get
  | Ppoll -> Ppoll
  | Pbytes_to_string
  | Pbytes_of_string
  | Pctconst _
  | Pignore
  | Pgetglobal _
  | Psetglobal _
    ->
      Misc.fatal_errorf "lambda primitive %a can't be converted to \
                         clambda primitive"
        Printlambda.primitive prim
