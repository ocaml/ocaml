(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Stefan Muenzel, Jane Street Asia                      *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
open Cmm

let rec reduce_cmm = function
  | Cop
      ( Ccmpi cmp_outer
      , [ Cop
            ( Caddi
            , [ Cop
                  ( Clsl
                  , [ Cop (Ccmpi cmp_inner, [left; right], dbg)
                    ; Cconst_int (1, _) ]
                  , _ )
              ; Cconst_int (1, _) ]
            , _ )
        ; Cconst_int (1, _) ]
      , _ ) ->
      (* nested-compare
         Size: (4 ops, 9 heads) in -> (2 ops, 5 heads) out *)
      Cop
        ( Ccmpi cmp_outer
        , [Cop (Ccmpi cmp_inner, [left; right], dbg); Cconst_int (0, dbg)]
        , dbg )
      |> reduce_cmm
  | Cop
      ( Ccmpi Ceq
      , [Cop (Ccmpi cmp_inner, [left; right], dbg); Cconst_int (1, _)]
      , _ ) ->
      (* nested-equal-one
         Size: (2 ops, 5 heads) in -> (1 ops, 3 heads) out *)
      Cop (Ccmpi cmp_inner, [left; right], dbg) |> reduce_cmm
  | Cop
      ( Ccmpi Ceq
      , [ Cop
            ( Caddi
            , [ Cop
                  ( Clsl
                  , [ Cop (Ccmpi cmp_inner, [left; right], dbg)
                    ; Cconst_int (1, _) ]
                  , _ )
              ; Cconst_int (1, _) ]
            , _ )
        ; Cconst_int (3, _) ]
      , _ ) ->
      (* nested-equal-three-tagged
         Size: (4 ops, 9 heads) in -> (1 ops, 3 heads) out *)
      Cop (Ccmpi cmp_inner, [left; right], dbg) |> reduce_cmm
  | Cop
      ( Ccmpi Cne
      , [Cop (Ccmpi cmp_inner, [left; right], dbg); Cconst_int (0, _)]
      , _ ) ->
      (* nested-nequal-zero
         Size: (2 ops, 5 heads) in -> (1 ops, 3 heads) out *)
      Cop (Ccmpi cmp_inner, [left; right], dbg) |> reduce_cmm
  | Cop
      ( Cand
      , [ Cop
            ( Caddi
            , [Cop (Clsl, [left; Cconst_int (1, _)], _); Cconst_int (1, _)]
            , _ )
        ; Cop
            ( Caddi
            , [Cop (Clsl, [right; Cconst_int (1, _)], _); Cconst_int (1, _)]
            , _ ) ]
      , dbg_op ) ->
      (* tag-and
         Size: (5 ops, 11 heads) in -> (3 ops, 7 heads) out *)
      Cop
        ( Caddi
        , [ Cop
              ( Clsl
              , [Cop (Cand, [left; right], dbg_op); Cconst_int (1, dbg_op)]
              , dbg_op )
          ; Cconst_int (1, dbg_op) ]
        , dbg_op )
      |> reduce_cmm
  | Cop
      ( Cor
      , [ Cop
            ( Caddi
            , [Cop (Clsl, [left; Cconst_int (1, _)], _); Cconst_int (1, _)]
            , _ )
        ; Cop
            ( Caddi
            , [Cop (Clsl, [right; Cconst_int (1, _)], _); Cconst_int (1, _)]
            , _ ) ]
      , dbg_op ) ->
      (* tag-or
         Size: (5 ops, 11 heads) in -> (3 ops, 7 heads) out *)
      Cop
        ( Caddi
        , [ Cop
              ( Clsl
              , [Cop (Cor, [left; right], dbg_op); Cconst_int (1, dbg_op)]
              , dbg_op )
          ; Cconst_int (1, dbg_op) ]
        , dbg_op )
      |> reduce_cmm
  | Cop
      ( Cor
      , [ Cop
            ( Cxor
            , [ Cop
                  ( Caddi
                  , [ Cop (Clsl, [left; Cconst_int (1, _)], _)
                    ; Cconst_int (1, _) ]
                  , _ )
              ; Cop
                  ( Caddi
                  , [ Cop (Clsl, [right; Cconst_int (1, _)], _)
                    ; Cconst_int (1, _) ]
                  , _ ) ]
            , dbg_xor )
        ; Cconst_int (1, _) ]
      , _ ) ->
      (* tag-xor
         Size: (6 ops, 13 heads) in -> (3 ops, 7 heads) out *)
      Cop
        ( Caddi
        , [ Cop
              ( Clsl
              , [Cop (Cxor, [left; right], dbg_xor); Cconst_int (1, dbg_xor)]
              , dbg_xor )
          ; Cconst_int (1, dbg_xor) ]
        , dbg_xor )
      |> reduce_cmm
  | other -> other
