(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Asttypes
open Parsetree
open Longident
open Location

type stream_pattern_component =
    Spat_term of pattern * expression option
  | Spat_nterm of pattern * expression
  | Spat_sterm of pattern
type stream_expr_component =
    Sexp_term of expression
  | Sexp_nterm of expression

(* Important: see parser.mly for the difference between "mk" and "gh". *)

let mktyp d = { ptyp_desc = d; ptyp_loc = symbol_rloc() }
let mkpat d = { ppat_desc = d; ppat_loc = symbol_rloc() }
let mkexp d = { pexp_desc = d; pexp_loc = symbol_rloc() }

let ghtyp d = { ptyp_desc = d; ptyp_loc = symbol_gloc() }
let ghpat d = { ppat_desc = d; ppat_loc = symbol_gloc() }
let ghexp d = { pexp_desc = d; pexp_loc = symbol_gloc() }

let spat = Ppat_var "%strm"
let sexp = Pexp_ident (Lident "%strm")
let econ c x = ghexp (Pexp_construct (Ldot (Lident "Stream", c), x, false))
let pcon c x = ghpat (Ppat_construct (Ldot (Lident "Stream", c), x, false))
let afun f x =
  ghexp (Pexp_apply (ghexp (Pexp_ident (Ldot (Lident "Stream", f))), x))
let araise c x =
  ghexp (Pexp_apply (ghexp (Pexp_ident (Lident "raise")), [econ c x]))
let esome x = ghexp (Pexp_construct (Lident "Some", Some x, false))


(* parsers *)

let stream_pattern_component skont =
  function
    Spat_term (p, None) ->
      (afun "peek" [ghexp sexp],
       p, ghexp (Pexp_sequence (afun "junk" [ghexp sexp], skont)))
  | Spat_term (p, Some e) ->
      (afun "peek" [ghexp sexp],
       p,
       ghexp
         (Pexp_when
            (e, ghexp(Pexp_sequence (afun "junk" [ghexp sexp], skont)))))
  | Spat_nterm (p, e) ->
      (ghexp
         (Pexp_try
            (esome (ghexp (Pexp_apply (e, [ghexp sexp]))),
             [(pcon "Failure" None,
               ghexp (Pexp_construct (Lident "None", None, false)))])),
       p, skont)
  | Spat_sterm p ->
      (esome (ghexp sexp), p, skont)

(* error continuation for 2nd to last component of a stream pattern *)
let ekont1 = function
  | Some _ as estr -> araise "Error" estr
  | None -> araise "Error" (Some (ghexp (Pexp_constant (Const_string ""))))
;;

let rec stream_pattern epo e ekont =
  function
    [] ->
      begin match epo with
        Some ep ->
          let countexpr = afun "count" [ghexp sexp] in
          ghexp (Pexp_match (countexpr, [(ep, e)]))
      | _ -> e
      end
  | (spc, err) :: spcl ->
      (* success continuation *)
      let skont = stream_pattern epo e ekont1 spcl in
      let (tst, p, e) = stream_pattern_component skont spc in
      let ckont = ekont err in
      ghexp
        (Pexp_match
           (tst,
            [(ghpat (Ppat_construct (Lident "Some", Some p, false)), e);
             (ghpat Ppat_any, ckont)]))

let rec parser_cases =
  function
    [] -> araise "Failure" None
  | (spcl, epo, e)::cl -> stream_pattern epo e (fun _ -> parser_cases cl) spcl
  
let cparser (bpo, pc) =
  let pc = parser_cases pc in
  let e =
    match bpo with
      Some bp -> ghexp (Pexp_match (afun "count" [ghexp sexp], [(bp, pc)]))
    | None -> pc
  in
  let p =
    let t =
      ghtyp (Ptyp_constr (Ldot (Lident "Stream", "t"), [ghtyp Ptyp_any]))
    in
    ghpat (Ppat_constraint (ghpat spat, t))
  in
  mkexp (Pexp_function [(p, e)])


(* streams *)

let clazy e = mkexp (Pexp_function [(ghpat Ppat_any, e)])

let rec cstream =
  function
    [] -> ghexp (Pexp_ident (Ldot (Lident "Stream", "sempty")))
  | Sexp_term e :: secl -> afun "lcons" [clazy e; cstream secl]
  | Sexp_nterm e :: secl -> afun "lapp" [clazy e; cstream secl]
