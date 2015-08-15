let ignore _ = ()

module Pervasives = struct
  (* the evil shadower *)
  let compare x y = (x, y)
  let (<=) x y = (x, y)
  let (mod) x y = (x, y)
  let (.()) x y = (x, y)
  let x = ()
end

[@@@ocaml.warning "+A"]

(* this should not warn: shadowed identifier is not used *)
let _ = fun compare ->
  ignore compare;
  let open Pervasives in
  x

let _ = fun (<=) ->
  ignore (<=);
  let open Pervasives in
  x

let _ = fun (mod) ->
  ignore (mod);
  let open Pervasives in
  x

let _ = fun (.()) ->
  ignore (.());
  let open Pervasives in
  x

(* this should raise 44 (the old warning) *)
let _ = fun compare ->
  ignore compare;
  let open Pervasives in
  compare

(* this should raise 44 (the old warning) *)
let _ = fun (<=) ->
  ignore (<=);
  let open Pervasives in
  (<=)

let _ = fun (mod) ->
  ignore (mod);
  let open Pervasives in
  (mod)

let _ = fun (mod) a b ->
  ignore (mod);
  let open Pervasives in
  a mod b

let _ = fun (.()) ->
  ignore (.());
  let open Pervasives in
  (.())

let _ = fun (.()) a b ->
  ignore (.());
  let open Pervasives in
  a.(b)

[@@@ocaml.warning "+A-44"]

(* this should raise 52 (the new warning) *)
let _ = fun compare ->
  ignore compare;
  let open Pervasives in
  compare

(* this should raise 53 (the new warning) *)
let _ = fun (<=) ->
  ignore (<=);
  let open Pervasives in
  (<=)

let _ = fun (mod) ->
  ignore (mod);
  let open Pervasives in
  (mod)

let _ = fun (mod) a b ->
  ignore (mod);
  let open Pervasives in
  a mod b

let _ = fun (.()) ->
  ignore (.());
  let open Pervasives in
  (.())

let _ = fun (.()) a b ->
  ignore (.());
  let open Pervasives in
  a.(b)

[@@@ocaml.warning "-A"]

(* this shouldn't raise anything *)

let _ = fun compare ->
  ignore compare;
  let open Pervasives in
  compare

let _ = fun (<=) ->
  ignore (<=);
  let open Pervasives in
  (<=)

let _ = fun (mod) ->
  ignore (mod);
  let open Pervasives in
  (mod)

let _ = fun (mod) a b ->
  ignore (mod);
  let open Pervasives in
  a mod b

let _ = fun (.()) ->
  ignore (.());
  let open Pervasives in
  (.())

let _ = fun (.()) a b ->
  ignore (.());
  let open Pervasives in
  a.(b)
