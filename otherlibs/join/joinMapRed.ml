(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

module type Problem = sig
  val identifier : string

  type init

  type client_data

  type input

  type key

  type value

  type output

  val init_client : init -> client_data

  val compare_keys : key -> key -> int

  val map : client_data -> input -> (key * value) list

  val combine : value -> value -> value

  val reduce : key -> value -> output -> output
end

module type S = sig
  type init

  type input

  type output

  val client : JoinHelper.configuration -> unit

  val server :
      JoinHelper.configuration -> init ->
        ('a, input) JoinPool.Simple.enum -> output -> output
end

module Make (P : Problem) : S
  with type input = P.input and type output = P.output and type init = P.init = struct

  let register_id = String.copy P.identifier

  type init = P.init

  type input = P.input

  type output = P.output

  module M = Map.Make (struct type t = P.key let compare = P.compare_keys end)

  let client cfg =
    let _, _,
      (i, register : P.init * ((P.input -> (P.key * P.value) list)) Join.chan) =
      JoinHelper.init_client_with_lookup
        ~at_fail:JoinHelper.exit_at_fail
        ~lookup:(JoinHelper.lookup_times 0 1.0)
        cfg
        register_id in
    let cd = P.init_client i in
    def work(x) = reply (P.map cd x) to work in
    spawn register(work);
    JoinHelper.wait_forever ()

  let server cfg i enum z =
    let comb l m =
      List.fold_left
        (fun m (k, v) ->
          let curr = try Some (M.find k m) with Not_found -> None in
          let v' = match curr with Some x -> P.combine x v| None -> v in
          M.add k v' m)
        m
        l in
    let pool = JoinPool.Simple.create enum comb M.empty in
    let _ =
      JoinHelper.init_server_with_register cfg register_id
        (i, pool.JoinPool.Simple.register) in
    let map = pool.JoinPool.Simple.wait () in
    M.fold P.reduce map z
end
