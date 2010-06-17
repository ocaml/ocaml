(***********************************************************************)
(*                                                                     *)
(*                           JoCaml                                    *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2008 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type 'a t =
    { put : ('a * bool Join.chan) Join.chan ;
      get : 'a option Join.chan Join.chan ;
      close : unit -> unit ;
      kill : unit Join.chan  ; }


let create () =

  def alive(xs,ys) & put(x,k) = alive(x::xs,ys) & k(true)
  or  dead() & put(_,k) = k(false)

  or  alive(xs,y::ys) & get(k) = alive(xs,ys) & k(Some y)
  or  alive(_::_ as xs,[]) & get(k) = alive([],List.rev xs) & get(k)
  or  dead() & get(k) = dead() & k(None)

  or alive([],[]) & close() = dead() & reply to close
  or dead() & close() = dead() & reply to close

  or alive(_) & kill() = dead() 
  or dead() & kill() = dead() in

  spawn alive([],[]) ;
  { get=get ; put=put; close=close ; kill=kill; }


let create_prod_cons () =
  let f = create () in
  let open JoinCom in
  { P.get = f.get ; P.kill = f.kill ; },
  { C.put = f.put ; C.close = f.close ; }

module S = struct
  exception Closed

  type 'a t =
    { put : 'a -> unit ;
      get :  unit -> 'a ;
      close : unit -> unit ;
      kill : unit -> unit ; }
end

let create_sync () =
  let f = create () in
  def put(v) =
    def k(true) & wait() = reply to wait
    or  k(false) & wait() = reply raise S.Closed to wait in
    f.put(v,k) & reply wait() to put in
  def get() =
    def k(Some v) & wait() = reply v to wait
    or  k(None) & wait()  = reply raise S.Closed to wait in
    f.get(k) & reply wait () to get in
  def kill() = f.kill() & reply to kill in
  {
   S.put = put ;
   S.get = get ;
   S.close = f.close ;
   S.kill = kill ;
  }
