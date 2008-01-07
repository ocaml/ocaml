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


module P = struct 
  type 'a t =
    { put : ('a * bool Join.chan) Join.chan ;
      close : unit -> unit ; }
end

module C = struct
  type 'a t =
    { get :  'a option Join.chan Join.chan ;
      kill : unit Join.chan ; }
end

let create_prod_cons () =
  let f = create () in
  { P.put = f.put ; P.close = f.close ; },
  { C.get = f.get ; C.kill = f.kill ; }


