(** Test type-directed disambiguation and spellchecker hints *)

type t = ..
type t += Alpha

module M = struct
  type w = ..
  type w += Alpha | Beta ;;
end;;

module F(X:sig end) = struct type t += Gamma end;;
module X = struct end;;

let x: t = Alha;;
open M;;
let y : w = Alha;;
let z: t = Alpha;;

module N = F(X);;
let g: t = N.Gamm ;;

raise Not_Found;;

type r = ..;;
module M = struct
  type t = r = ..
  type s = t = ..
  module N = struct
    type u = s = ..
    type u += Foo
  end
end
open M.N;;

type exn += Foo;;

let x : r = Foo;;

;; (** Closed open extensible type support *)

module M : sig
  type t
  type t += Aleph
end = struct
  type t = ..
  type t += Aleph
end;;
open M;;

type exn += Aleph
;;
let x : t = Aleph;;

;;

module F(X: sig type t = .. end ) = struct type X.t+= Beth end
module X = struct type t = .. end
module FX = F(X) open FX
type exn += Beth;;
let x : X.t = Beth;;
