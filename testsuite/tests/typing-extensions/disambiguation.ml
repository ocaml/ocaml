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

