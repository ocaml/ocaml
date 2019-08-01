(* TEST
   * toplevel
*)

(* This is a toplevel test to trigger toplevel specific hints *)


module Empty = struct end


type u = A
type v = B
module type S = sig end
let m = (module Empty:S)

module M = struct
  type 'a t = X of 'a
end
let x =M.X (A,B,m);;

module type S = sig end
let m = (module Empty:S)

type u = A
type v = B
module M = struct
  type 'a t = X of 'a
end
let y = M.X (A,B,m);;

x = y;;

type a = A
let a = A;;

type a = A
let b = A;;

a = b;;
exit 0;;
