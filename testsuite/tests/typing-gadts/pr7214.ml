type _ t = I : int t;;

let f (type a) (x : a t) =
  let module M = struct
    let (I : a t) = x     (* fail because of toplevel let *)
    let x = (I : a t)
  end in
  () ;;

(* extra example by Stephen Dolan, using recursive modules *)
(* Should not be allowed! *)
type (_,_) eq = Refl : ('a, 'a) eq;;

let bad (type a) =
 let module N = struct
   module rec M : sig
     val e : (int, a) eq
   end = struct
     let (Refl : (int, a) eq) = M.e  (* must fail for soundness *)
     let e : (int, a) eq = Refl
   end
 end in N.M.e
;;
