type _ t = I : int t;;

let f (type a) (x : a t) =
  let module M = struct
    let (I : a t) = x     (* fail because of toplevel let *)
    let x = (I : a t)
  end in
  () ;;
