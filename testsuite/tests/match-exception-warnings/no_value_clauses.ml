let test f =
  match f () with exception Not_found -> ()
;;
