##ifdef CAMLTK

let contained x y w =
  w = containing x y
;;

##else

let contained ~x ~y w =
  forget_type w = containing ~x ~y ()
;;

##endif
