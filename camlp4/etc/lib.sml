structure System =
  struct
    structure Unsafe =
      struct
        structure Susp =
          struct
            type 'a susp = 'a Lazy.t
            fun delay f = (ref (Lazy.Delayed f) : 'a susp)
            fun force (f : 'a susp) = Lazy.force f
          end
      end
  end

fun null x = x = []
fun fold f = List.fold_right (fn x => fn y => f (x, y))
fun foldl f = List.fold_left (fn x => fn y => f (y, x))
fun foldr f = List.fold_left (fn x => fn y => f (y, x))

val rev = List.rev
val length = List.length
val map = List.map
