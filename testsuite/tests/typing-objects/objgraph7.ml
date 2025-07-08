(* TEST
   expect;
 *)

module M = struct
  class c1 = object
    method c1 = new c1
    method c2 = new c2
    method c3 = new c3
    method c4 = new c4
    method c5 = new c5
    method c6 = new c6
    method c7 = new c7
  end
  and c2 = object
    method c1 = new c2
    method c2 = new c3
    method c3 = new c4
    method c4 = new c5
    method c5 = new c6
    method c6 = new c7
    method c7 = new c1
  end
  and c3 = object
    method c1 = new c3
    method c2 = new c4
    method c3 = new c5
    method c4 = new c6
    method c5 = new c7
    method c6 = new c1
    method c7 = new c2
  end
  and c4 = object
    method c1 = new c4
    method c2 = new c5
    method c3 = new c6
    method c4 = new c7
    method c5 = new c1
    method c6 = new c2
    method c7 = new c3
  end
  and c5 = object
    method c1 = new c5
    method c2 = new c6
    method c3 = new c7
    method c4 = new c1
    method c5 = new c2
    method c6 = new c3
    method c7 = new c4
  end
  and c6 = object
    method c1 = new c6
    method c2 = new c7
    method c3 = new c1
    method c4 = new c2
    method c5 = new c3
    method c6 = new c4
    method c7 = new c5
  end
  and c7 = object
    method c1 = new c7
    method c2 = new c1
    method c3 = new c2
    method c4 = new c3
    method c5 = new c4
    method c6 = new c5
    method c7 = new c6
  end
end

let f (x : M.c1) = (x : M.c2)

let g (x : M.c1) = (x :> M.c2)

let h x = (x :> M.c2)

(* This one is already slow in principal mode
module M1 : sig class c1 : M.c2 end = M
*)

module M2 : sig type c1 = M.c2 end = M

module M3 : sig val f : unit -> M.c2 end = struct let f () = new M.c1 end

[%%expect{|
module M :
  sig
    class c1 :
      object
        method c1 : c1
        method c2 : c2
        method c3 : c3
        method c4 : c4
        method c5 : c5
        method c6 : c6
        method c7 : c7
      end
    and c2 :
      object
        method c1 : c2
        method c2 : c3
        method c3 : c4
        method c4 : c5
        method c5 : c6
        method c6 : c7
        method c7 : c1
      end
    and c3 :
      object
        method c1 : c3
        method c2 : c4
        method c3 : c5
        method c4 : c6
        method c5 : c7
        method c6 : c1
        method c7 : c2
      end
    and c4 :
      object
        method c1 : c4
        method c2 : c5
        method c3 : c6
        method c4 : c7
        method c5 : c1
        method c6 : c2
        method c7 : c3
      end
    and c5 :
      object
        method c1 : c5
        method c2 : c6
        method c3 : c7
        method c4 : c1
        method c5 : c2
        method c6 : c3
        method c7 : c4
      end
    and c6 :
      object
        method c1 : c6
        method c2 : c7
        method c3 : c1
        method c4 : c2
        method c5 : c3
        method c6 : c4
        method c7 : c5
      end
    and c7 :
      object
        method c1 : c7
        method c2 : c1
        method c3 : c2
        method c4 : c3
        method c5 : c4
        method c6 : c5
        method c7 : c6
      end
  end
val f : M.c1 -> M.c2 = <fun>
val g : M.c1 -> M.c2 = <fun>
val h :
  < c1 : #M.c2; c2 : #M.c3; c3 : #M.c4; c4 : #M.c5; c5 : #M.c6; c6 :
    #M.c7; c7 : #M.c1; .. > ->
  M.c2 = <fun>
module M2 : sig type c1 = M.c2 end
module M3 : sig val f : unit -> M.c2 end
|}]
