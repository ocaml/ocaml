(* TEST
   * expect
*)

(* Make sure that compilation is done properly and that we don't access a
   private item by mistake. *)

module M = struct
  let x = 1
  private let y = 2
  let z = 3
end;;
M.x, M.z;;
[%%expect{|
module M : sig val x : int val z : int end
- : int * int = (1, 3)
|}]

module M = struct
  exception E
  private exception F
  exception G
end;;
M.E, M.G;;
[%%expect{|
module M : sig exception E exception G end
- : exn * exn = (M.E, M.G)
|}]

module M = struct
  module M = struct
    let x = 1
  end
  private module N = struct
    let x = 2
  end
  module O = struct
    let x = 3
  end
end;;
M.M.x, M.O.x;;
[%%expect{|
module M :
  sig module M : sig val x : int end module O : sig val x : int end end
- : int * int = (1, 3)
|}]

module M = struct
  type t = ..

  type t += A
  private type t += B
  type t += C
end;;
M.A, M.C;;
[%%expect{|
module M : sig type t = .. type t += A type t += C end
- : M.t * M.t = (M.A, M.C)
|}]
