(* TEST
flags = "-no-app-funct"
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)
module type Dep = sig type t val x : t end
module String = struct type t = string let x = "Forty Two" end
module Int = struct type t = int let x = 42 end

module type S = sig
  val x : 'a option
  module M : functor (X : Dep) -> sig
    val x : X.t option
    module M : functor (Y : Dep) -> sig
      val x : X.t option
    end
  end
end

module type S' = sig
  val x : 'a option
  module M : functor (_ : Dep) -> S
end

module Bad (A : S') : S = A

module M = struct
  let x = None
  module M (_ : Dep) = struct
    let x = None
    module M (X : Dep) = struct
      let x = Some X.x
      module M (Y : Dep) = struct
        let x = Some X.x
      end
    end
  end
end

module N = Bad(M)
module N' = N.M(String)
module N'' = N'.M(Int)

let () = print_endline (Option.get N''.x)
