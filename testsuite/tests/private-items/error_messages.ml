(* TEST
   * expect
*)

module Type = struct
  private type t = A

  let a = A
end
;;
[%%expect{|
Line 2, characters 10-20:
    private type t = A
            ^^^^^^^^^^
Error: This type t cannot be made private
       Line 4, characters 6-7:
         The value a has no valid type if t is removed from the signature
|}]

module Extension = struct
  private type t = ..

  type t += A
end
;;
[%%expect{|
Line 2, characters 10-21:
    private type t = ..
            ^^^^^^^^^^^
Error: This type t cannot be made private
       Line 4, characters 12-13:
         The extension constructor A has no valid type if t is removed from the
         signature
|}]

module Module = struct
  private module M = struct
    type t = A
  end

  let ma = M.A
end
;;
[%%expect{|
Line 2, characters 10-48:
  ..........module M = struct
      type t = A
    end
Error: This module M cannot be made private
       Line 6, characters 6-8:
         The value ma has no valid type if M is removed from the signature
|}]

module Module_type = struct
  private module type S

  let f (_ : (module S)) =
    ()
end
;;
[%%expect{|
Line 2, characters 10-23:
    private module type S
            ^^^^^^^^^^^^^
Error: This module type S cannot be made private
       Line 4, characters 6-7:
         The value f has no valid type if S is removed from the signature
|}]

module type Type = sig
  private type t

  val a : t
end
;;
[%%expect{|
Line 2, characters 10-16:
    private type t
            ^^^^^^
Error: This type t cannot be made private
       Line 4, characters 2-11:
         The value a has no valid type if t is removed from the signature
|}]

module type Extension = sig
  private type t = ..

  type t += A
end
;;
[%%expect{|
Line 2, characters 10-21:
    private type t = ..
            ^^^^^^^^^^^
Error: This type t cannot be made private
       Line 4, characters 12-13:
         The extension constructor A has no valid type if t is removed from the
         signature
|}]

module type Module = sig
  private module M : sig
    type t
  end

  val ma : M.t
end
;;
[%%expect{|
Line 2, characters 17-18:
    private module M : sig
                   ^
Error: This module M cannot be made private
       Line 6, characters 2-14:
         The value ma has no valid type if M is removed from the signature
|}]

module type Module_type = sig
  private module type S

  val f : (module S) -> unit
end
;;
[%%expect{|
Line 2, characters 10-23:
    private module type S
            ^^^^^^^^^^^^^
Error: This module type S cannot be made private
       Line 4, characters 2-28:
         The value f has no valid type if S is removed from the signature
|}]
