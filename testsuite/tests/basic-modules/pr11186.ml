(* TEST *)

module M =
  (((struct
       module N = struct let s = "Hello" end
       module A = N
       module B = A
       module C = B
     end : sig
       module A : sig val s : string end
       module B = A
       module C = B
     end) : sig
      module B : sig val s : string end
      module C = B
    end) : sig
     module C : sig val s : string end
   end)
