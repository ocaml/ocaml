(* TEST *)

module Z = struct end

module type QSig = sig
  module Z : sig end
end

module Q : QSig with module Z = Z = struct
  module Z = Z
end
