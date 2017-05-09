module X : sig
  val x : int [@@deprecated "DEPRECATED"]
end = struct
  let x = 7
end

module Y : sig val x : int end = X

module Z : sig val x : int [@@deprecated "..."] end = X

module F(A : sig val x : int end) = struct end

module B = F(X)



module XX = struct let x = 7 end
module YY : sig val x : int [@@deprecated "..."] end = XX
