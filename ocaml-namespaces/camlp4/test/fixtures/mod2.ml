module type S = sig type t = 'a; end;
module F (A : S) = struct
  type t2 = A.t;
end;

module A = struct type t = int; end;

module type S2 = S with type t = (F A).t2;
