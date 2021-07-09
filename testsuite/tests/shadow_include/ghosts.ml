(* TEST
  * expect
*)

module C = struct
  class c = object end
end

module R = struct
  include C
  type c
end
[%%expect {|
module C : sig class c : object  end end
module R : sig type c end
|}]


module CT = struct
  include C
  class type c = object end
end
[%%expect {|
module CT : sig class type c = object  end end
|}]


module P = struct
  type t = private < .. >
end

module M = struct
  include P
  type t = A
end
[%%expect {|
module P : sig type t = private < .. > end
module M : sig type t = A end
|}]
