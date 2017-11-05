module type S = sig type t end
module F(Op:S -> S -> S)(X:S)(Y:S) = Op(X)(Y);;

module K()()=struct end;;

module M = F(K)()();;
[%%expect {|
module type S = sig type t end
module F :
  functor (Op : S -> S -> S) (X : S) (Y : S) -> sig type t = Op(X)(Y).t end
module K : functor () () -> sig  end
Line _, characters 13-14:
Error: Signature mismatch:
       Modules do not match:
         functor !(()) -> ... -> !(sig  end)
       is not included in
         !(S) -> ... -> !(S)
|}]

module type s = sig type t end;;
module M: sig
        module F: s -> s
end=
struct
  module F(X:s) = struct end
end;;
[%%expect {|
module type s = sig type t end
Line _, characters 0-39:
Error: Signature mismatch:
       Modules do not match:
         sig module F : ... -> !(sig  end) end
       is not included in
         sig module F : ... -> !(s) end
       In module F:
       Modules do not match:
         functor (X : s) -> !(sig  end)
       is not included in
         s -> !(s)
       At position module F(X) : <here>
       Modules do not match: !(sig  end) is not included in !(s)
       At position module F(X) : <here>
       The type `t' is required but not provided
|}]


module type S = sig type t end;;
module F(Op:S -> S -> S)(X:S)(Y:S) = Op(X)(Y);;

module M = F()()();;
[%%expect {|
module type S = sig type t end
module F :
  functor (Op : S -> S -> S) (X : S) (Y : S) -> sig type t = Op(X)(Y).t end
Line _, characters 11-14:
Error: Signature mismatch:
       Modules do not match: !(sig  end) is not included in !(S -> ... -> S)
|}]

module type S;;
module F: S -> functor(Y:S)(Z:S) -> S -> functor (W:S) -> S =
  functor(_:S)(Y:S)(Z:S)(_:S) -> struct end;;
[%%expect {|
module type S
Line _, characters 2-43:
Error: Signature mismatch:
       Modules do not match:
         ... * 4 -> !(sig  end)
       is not included in
         ... * 4 -> !(functor (W : S)) -> !(S)
       At position functor (_) -> <here>
       Modules do not match:
         ... * 3 -> !(sig  end)
       is not included in
         ... * 3 -> !(functor (W : S)) -> !(S)
       At position functor (_) -> functor (Y) -> <here>
       Modules do not match:
         ... * 2 -> !(sig  end)
       is not included in
         ... * 2 -> !(functor (W : S)) -> !(S)
       At position functor (_) -> functor (Y) -> functor (Z) -> <here>
       Modules do not match:
         ... -> !(sig  end)
       is not included in
         ... -> !(functor (W : S)) -> !(S)
       At position
         functor (_) ->
         functor (Y) ->
         functor (Z) ->
         functor (_) ->
         <here>
       Modules do not match: !(sig  end) is not included in !(functor (W : S) -> S)
|}]

