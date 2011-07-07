type t;;
type xdr_value;;

class type [ 't ] engine = object
end;;

module type T = sig
class unbound_async_call : t -> [xdr_value] engine;;
end;;
