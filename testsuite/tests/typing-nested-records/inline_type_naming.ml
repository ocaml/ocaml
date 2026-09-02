(* TEST
 expect;
*)

(* Basic type naming: type_name.field as a type expression *)

type http_request = {
  headers : { content_type : string; authorization : string };
  body : string;
}
[%%expect{|
type http_request = {
  headers : { content_type : string; authorization : string; };
  body : string;
}
|}]

(* The inline record type can be used as a type expression *)
let get_ct (h : http_request.headers) = h.content_type
[%%expect{|
val get_ct : http_request.headers -> string = <fun>
|}]

(* Use in a type annotation on a binding *)
let build_headers : http_request.headers =
  { content_type = "text/html"; authorization = "Bearer xyz" }
[%%expect{|
val build_headers : http_request.headers =
  {content_type = "text/html"; authorization = "Bearer xyz"}
|}]

(* Nested inline record types *)

type config = {
  server : { host : string; port : int; tls : { enabled : bool; cert : string } };
}
[%%expect{|
type config = {
  server : { host : string; port : int;
    tls : { enabled : bool; cert : string; };
  };
}
|}]

(* One level deep *)
let get_server (c : config) : config.server = c.server
[%%expect{|
val get_server : config -> config.server = <fun>
|}]

(* Two levels deep *)
let get_tls (c : config) : config.server.tls = c.server.tls
[%%expect{|
val get_tls : config -> config.server.tls = <fun>
|}]

(* Parametric types *)

type 'a wrapper = {
  inner : { value : 'a; label : string };
}
[%%expect{|
type 'a wrapper = { inner : { value : 'a; label : string; }; }
|}]

(* Type parameters applied to the outer type *)
let get_inner_value (w : int wrapper) : int wrapper.inner =
  w.inner
[%%expect{|
val get_inner_value : int wrapper -> int wrapper.inner = <fun>
|}]

(* Polymorphic function *)
let get_value (w : 'a wrapper) = w.inner.value
[%%expect{|
val get_value : 'a wrapper -> 'a = <fun>
|}]

(* Multiple type parameters: arity of inline field types *)

(* Inline field types keep referenced parent parameters in declaration order. *)
type ('a, 'b) either_record = {
  left : { payload : 'a };
  right : { payload : 'b };
}
[%%expect{|
type ('a, 'b) either_record = {
  left : { payload : 'a; };
  right : { payload : 'b; };
}
|}]

let get_left_payload
    (e : (int, string) either_record) : int either_record.left =
  e.left
[%%expect{|
val get_left_payload : (int, string) either_record -> int either_record.left =
  <fun>
|}]

let get_right_payload
    (e : (int, string) either_record) : string either_record.right =
  e.right
[%%expect{|
val get_right_payload :
  (int, string) either_record -> string either_record.right = <fun>
|}]

type ('a, 'phantom) phantom_parameter = {
  inner : { value : 'a };
}

let choose_phantom condition
    (left : (int, string) phantom_parameter)
    (right : (int, bool) phantom_parameter) =
  if condition then left.inner else right.inner
[%%expect{|
type ('a, 'phantom) phantom_parameter = { inner : { value : 'a; }; }
val choose_phantom :
  bool ->
  (int, string) phantom_parameter ->
  (int, bool) phantom_parameter -> int phantom_parameter.inner = <fun>
|}]

(* Parameters keep the parent declaration order, not field traversal order. *)
type ('a, 'b) parameter_order = {
  data : { first : 'a; second : 'b };
}

let ordered_data : (int, string) parameter_order.data =
  { first = 1; second = "two" }
[%%expect{|
type ('a, 'b) parameter_order = { data : { first : 'a; second : 'b; }; }
val ordered_data : (int, string) parameter_order.data =
  {first = 1; second = "two"}
|}]

(* Disambiguating overlapping field names *)

type t1 = { data : { x : int } }
type t2 = { data : { x : string; y : bool } }
[%%expect{|
type t1 = { data : { x : int; }; }
type t2 = { data : { x : string; y : bool; }; }
|}]

(* Use the type name to disambiguate which inline record we mean *)
let f1 (d : t1.data) = d.x
[%%expect{|
val f1 : t1.data -> int = <fun>
|}]

let f2 (d : t2.data) = d.x
[%%expect{|
val f2 : t2.data -> string = <fun>
|}]

(* Type naming with modules *)

module M = struct
  type t = { info : { name : string; age : int } }
end
[%%expect{|
module M : sig type t = { info : { name : string; age : int; }; } end
|}]

let get_name (i : M.t.info) = i.name
[%%expect{|
val get_name : M.t.info -> string = <fun>
|}]

(* Uppercase path components remain module paths. *)
module Qualified = struct
  module Request = struct
    type headers = int
  end
end

let qualified_headers : Qualified.Request.headers = 1
[%%expect{|
module Qualified : sig module Request : sig type headers = int end end
val qualified_headers : Qualified.Request.headers = 1
|}]

(* Round-trip: inline type printed in function signatures *)

type person = {
  name : string;
  address : { street : string; city : string };
}

let get_address (p : person) = p.address

let update_address (p : person) (a : person.address) =
  { p with address = a }
[%%expect{|
type person = {
  name : string;
  address : { street : string; city : string; };
}
val get_address : person -> person.address = <fun>
val update_address : person -> person.address -> person = <fun>
|}]

(* Type declarations still show inline records expanded *)

(* When defining a type, the inline record fields are shown in full *)
type foo = { bar : { baz : int; qux : string } }
[%%expect{|
type foo = { bar : { baz : int; qux : string; }; }
|}]

(* But when used as a type expression, it's shown as a dotted path *)
let extract_bar (f : foo) : foo.bar = f.bar
[%%expect{|
val extract_bar : foo -> foo.bar = <fun>
|}]

(* Error: non-existent inline field *)

type err1 = { a : { x : int } }

let f (v : err1.nonexistent) = v
[%%expect{|
type err1 = { a : { x : int; }; }
Line 3, characters 11-27:
3 | let f (v : err1.nonexistent) = v
               ^^^^^^^^^^^^^^^^
Error: Unbound type constructor "err1.nonexistent"
|}]

(* Error: field exists but is not an inline record *)

type err2 = { name : string; data : { x : int } }

let f (v : err2.name) = v
[%%expect{|
type err2 = { name : string; data : { x : int; }; }
Line 3, characters 11-20:
3 | let f (v : err2.name) = v
               ^^^^^^^^^
Error: Unbound type constructor "err2.name"
|}]

(* Inline record types work in other type contexts *)

type with_opt = { payload : { data : int } }

let none_payload : with_opt.payload option = None
[%%expect{|
type with_opt = { payload : { data : int; }; }
val none_payload : with_opt.payload option = None
|}]

let payload_list : with_opt.payload list = []
[%%expect{|
val payload_list : with_opt.payload list = []
|}]

(* Recursive use of nested type names *)

type +'a recursive = {
  inner : { value : 'a; next : 'a recursive.inner option };
}

let rec make_recursive value : 'a recursive.inner =
  { value; next = None }
[%%expect{|
type 'a recursive = {
  inner : { value : 'a; next : 'a recursive.inner option; };
}
val make_recursive : 'a -> 'a recursive.inner = <fun>
|}]

type +'a invalid_recursive_variance = {
  inner : {
    value : 'a;
    consume : 'a invalid_recursive_variance.inner -> unit;
  };
}
[%%expect{|
Lines 1-6, characters 0-1:
1 | type +'a invalid_recursive_variance = {
2 |   inner : {
3 |     value : 'a;
4 |     consume : 'a invalid_recursive_variance.inner -> unit;
5 |   };
6 | }
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

type 'a recursive_left = {
  inner : { next : 'a recursive_right.inner option };
}
and 'a recursive_right = {
  inner : { value : 'a; next : 'a recursive_left.inner option };
}
[%%expect{|
type 'a recursive_left = {
  inner : { next : 'a recursive_right.inner option; };
}
and 'a recursive_right = {
  inner : { value : 'a; next : 'a recursive_left.inner option; };
}
|}]

type ('a, 'phantom) recursive_phantom_left = {
  inner : { next : 'a recursive_phantom_right.inner option };
}
and ('a, 'phantom) recursive_phantom_right = {
  inner : { value : 'a };
}
[%%expect{|
type ('a, 'phantom) recursive_phantom_left = {
  inner : { next : 'a recursive_phantom_right.inner option; };
}
and ('a, 'phantom) recursive_phantom_right = { inner : { value : 'a; }; }
|}]

type ('a, 'b) recursive_poly = {
  inner : { value : 'a; apply : 'b. 'b -> int };
}
and recursive_poly_user = {
  value : int recursive_poly.inner;
}
[%%expect{|
type ('a, 'b) recursive_poly = {
  inner : { value : 'a; apply : 'b0. 'b0 -> int; };
}
and recursive_poly_user = { value : int recursive_poly.inner; }
|}]

(* Signatures *)

module Public_api : sig
  type t = { info : { name : string } }
  val get_info : t -> t.info
end = struct
  type t = { info : { name : string } }
  let get_info t = t.info
end
[%%expect{|
module Public_api :
  sig type t = { info : { name : string; }; } val get_info : t -> t.info end
|}]

(* Substitution through functors *)

module Make (X : sig type t end) = struct
  type wrapped = { inner : { value : X.t; nested : { value : X.t } } }
end

module Int_wrapped = Make (struct type t = int end)
module Int_arg = struct type t = int end

let unwrap_int (x : Int_wrapped.wrapped.inner) = x.value
let unwrap_nested_int (x : Int_wrapped.wrapped.inner.nested) = x.value
let unwrap_direct (x : Make(Int_arg).wrapped.inner) = x.value
[%%expect{|
module Make :
  (X : sig type t end) ->
    sig
      type wrapped = { inner : { value : X.t; nested : { value : X.t; }; }; }
    end
module Int_wrapped :
  sig
    type wrapped = { inner : { value : int; nested : { value : int; }; }; }
  end
module Int_arg : sig type t = int end
val unwrap_int : Int_wrapped.wrapped.inner -> int = <fun>
val unwrap_nested_int : Int_wrapped.wrapped.inner.nested -> int = <fun>
val unwrap_direct : Make(Int_arg).wrapped.inner -> Int_arg.t = <fun>
|}]

(* Destructive substitutions *)

module type Has_type = sig
  type t
  val use : t -> int
end

module type Substituted =
  Has_type with type t := http_request.headers
[%%expect{|
module type Has_type = sig type t val use : t -> int end
module type Substituted = sig val use : http_request.headers -> int end
|}]

module Replacement = struct
  type t = { inner : { value : int } }
end

module type Has_nested_module = sig
  module A : sig
    module X : sig type t = { inner : { value : int } } end
    val value : X.t.inner
  end
end

module type Module_substituted =
  Has_nested_module with module A.X := Replacement
[%%expect{|
module Replacement : sig type t = { inner : { value : int; }; } end
module type Has_nested_module =
  sig
    module A :
      sig
        module X : sig type t = { inner : { value : int; }; } end
        val value : X.t.inner
      end
  end
module type Module_substituted =
  sig module A : sig val value : Replacement.t.inner end end
|}]

(* Private parent records *)

module Private : sig
  type t = private { inner : { value : int } }
  val get_inner : t -> t.inner
end = struct
  type t = { inner : { value : int } }
  let get_inner t = t.inner
end

let private_value (x : Private.t.inner) = x.value
[%%expect{|
module Private :
  sig
    type t = private { inner : { value : int; }; }
    val get_inner : t -> t.inner
  end
val private_value : Private.t.inner -> int = <fun>
|}]

let invalid_private : Private.t.inner = { value = 1 }
[%%expect{|
Line 1, characters 40-53:
1 | let invalid_private : Private.t.inner = { value = 1 }
                                            ^^^^^^^^^^^^^
Error: Cannot create values of the private type "Private.t.inner"
|}]

(* Transparent aliases *)

type alias_source = { inner : { value : int } }
type alias = alias_source

let alias_value (x : alias.inner) = x.value
[%%expect{|
type alias_source = { inner : { value : int; }; }
type alias = alias_source
val alias_value : alias_source.inner -> int = <fun>
|}]

type ('a, 'b) remapped_source = {
  inner : { first : 'a; second : 'b };
}
type ('a, 'b) remapped_alias = ('b, 'a) remapped_source

let remapped_value
    (x : (int, string) remapped_alias.inner) :
    (string, int) remapped_source.inner =
  x
let remapped_value_again
    (x : (bool, float) remapped_alias.inner) :
    (float, bool) remapped_source.inner =
  x
let rec remapped_recursive (x : (int, string) remapped_alias.inner) =
  if Sys.opaque_identity false then remapped_recursive x else x
[%%expect{|
type ('a, 'b) remapped_source = { inner : { first : 'a; second : 'b; }; }
type ('a, 'b) remapped_alias = ('b, 'a) remapped_source
val remapped_value :
  (string, int) remapped_source.inner -> (string, int) remapped_source.inner =
  <fun>
val remapped_value_again :
  (float, bool) remapped_source.inner -> (float, bool) remapped_source.inner =
  <fun>
val remapped_recursive :
  (string, int) remapped_source.inner -> (string, int) remapped_source.inner =
  <fun>
|}]

type 'a deep_alias_source = {
  inner : { nested : { value : 'a } };
}
type 'a deep_alias = 'a deep_alias_source

let deep_alias_value (x : string deep_alias.inner.nested) = x.value
[%%expect{|
type 'a deep_alias_source = { inner : { nested : { value : 'a; }; }; }
type 'a deep_alias = 'a deep_alias_source
val deep_alias_value : string deep_alias_source.inner.nested -> string =
  <fun>
|}]

module Private_alias : sig
  type t = private alias_source
end = struct
  type t = alias_source
end

let private_alias_value (x : Private_alias.t.inner) = x.value
[%%expect{|
module Private_alias : sig type t = private alias_source end
Line 7, characters 29-50:
7 | let private_alias_value (x : Private_alias.t.inner) = x.value
                                 ^^^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "Private_alias.t.inner"
|}]

(* Variance *)

type (+'a, -'b) variance = {
  inner : { value : 'a; apply : 'b -> unit };
}

type (+'a, -'b) nested_variance = {
  value : ('a, 'b) variance.inner;
}
[%%expect{|
type ('a, 'b) variance = { inner : { value : 'a; apply : 'b -> unit; }; }
type ('a, 'b) nested_variance = { value : ('a, 'b) variance.inner; }
|}]

type +'a recursive_variance = {
  inner : { value : 'a; next : 'a recursive_variance option };
}
[%%expect{|
type 'a recursive_variance = {
  inner : { value : 'a; next : 'a recursive_variance option; };
}
|}]

type +'a private_variance = private {
  inner : { value : 'a };
}
[%%expect{|
type +'a private_variance = private { inner : { value : 'a; }; }
|}]

module type Covariant_constraint = sig
  type ('a, +'b) t constraint 'b = int
end

module Constrained_nested : Covariant_constraint = struct
  type ('a, +'b) t = {
    inner : { value : 'a };
  } constraint 'b = int
end
[%%expect{|
module type Covariant_constraint =
  sig type ('a, +'b) t constraint 'b = int end
module Constrained_nested : Covariant_constraint
|}]

type +'a variance_left = { inner : { next : 'a variance_right } }
and +'a variance_right = {
  inner : { value : 'a; next : 'a variance_left option };
}
[%%expect{|
type 'a variance_left = { inner : { next : 'a variance_right; }; }
and 'a variance_right = {
  inner : { value : 'a; next : 'a variance_left option; };
}
|}]

(* Record re-export coherence *)

type source = { inner : { value : int } }
[%%expect{|
type source = { inner : { value : int; }; }
|}]

type invalid_reexport = source = { inner : { value : string } }
[%%expect{|
Line 1, characters 0-63:
1 | type invalid_reexport = source = { inner : { value : string } }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "source"
       Fields do not match:
         "inner : { value : int; };"
       is not the same as:
         "inner : { value : string; };"
       Their nested record definitions differ.
|}]

type compatible_reexport = source = { inner : { value : int } }
[%%expect{|
type compatible_reexport = source = { inner : { value : int; }; }
|}]

let preserve_source (x : source) : source.inner = x.inner
let nested_reexport_identity
    (x : compatible_reexport.inner) : source.inner = x
[%%expect{|
val preserve_source : source -> source.inner = <fun>
val nested_reexport_identity : compatible_reexport.inner -> source.inner =
  <fun>
|}]

type ('a, 'phantom) phantom_source = { inner : { value : 'a } }
type ('a, 'phantom) phantom_reexport = ('a, 'phantom) phantom_source = {
  inner : { value : 'a };
}

let phantom_reexport_identity
    (x : int phantom_reexport.inner) : int phantom_source.inner = x
[%%expect{|
type ('a, 'phantom) phantom_source = { inner : { value : 'a; }; }
type ('a, 'phantom) phantom_reexport =
  ('a, 'phantom) phantom_source = {
  inner : { value : 'a; };
}
val phantom_reexport_identity :
  int phantom_reexport.inner -> int phantom_source.inner = <fun>
|}]

type deep_source = { inner : { nested : { value : int } } }
[%%expect{|
type deep_source = { inner : { nested : { value : int; }; }; }
|}]

type invalid_deep_reexport = deep_source = {
  inner : { nested : { value : string } };
}
[%%expect{|
Lines 1-3, characters 0-1:
1 | type invalid_deep_reexport = deep_source = {
2 |   inner : { nested : { value : string } };
3 | }
Error: This variant or record definition does not match that of type
         "deep_source"
       Fields do not match:
         "inner : { nested : { value : int; }; };"
       is not the same as:
         "inner : { nested : { value : string; }; };"
       Their nested record definitions differ.
|}]

type compatible_deep_reexport = deep_source = {
  inner : { nested : { value : int } };
}

let deep_reexport_identity
    (x : compatible_deep_reexport.inner.nested) : deep_source.inner.nested = x
[%%expect{|
type compatible_deep_reexport =
  deep_source = {
  inner : { nested : { value : int; }; };
}
val deep_reexport_identity :
  compatible_deep_reexport.inner.nested -> deep_source.inner.nested = <fun>
|}]
