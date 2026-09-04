(* TEST
 toplevel;
*)

type person = {
  name : string;
  address : { street : string; city : string };
}
;;

let p =
  { name = "Alice"; address = { street = "123 Main St"; city = "Springfield" } }
;;

let street = p.address.street;;
let city = p.address.city;;
let get_city { address = { city; _ }; _ } = city;;

let updated =
  { p with address = { street = "456 Elm St"; city = "Shelbyville" } }
;;

type nested3 = { a : { b : { c : int } } };;

let x = { a = { b = { c = 42 } } };;
let v = x.a.b.c;;
let get_c { a = { b = { c } } } = c;;

type mutable_nested = { data : { mutable count : int; label : string } };;

let m = { data = { count = 0; label = "test" } };;

m.data.count <- 5;;

type config = {
  server : { host : string; port : int };
  database : { host : string; port : int };
}
;;

let cfg =
  {
    server = { host = "localhost"; port = 8080 };
    database = { host = "db.local"; port = 5432 };
  }
;;

let server_host = cfg.server.host;;
let db_host = cfg.database.host;;

let get_hosts { server = { host = sh; _ }; database = { host = dh; _ } } =
  (sh, dh)
;;

let get_server (c : config) = c.server;;
let standalone = { host = "test"; port = 99 };;

type 'a box = { inner : { value : 'a } };;

let b : int box = { inner = { value = 1 } };;
let bv = b.inner.value;;

type ('a, 'b) either_record = {
  left : { payload : 'a };
  right : { payload : 'b };
}
;;

let e : (int, string) either_record =
  { left = { payload = 42 }; right = { payload = "hi" } }
;;

let el = e.left.payload;;
let er = e.right.payload;;

type 'a deep = { a : { b : { c : 'a } } };;

let dx : float deep = { a = { b = { c = 3.14 } } };;
let dv = dx.a.b.c;;
let unwrap_deep { a = { b = { c } } } = c;;

let p2 = { p with name = "Bob" };;
let p3 = { p with address = { street = "Elm St"; city = "LA" } };;
let p4 = { p with address = { p.address with street = "Oak St" } };;
