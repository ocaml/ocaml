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
