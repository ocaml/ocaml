let name (info : Nested_api.t.info) = info.name

let _ = name (Nested_api.info (Nested_api.make "test"))
