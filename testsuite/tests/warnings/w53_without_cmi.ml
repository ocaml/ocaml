[@@@alert foo "foo"] (* accepted *)
[@@@alert bar "bar"] (* accepted *)

let x = 42

[@@@alert xyz "xyz"] (* rejected *)

module Sub = struct
  [@@@alert foo "foo"] (* rejected *)
  let x = 42
end
