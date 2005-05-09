module A = struct
  type t = X | Y of int | Z of t * t
end

module B = struct
  type t = X | Z of t * t
end

let f (x : B.t) : A.t = {: {:x:} :}

type t = {{ <x>[] | <y val=Int>[] | <z>[ t t ] }}

let rec xml : A.t -> t = function
  | A.X -> {{ <x>[] }}
  | A.Y i -> {{ <y val={:i:}>[] }}
  | A.Z (a,b) -> {{ <z>[ (xml a) (xml b) ] }}

let () =
  let v = A.Z (A.X, A.Y 2) in
  print_endline (Xml_values.to_string {{ {:v:} }});
  print_endline (Xml_values.to_string (xml v))
