let test = function
  | 1 -> `Primary
  | 2 -> `Secondary
  | 3 -> `Tertiary
  | n -> invalid_arg "test"

let to_string = function
  | `Primary -> "`Primary"
  | `Secondary -> "`Secondary"
  | `Tertiary -> "`Tertiary"

let () =
  assert (to_string (test 1) = "`Primary");
  assert (to_string (test 2) = "`Secondary");
  assert (to_string (test 3) = "`Tertiary")
