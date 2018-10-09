(* TEST *)

open Fun.Ops

let () =
  let positive x = x > 0 in
  let l = [-1; -2; 0; 3; 1] in

  assert (List.filter (positive %> not) l = [-1; -2; 0]);
  assert (List.filter (not %< positive) l = [-1; -2; 0]);
  assert (List.filter (not %< positive %< succ) l = [-1; -2]);
  assert (List.filter (not %< (succ %> positive)) l = [-1; -2]);
  for i = - 3 to 3 do
    assert (
      (i |> succ %> positive %> not %> not)
      =
      (i |> succ %> positive |> not %> not)
    )
  done
