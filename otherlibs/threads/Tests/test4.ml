let rec fib n = if n <= 2 then 1 else fib(n-1) + fib(n-2)

let fibtask n =
  while true do
    print_int(fib n); print_newline()
  done

let _ =
  Thread.create fibtask 28;
  while true do
    let l = read_line () in
    print_string ">> "; print_string l; print_newline()
  done
