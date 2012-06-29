include IFDEF(XHOME)(struct
  let () = print_endline "Defined!"
end)
(struct
  let () = print_endline "Not defined!"
end)


let () =
  Printf.printf "compiled by user %s in directory %s\n%!"
    (GETENV USER)
    (GETENV PWD)
