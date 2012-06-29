include IFDEF(XHOME)(struct
  let () = print_endline "Defined!"
end)(struct
  let () = print_endline "Not defined!"
end)
