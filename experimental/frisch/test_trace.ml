type t = int

module A =
  struct
    let () = print_endline "FOO"
  end

module B =
  struct
    let () = print_endline "BAR"

    module C =
      struct
      end
  end


let () =
  let o = object
    method x = 1
    method y = 2
  end
  in
  ignore (o # x + o # y)
