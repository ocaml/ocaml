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
