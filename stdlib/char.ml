(* Character operations *)

external code: char -> int = "%identity"
external unsafe_chr: int -> char = "%identity"

let chr n =
  if n < 0 or n > 255 then invalid_arg "Char.chr" else unsafe_chr n

external is_printable: char -> bool = "is_printable"

let escaped = function
    '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | c ->  if is_printable c then
            String.make 1 c
          else begin
            let n = code c in
            let s = String.create 4 in
            String.unsafe_set s 0 '\\';
            String.unsafe_set s 1 (unsafe_chr (48 + n / 100));
            String.unsafe_set s 2 (unsafe_chr (48 + (n / 10) mod 10));
            String.unsafe_set s 3 (unsafe_chr (48 + n mod 10));
            s
          end
