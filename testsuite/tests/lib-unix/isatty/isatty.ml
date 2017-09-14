let console =
  try
    Unix.(openfile "/dev/tty" [O_RDWR] 0)
  with _ ->
    Unix.(openfile "CONIN$" [O_RDWR] 0)
in
Printf.printf
  "Unix.isatty Unix.stdin = %b\n\
   Unix.isatty Unix.stdout = %b\n\
   Unix.isatty Unix.stderr = %b\n\
   /dev/tty = %b\n"
  (Unix.isatty Unix.stdin)
  (Unix.isatty Unix.stdout)
  (Unix.isatty Unix.stderr)
  (Unix.isatty console)
