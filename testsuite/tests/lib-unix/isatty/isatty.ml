Printf.printf
  "Unix.isatty Unix.stdin = %b\n\
   Unix.isatty Unix.stdout = %b\n\
   Unix.isatty Unix.stderr = %b"
  (Unix.isatty Unix.stdin)
  (Unix.isatty Unix.stdout)
  (Unix.isatty Unix.stderr)
