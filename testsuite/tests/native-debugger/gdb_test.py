import re

# Trim trailing numbers on symbols.
#
def trim_basename(name):
    return re.sub(r"(caml.*)_[0-9]+", r"\1", name)

class PrintBacktrace (gdb.Command):
  """Print backtrace by walking GDB frames"""

  def __init__ (self):
    super (PrintBacktrace, self).__init__ ("print_backtrace", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    frame = gdb.selected_frame ()
    while (not (frame is None)):
        print("frame %i: %s"% (frame.level(), trim_basename(frame.name())))
        frame = frame.older();

PrintBacktrace ()

# Usage:
# (gdb) print-backtrace
