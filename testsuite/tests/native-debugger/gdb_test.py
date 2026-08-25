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

class BreakStartProgram (gdb.Command):
  """Set breakpoint inside caml_start_program at a valid instruction boundary.
     A hardcoded byte offset does not work across architectures.
     """

  def __init__ (self):
    super (BreakStartProgram, self).__init__ (
      "break_start_program", gdb.COMMAND_BREAKPOINTS)

  def invoke (self, arg, from_tty):
    arch = gdb.selected_inferior().architecture()
    start = int(gdb.parse_and_eval("&caml_start_program"))
    arch_name = arch.name()
    # Disassemble the function entry to find the correct offset.
    # On s390x the 3rd instruction is the shared entry point used
    # by callbacks, so use the 2nd instruction instead.
    if "s390" in arch_name:
      insns = arch.disassemble(start, count=2)
      offset = insns[1]['addr'] - start
    else:
      insns = arch.disassemble(start, count=3)
      offset = insns[2]['addr'] - start
    gdb.execute("break *(&caml_start_program+%d)" % offset)

BreakStartProgram ()

# Usage:
# (gdb) print-backtrace
# (gdb) break_start_program
