#!/usr/bin/env python

import lldb
import re

# Trim trailing numbers on symbols.
#
def trim_basename(name):
    return re.sub(r"(caml.*)_[0-9]+", r"\1", name)

# Print backtrace by walking LLDB frames
#
def print_backtrace(debugger, command, result, dict):
    """
    Walk process call stack printing out frame names
    """
    target = debugger.GetSelectedTarget()
    if target:
        process = target.GetProcess()
        if process:
            frame_info = {}
            thread = process.GetSelectedThread()
            for thread in process:
                for frame in thread:
                    print("frame %i: %s`%s"% (frame.idx, frame.module.file.basename, trim_basename(frame.name)))
        else:
            result.SetError('No current process for the debugger. Has a process to debug been launched?')
    else:
        result.SetError('No current target for the debugger. Has a process to debug been launched?')
# Usage:
# (lldb) backtrace

# Create breakpoint at address
# lldb.target.BreakpointCreateByAddress(0x0000000100005ad0)

# Create breakpoint on Regex
# lldb.target.BreakpointCreateByRegex("camlFib.fib_*")

def create_breakpoint(debugger, command, result, dict):
    """
    Create breakpoints based on symbol names
    """
    target = debugger.GetSelectedTarget()
    if target:
        breakpoint = target.BreakpointCreateByRegex(command)
        if (breakpoint and breakpoint.GetNumLocations() == 1):
            print(f"Breakpoint created for regex {command}." )
        else:
            print(f"No matches for breakpoint regex {command}.")
            result.SetError(f"No matches for breakpoint regex {command}.")
    else:
        result.SetError('No current target for the debugger. Has a process to debug been launched?')

def __lldb_init_module(debugger, internal_dict):
    # Install commands for printing backtrace and setting breakpoints
    debugger.HandleCommand('command script add -f %s.print_backtrace backtrace' % __name__)
    debugger.HandleCommand('command script add -f %s.create_breakpoint create' % __name__)
