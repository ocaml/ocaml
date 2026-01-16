#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 Stephen Dolan, University of Cambridge                 *
#*                                                                        *
#*   Copyright 2016 Stephen Dolan.                                        *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

import gdb

# When running inside GDB, the current directory isn't automatically
# found, so we hack the path to find ocaml.py.
import sys
import os
sys.path.append(os.path.dirname(__file__))

import ocaml

# These three classes (GDBType, GDBValue, GDBTarget) provide a
# generic interface to the debugger, to allow debugger-agnostic code
# in ocaml.py to access debugger and process state.  For a description
# of the required slots and methods, see ocaml.py.

class GDBType:
    def __init__(self, t):
        self._t = t

    def pointer(self):
        return GDBType(self._t.pointer())

    def array(self, size):
        # Amazing mis-feature in the GDB interface: the argument
        # of the `array` method is the inclusive upper index bound,
        # while the lower bound is zero. So we need to pass size-1,
        return GDBType(self._t.array(size-1))

    def size(self):
        return self._t.sizeof

class GDBValue:
    def __init__(self, v, target):
        self._v = v
        self._target = target

    def valid(self):
        # unclear what else this could mean for GDB
        return not (self._v.is_optimized_out)

    def unsigned(self):
        bits = int(self._v)
        if bits < 0:
            bits += (1 << (self._target.word_size * 8))
        return bits

    def signed(self):
        return int(self._v)

    def type(self):
        return GDBType(self._v.type)

    def cast(self, t):
        return GDBValue(self._v.cast(t._t), self._target)

    def value(self):
        return self.cast(self._target._value_type)

    def pointer(self):
        return self.cast(self._target._value_ptr_type)

    def dereference(self):
        return GDBValue(self._v.dereference(), self._target)

    def struct(self):
        return {f.name: GDBValue(self._v[f], self._target)
                for f in self._v.type.fields()}

    def array_size(self):
        range = self._v.type.range()
        return range[1]-range[0]+1

    def sub(self, index):
        return GDBValue(self._v[index], self._target)

    def field(self, index):
        res = ((self._v.cast(self._target._value_ptr_type._t) + index)
               .dereference())
        return GDBValue(res, self._target)

    def field_pointer(self, index):
        return GDBValue(self._v.cast(self._target._value_ptr_type._t) + index,
                        self._target)

    def byte_field(self, index):
        return GDBValue((self._v.cast(self._target._char_ptr_type._t) + index)
                        .dereference(), self._target)

    def double_field(self, index):
        return float((self._v.cast(self._target._double_ptr_type._t) + index)
                     .dereference())

    def string(self, length):
        return (bytes(gdb.selected_inferior().read_memory(self._v, length))
                .decode('UTF-8'))

    def c_string(self):
        return self.cast(self._target._char_ptr_type)._v.string()

    def field_array(self, offset, size):
        ptr = self._v.cast(self._target._value_ptr_type._t) + offset
        field0 = ptr.dereference()
        return field0.cast(field0.type.array(size-1))

    def double_array(self, size):
        return self._v.cast(self._target._double_type.array(size-1)._t)

    def pointer_index(self, index):
        return GDBValue(self._v[index], self._target)

class GDBTarget:
    def __init__(self):
        self._value_type = GDBType(gdb.lookup_type('value'))
        self._value_ptr_type = self._value_type.pointer()
        self._uintnat_type = GDBType(gdb.lookup_type('uintnat'))
        self._uintnat_ptr_type = self._uintnat_type.pointer()
        self._double_type = GDBType(gdb.lookup_type('double'))
        self._double_ptr_type = self._double_type.pointer()
        self._char_type = GDBType(gdb.lookup_type('char'))
        self._char_ptr_type = self._char_type.pointer()

        self.word_size = self._value_type.size()
        self.double_size = self._double_type.size()

    def global_variable(self, name):
        sym = gdb.lookup_symbol(name, domain=gdb.SYMBOL_VAR_DOMAIN)
        return GDBValue(sym[0].value(), self)

    def type(self, typename):
        return GDBType(gdb.lookup_type(typename))

    def symbol(self, address):
        # Annoyingly GDB doesn't provide a progspace.symbol_of_pc()
        # and gdb doesn't recognise OCaml functions as "functions"
        # for the purposes of progspace.block_of_pc(). So we
        # use a GDB command to get at the symbol.
        text = gdb.execute(f'info symbol 0x{address:x}', to_string=True)
        if not text.startswith('No symbol matches'):
            len = text.find(' in section ')
            if len > 0:
                return text[:len]

    def mapping(self, addr):
        # Annoyingly the progspace.solib_name() and
        # objfile_for_address() functions either aren't reliably
        # present on older versions of GDB, or return unhelpful
        # answers. So we use parse the output of `info proc mapping`.
        # This may be fragile to changes in GDB.
        text = gdb.execute('info proc mappings', to_string=True)
        all_mappings = [m.split() for m in text.split('\n') if '0x' in m]
        mappings = [m for m in all_mappings
                    if int(m[0],0) <= addr < int(m[1],0)]
        if not mappings:
            return
        file_mappings = [m[5] for m in mappings
                         if len(m) > 5
                         and not m[5].startswith('[')]
        # will be surprising if there's more than one of these
        if file_mappings:
            return ', '.join(file_mappings)

    def value(self, v):
        return GDBValue(gdb.Value(v).cast(self._value_type._t),
                        self)

# Object obeying Python's iterator protocol, for iterating through the
# children of a value. This gives us slightly nicer display of block
# values.

class BlockChildren:
    def __init__(self, value):
        self.value = value
        self.index = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self.index >= self.value.num_children:
            raise StopIteration
        element = self.value.child(self.index)
        if isinstance(element, GDBValue):
            element = element._v
        res = (str(self.index), element)
        self.index += 1
        return res

# For pretty-printing values, GDB needs an object with a to_string
# method.  Rather than pushing that into ocaml.Value, we wrap that
# class in a GDB-specific one here.

class ValuePrinter:
    def __init__(self, value):
        target = GDBTarget()
        self._v = ocaml.Value(GDBValue(value, target), target)

    def to_string(self):
        return str(self._v)

    # For pretty-printing block values with children, we
    # need a number of additional methods (which basically
    # delegate to the BlockChildren class above).

    def display_hint(self):
        if self._v.children:
            return 'array'
        else:
            return None

    def children(self):
        return BlockChildren(self._v)

    def num_children(self):
        return self._v.num_children

    def child(self, n):
        return self._v.child(n)

# The actual GDB pretty-printer.

def value_printer(val):
    if str(val.type) != 'value':
        return None
    return ValuePrinter(val)

gdb.pretty_printers = [value_printer]

# Interface to OCaml block finder

class OCamlCommand(gdb.Command):
    "Prefix of all GDB commands for debugging OCaml."
    def __init__(self):
        super(OCamlCommand, self).__init__("ocaml",
                                           gdb.COMMAND_USER, prefix=True)

OCamlCommand()

class OCamlFind(gdb.Command):
    "ocaml find <expr>: report the location of <expr> on the OCaml heap."
    def __init__(self):
        super(OCamlFind, self).__init__("ocaml find", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        self.dont_repeat()
        target = GDBTarget()
        val = ocaml.Value(GDBValue(gdb.parse_and_eval(arg),
                                   target),
                          target)
        ocaml.Finder(target).find(arg, val)

OCamlFind()

# A convenience function $Array which casts a value to an array of values.

class Array(gdb.Function):
    """Turns a Caml value into an array."""
    def __init__ (self):
        super (Array, self).__init__ ("Array")

    def invoke (self, val):
        assert str(val.type) == 'value'
        target = GDBTarget()
        v = ocaml.Value(GDBValue(val, target), target)
        return v.child_array()

Array()

print("OCaml support module loaded. Values of type 'value' will now\n"
      "print as OCaml values, there is a $Array() convenience function,\n"
      "and an 'ocaml' command is available for heap exploration\n"
      "(see 'help ocaml' for more information).")
