#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                           Nick Barnes, Tarides                         *
#*                                                                        *
#*   Copyright 2024 Tarides.                                              *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

import lldb
import ocaml

def __lldb_init_module(d, _internal_dict):
    global debugger
    debugger = d
    d.HandleCommand('type summary add --python-function '
                    f'{__name__}.show_value value')
    d.HandleCommand('command container add '
                    '-h "OCaml runtime debugging utilities" '
                    'ocaml')
    d.HandleCommand('command script add --class '
                    f'{__name__}.OCamlFind '
                    'ocaml find'
                    )
    print("OCaml support module loaded. Values of type 'value' will now\n"
          "print as OCaml values, and an 'ocaml' command is available for\n"
          "heap exploration (see 'help ocaml' for more information).")

    # Synthetic Child providers don't seem so useful
    # d.HandleCommand("type synthetic add value --python-class "
    #                f"{__name__}.ChildProvider")

class OCamlFind:
    def __init__(self, debugger, internal_dict):
        super()

    def __call__(self, debugger, expr, exe_ctx, result):
        target = get_target()
        finder = ocaml.Finder(target)
        val = ocaml.Value(LLDBValue(exe_ctx.GetFrame().EvaluateExpression(expr),
                                    target),
                          target)
        finder.find(expr, val)

    def get_short_help(self):
        return "Describe the location of the given OCaml value in the heap."
    def get_long_help(self):
        return "Describe the location of the given OCaml value in the heap."


# These three classes (LLDBType, LLDBValue, LLDBTarget) provide a
# generic interface to the debugger, to allow debugger-agnostic code
# in ocaml.py to access debugger and process state.  For a description
# of the required slots and methods, see ocaml.py.

class LLDBType:
    def __init__(self, t):
        self._t = t

    def pointer(self):
        return LLDBType(self._t.GetPointerType())

    def array(self, size):
        return LLDBType(self._t.GetArrayType(size))

    def size(self):
        return self._t.size


class LLDBValue:
    def __init__(self, v, target):
        self._v = v
        self._target = target

    def valid(self):
        return self._v.IsValid()

    def unsigned(self):
        return self._v.unsigned

    def signed(self):
        return self._v.signed

    def type(self):
        return LLDBType(self._v.type)

    def cast(self, t):
        return LLDBValue(self._v.Cast(t._t), self._target)

    def value(self):
        return self.cast(self._target._value_type)

    def pointer(self):
        return self.cast(self._target._value_ptr_type)

    def dereference(self):
        return LLDBValue(self._v.Dereference(), self._target)

    def struct(self):
        t = self._v.type
        fields = t.GetNumberOfFields()
        return {member.name:
                    LLDBValue(self._v.GetChildMemberWithName(member.name),
                              self._target)
                for i in range(t.GetNumberOfFields())
                if (member := t.GetFieldAtIndex(i))}

    def array_size(self):
        return self._v.GetNumChildren()

    def sub(self, index):
        return LLDBValue(self._v.GetChildAtIndex(index,
                                                 lldb.eNoDynamicValues,
                                                 False),
                         self._target)

    def field(self, index):
        address = self.unsigned() + index * self._target.word_size
        return self._target._create_value(f'[{index}]', address,
                                          self._target._value_type)

    def field_pointer(self, index):
        return LLDBValue(self.field(index)._v.AddressOf(), self._target)

    def byte_field(self, index):
        ptr = self._v.Cast(self._target._char_ptr_type._t)
        return LLDBValue(ptr.GetChildAtIndex(index,
                                             lldb.eNoDynamicValues,
                                             True),
                         self._target)

    # converts to/from a string; could use GetValueAsUnsigned and the
    # `struct` module instead.
    def double_field(self, index):
        ptr = self._v.Cast(self._target._double_ptr_type._t)
        return float(ptr.GetChildAtIndex(index,
                                         lldb.eNoDynamicValues,
                                         True)
                     .GetValue())

    def string(self, length):
        return self._target._memory(self.unsigned(), length).decode('UTF-8')

    def c_string(self):
        return self._target._c_string(self.unsigned())

    def field_array(self, offset, size):
        address = self.unsigned() + offset * self._target.word_size
        val = self._target._create_value('[]', address,
                                         self._target._value_type.array(size))

    def double_array(self, size):
        return self._v.Cast(self._target._double_type.array(size)._t)

    def pointer_index(self, index):
        t = self._v.type.GetPointeeType()
        address = self.unsigned() + index * t.size
        return self._target._create_value("[]", address, LLDBType(t))

class LLDBTarget:
    def __init__(self, target):
        self._target = target
        self._value_type = LLDBType(target.FindFirstType("value"))
        self._value_ptr_type = self._value_type.pointer()
        self._double_type = LLDBType(target.FindFirstType("double"))
        self._double_ptr_type = self._double_type.pointer()
        self._char_type = LLDBType(target.FindFirstType("char"))
        self._char_ptr_type = self._char_type.pointer()

        self.word_size = self._value_type.size()
        self.double_size = self._double_type.size()

    def global_variable(self, name):
        return LLDBValue(self._target.FindFirstGlobalVariable(name),
                         self)

    def type(self, typename):
        return LLDBType(self._target.FindFirstType(typename))

    def symbol(self, address):
        addr = lldb.SBAddress(address, self._target)
        if addr.IsValid() and addr.symbol and addr.symbol.name:
            return addr.symbol.name

    def mapping(self, addr):
        address = self._address(addr)
        section = address.GetSection()
        module = address.GetModule()
        if section.name:
            return f"{str(module.file)}:{section.name}"


    def value(self, v):
        return LLDBValue(self._target.EvaluateExpression(f"((value){v})"),
                         self)

    ## These methods are only used by methods inside LLDBValue and/or LLDBType

    def _address(self, addr):
        return lldb.SBAddress(addr, self._target)

    def _memory(self, addr, len):
        return self._target.process.ReadMemory(addr, len, lldb.SBError())

    def _c_string(self, addr):
        return self._target.process.ReadCStringFromMemory(addr, 256,
                                                          lldb.SBError())

    def _create_value(self, name, addr, ty):
        val = self._target.CreateValueFromAddress(name,
                                                  self._address(addr),
                                                  ty._t)
        return LLDBValue(val, self)

targets = {}

def get_target():
    target = debugger.GetSelectedTarget()
    # TODO: SBTarget not hashable, but we want something hashable to
    # uniquely represent it, for advanced cases in which we end up
    # with (for instance) more than one process in the same debugging
    # session.
    key = (target.triple, target.process.id)
    if key not in targets:
        targets[key] = LLDBTarget(target)
    return targets[key]

def show_value(value, _internal_dict, options):
    target = get_target()
    return str(ocaml.Value(LLDBValue(value, target), target))

# A class like this for aggregate values should let LLDB show members
# in a natural way, but I can't get it to work.

class ChildProvider:
    def __init__(self, value, internal_dict):
        self.value = value
        self.ocaml = None

    def _update(self):
        target = get_target()
        self.ocaml = ocaml.Value(LLDBValue(self.value, target),
                                 target)

    def _ensure(self):
        if self.ocaml is None:
            self._update()

    def has_children(self):
        """Return True if the value might have children,
        False if definitely not."""
        self._ensure()
        print(f"has_children called on {self.ocaml}")
        return self.ocaml.children

    def num_children(self):
        "Return the number of children."
        self._ensure()
        print(f"num_children called on {self.ocaml}")
        return self.ocaml.num_children

    def get_child_index(self, name):
        """Return the index of a child, given part of an expression
        identifying a child. Evidently used by LLDB parser.
        """
        # this is pretty mysterious
        self._ensure()
        print(f"get_child_index called on {self.ocaml}")
        return int(name.lstrip('[').rstrip(']'))

    def get_child_at_index(self, index):
        self._ensure()
        return self.ocaml.child(index)

    def update(self):
        """Update the internal state whenever the state of the variables
        in LLDB changes. Invoked before any other method in the interface."""
        self._update()
        print(f"update called on {self.ocaml}")

    def get_value(self):
        self._update()
        print(f"get_value called on {self.ocaml}")
        if self.ocaml.children:
            return self.value.cast(
                self.value.type.GetArrayType(self.ocaml.num_children))
        else:
            return self.value
