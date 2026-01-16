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

# This file contains any debugger-agnostic code for debugger plugins.
#
# Each debugger front end has three ciasses - targets, types, and
# values, which must provide these slots and methods:
#
# targets:
#
#     word_size: size of a word in 8-bit bytes.
#
#     double_size: the number of 8-bit bytes in a double-precision
#        float (i.e. 8).
#
#     value(address): a debugger "value" representing the given address
#         with type `value`.
#
#     global_variable(name): the value of a named global variable.
#
#     type(name): a named type.
#
#     symbol(address): the symbol name associated with the given address.
#
#     mapping(address): a string describing any file mapping
#         associated with the address, or None. Blocks on the Caml
#         heap do not have an associated file mapping.
#
# types:
#
#     size(): the number of bytes required for this type.
#
#     pointer(): the type of a pointer to this type.
#
#     array(size): the type of an array of given size of this type.
#
# values:
#
#     valid(): False if this value is somehow "invalid" (for example,
#       optimised away.
#
#     type(): The type of this value.
#
#     unsigned(): the value as an unsigned integer.
#
#     signed(): the value as a signed integer.
#
#     cast(t): the value cast to type `t`.
#
#     value(): the value cast to the "value" Caml runtime type.
#
#     pointer(): the value cast to "value*"
#
#     dereference(): the result of dereferencing the value, which must
#       be a pointer.
#
#     array_size(): only used when the value is an array, returns the
#       number of entries in the array.
#
#     sub(index): only used when the value is an array, returns an
#       entry, as a debugger value. TODO: switch to __getitem__ to make
#       this more transparent.
#
#     struct(): a dictionary {slot_name: value} representing the value,
#       which must be a struct. TODO: could use __getattribute__ to make
#       this more transparent.
#
#     field(index): the `index`th field of a value array whose address is
#       in the value, which can have any scalar type. `index` is a Python
#       number which can have any value, including negative.
#
#     field_pointer(index): a pointer to the `index`th field (see above).
#
#     byte_field(index): a byte value, from a byte array.
#
#     double_field(index): a double-precision floating-point value
#       from an array, as a Python float.
#
#     string(length): treating the value as an address, `length` bytes from
#       memory, decoded as UTF-8, as a Python string.
#
#     c_string(): treating the value as an address, returns the NUL-terminated
#       C string at the location as a Python string.
#
#     field_array(offset, size): an array of elements [offset,
#       offset+size), as a debugger-native array.
#
#     double_array(size): an array of double-precision floating point
#       members, as a debugger-native array.
#
#     pointer_index(index): treating the value as a pointer p, returning
#       p[index] as a value. TODO: switch to __getitem__.

MAX_BLOCK_SLOTS = 8
MAX_STRING_LEN = 80
STRING_SUFFIX = 8
STRING_PREFIX = MAX_STRING_LEN - STRING_SUFFIX - 5
MAX_STRING_SUMMARY = 20
STRING_SUMMARY_PREFIX = 8
STRING_SUMMARY_SUFFIX = MAX_STRING_SUMMARY - STRING_SUMMARY_PREFIX - 5

TAGS = {
    244: 'Forcing',
    245: 'Cont',
    246: 'Lazy',
    247: 'Closure',
    248: 'Object',
    249: 'Infix',
    250: 'Forward',
    251: 'Abstract',
    252: 'String',
    253: 'Double',
    254: 'Double_array',
    255: 'Custom'
}

# specific tag values which we display in particular ways.

TAG_CLOSURE = 247
TAG_INFIX = 249
TAG_STRING = 252
TAG_DOUBLE = 253
TAG_DOUBLE_ARRAY = 254
TAG_CUSTOM = 255

# constants for header word decoding.

HEADER_TAG_BITS = 8
HEADER_TAG_MASK = (1 << HEADER_TAG_BITS) - 1

HEADER_COLOR_BITS = 2
HEADER_COLOR_SHIFT = HEADER_TAG_BITS
HEADER_COLOR_MASK = ((1 << HEADER_COLOR_BITS) - 1) << HEADER_COLOR_SHIFT
NOT_MARKABLE = 3 << HEADER_COLOR_SHIFT

HEADER_WOSIZE_SHIFT = HEADER_TAG_BITS + HEADER_COLOR_BITS

# tags of this or above indicate blocks with no scannable fields
NO_SCAN_TAG = 251

# The debug runtime fills free and uninitialized memory with words:
#
#             D7xx D6D8 on 32-bit platforms
#   D7xx D7D7 D7xx D6D8 in 64-bit platforms
#
# where xx is one of the following 8-bit values, depending on the
# context of the memory word.

DEBUG_TAGS = {
    0x00: 'free minor',
    0x01: 'free major',
    0x03: 'free shrink',
    0x04: 'free truncate', # obsolete
    0x05: 'free unused',
    0x10: 'uninit minor',
    0x11: 'uninit major',
    0x15: 'uninit align',
    0x85: 'filler align',
    0x99: 'pool magic',
}

DEBUG_LOW_BYTES = [0xd8, 0xd6]
DEBUG_OTHER = 0xd7
DEBUG_TAG_BYTES = [2, 6]

def debug_decode(word, word_size):
    """If `word` is a debug padding word, return a string representation of
it. Otherwise, return None. `target` is used for word size."""

    if (word >> (word_size * 8)) not in {0,-1}:
        return
    bytes = [(word >> (i * 8)) & 0xff for i in range(word_size)]
    if bytes[:len(DEBUG_LOW_BYTES)] != DEBUG_LOW_BYTES:
        return
    pads = set(bytes[i]
               for i in range(len(DEBUG_LOW_BYTES), word_size)
               if i not in DEBUG_TAG_BYTES)
    if pads != {DEBUG_OTHER}: # not all pad bytes DEBUG_OTHER
        return
    tags = set(bytes[i] for i in DEBUG_TAG_BYTES if i < word_size)
    if len(tags) != 1: # differing tags on 64-bits
        return
    tag = list(tags)[0] # unique tag byte
    if tag not in DEBUG_TAGS:
        return f'Debug(0x{tag:x}?!)'
    return f'Debug({DEBUG_TAGS[tag]})'

# we show colors as [x], for some character x:

COLOR_SUMMARY = {
    'MARKED': 'm',
    'UNMARKED': 'u',
    'GARBAGE': 'g',
    'NOT MARKABLE': '-',
}

def colors(target):
    """Return a dictionary value -> name of the current GC colors
    (MARKED, UNMARKED, GARBAGE, NOT MARKABLE).
    """

    heapState = target.global_variable('caml_global_heap_state').struct()
    cols = {v.unsigned(): (f, COLOR_SUMMARY[f])
            for f, v in heapState.items()}
    cols[NOT_MARKABLE] = ('NOT MARKABLE', '-')
    return cols

class Value:
    def __init__(self, value, target):
        self._value = value
        self._target = target
        self.children = False
        self.num_children = 0
        self.valid = value.valid()
        if not self.valid:
            return

        self.word = value.signed()
        if self.word == 0:
            self.valid = False
            return

        self.immediate = (self.word & 1) == 1
        if self.immediate:
            return

        self.debug = debug_decode(self.word, target.word_size)
        if self.debug is not None:
            return

        self.pointer = value.pointer()
        self._header = self.pointer.field(-1).unsigned()
        self._wosize = self._header >> HEADER_WOSIZE_SHIFT
        self._tag = self._header & HEADER_TAG_MASK
        self._color_bits = self._header & HEADER_COLOR_MASK
        self.children = True
        self.num_children = self._wosize # overridden for some tags
        if self._tag == TAG_DOUBLE:
            self.children = False
            self.num_children = 0
        elif self._tag == TAG_DOUBLE_ARRAY:
            self.num_children = ((self.num_children * target.word_size)
                                 // target.double_size)
        elif self._tag == TAG_STRING:
            self.children = False
            self.num_children = 0
            byteSize = target.word_size * self._wosize
            lastByte = value.byte_field(byteSize-1).unsigned()
            self._length = byteSize-1-lastByte
            if self._length > 0:
                self._string = value.string(self._length)
            else:
                self._string = ''
        elif self._tag == TAG_CLOSURE:
            # collect code pointers and metadata for all functions
            # in this closure.
            self._functions = []
            # list of (code, arity [, additional code]) tuples.
            self._infix_map = {}
            # map from infix offset to tuple
            arity_shift = target.word_size * 8 - 8
            closinfo = value.field(1).signed()
            self._start_env = (closinfo & ((1 << arity_shift) - 1)) >> 1
            self.num_children = self._wosize - self._start_env
            block = 0
            while block < self._start_env:
                code = value.field(block).unsigned()
                closinfo = value.field(block+1).unsigned()
                arity = closinfo >> arity_shift
                if (arity == 0) or (arity == 1):
                    fn = (code, arity)
                    bump = 0
                else: # higher arity, so code is curry/tuplify
                    true_code = value.field(block+2).unsigned()
                    fn = (true_code, arity, code)
                    bump = 1
                self._functions.append(fn)
                self._infix_map[block] = fn
                block += 3 +bump # code, closinfo, [extra code], [infix header]
        elif self._tag == TAG_INFIX:
            self._container = Value(value.field_pointer(-self._wosize), target)
            self.num_children = 0
            self.children = False
        elif self._tag == TAG_CUSTOM:
            ptr_type = target.type('struct custom_operations').pointer()
            self._ops = value.field(0).cast(ptr_type).dereference().struct()
            self._id = self._ops['identifier'].c_string()
            self.children = False
            self.num_children = 0

    def tag_part(self):
        if self._tag in TAGS:
            return f'{TAGS[self._tag]}'
        elif self._tag == 0:
            return ''
        else:
            return f't{self._tag}'

    def infix_sym(self):
        cont = self._container
        sym = f'+{self._wosize}'
        # try to find symbol in infix map of container
        if cont._tag == TAG_CLOSURE: # always true!
            if self._wosize in cont._infix_map:
                code = cont._infix_map[self._wosize][0]
                sym = self._target.symbol(code)
                if sym is None:
                    sym = f'0x{code:x}'
        return sym

    def code_sym(self, t):
        code = t[0]
        sym = self._target.symbol(code)
        if sym is None:
            sym = f'0x{code:x}'
        if len(t) == 2:
            return sym
        else:
            return f'{sym}({self._target.symbol(t[2])})'

    def closure_syms(self):
        return [self.code_sym(t) for t in self._functions]

    def array_contents(self, short=False):
        if self._tag == TAG_DOUBLE_ARRAY:
            if self.num_children <= MAX_BLOCK_SLOTS:
                return [str(self._value.double_field(i))
                        for i in range(self.num_children)]
            return ([str(self._value.double_field(i))
                     for i in range(MAX_BLOCK_SLOTS-2)]
                    + ['...',
                       str(self._value.double_field(
                           self.num_children - 1))])

        if self.num_children < MAX_BLOCK_SLOTS:
            return [self.field_summary(i, short)
                    for i in range(self.num_children)]
        else:
            return ([self.field_summary(i, short)
                     for i in range(MAX_BLOCK_SLOTS-2)]
                    + ['...',
                       self.field_summary(self.num_children - 1, short)])

    def summary(self, short=False):
        """Return a short value summary string, suitable for display in a
        larger aggregate. If `short` then summarise the summary.
        """
        if not self.valid:
            return '[invalid]'
        if self.immediate:
            return f'{self.word // 2}'
        if self.debug is not None:
            return self.debug

        if self._tag == TAG_DOUBLE:
            return str(self._value.double_field(0))
        elif self._tag == TAG_STRING:
            if self._length > MAX_STRING_SUMMARY:
                return (repr(self._string[:STRING_SUMMARY_PREFIX])
                        + '...'
                        + repr(self._string[-STRING_SUMMARY_SUFFIX:])
                        + f'<{self._length}>')
            return repr(self._string)
        elif self._tag == TAG_INFIX:
            sym = self.infix_sym()
            return f'infix({sym}) in ' + self._container.summary(short=True)
        elif self._tag == TAG_CLOSURE:
            syms = self.closure_syms()
            if len(syms) > 1:
                sym = f'{syms[0]}, +{len(syms)-1}'
            else:
                sym = syms[0]
            return f'closure({sym})<{self.num_children}>'
        elif self._tag == TAG_CUSTOM:
            return f"custom {self._id}<{self._wosize}>"

        tag_part = self.tag_part()

        if short:
            if not tag_part:
                tag_part = 't0'
            return f'<{tag_part}:{self.num_children}>'

        if tag_part:
            tag_part += ':'

        contents = self.array_contents(short=True)

        return (f'({tag_part}' + ', '.join(contents) + ')')


    def field_summary(self, index, short=False):
        return (Value(self._value.field(index), self._target).
                summary(short))

    def __str__(self):
        if not self.valid:
            return '[invalid]'
        if self.immediate:
            return f'caml:{self.word // 2}'
        if self.debug is not None:
            return f'Caml:{self.debug}'

        color_char = colors(self._target).get(self._color_bits,
                                         f'BAD COLOR {self._color_bits}')[1]
        prefix = f'caml({color_char}):'

        if self._tag == TAG_DOUBLE:
            val = str(self._value.double_field(0))
            return f'{prefix}{val}'
        elif self._tag == TAG_STRING:
            if self._length > MAX_STRING_LEN:
                s = (repr(self._string[:STRING_PREFIX])
                     + '...' + repr(self._string[-STRING_SUFFIX:]))
            else:
                s = repr(self._string)
            return (f'{prefix}{s}<{self._length}>')
        elif self._tag == TAG_INFIX:
            sym = self.infix_sym()
            return (f'{prefix}infix({sym}) in'
                    + f' 0x{self._container._value.unsigned():x} '
                    + self._container.summary())
        elif self._tag == TAG_CLOSURE:
            syms = ', '.join(self.closure_syms())
            return (f'{prefix}closure({syms})'
                    + f' arity {self._functions[0][1]} ('
                    + ', '.join(self.field_summary(i + self._start_env)
                                for i in range(self.num_children))
                    + ')')
        elif self._tag == TAG_CUSTOM:
            return (f"{prefix}custom {self._id}"
                    f"<{self._wosize}>")

        tag_part = self.tag_part()
        if self._tag != 0:
            tag_part += ': '
        suffix = ('' if self.num_children <= MAX_BLOCK_SLOTS
                  else f'<{self.num_children}>')

        contents = self.array_contents()

        return (f'{prefix}({tag_part}'
                + ', '.join(contents)
                + f'){suffix}')

    # Useful in GDB and maybe one day in LLDB too.

    def child(self, index):
        if (not self.children) or index < 0 or index >= self.num_children:
            return
        if self._tag == TAG_DOUBLE_ARRAY:
            return self._value.double_field(index)
        elif self._tag == TAG_CLOSURE:
            return self._value.field(index + self._start_env).value()
        else:
            return self._value.field(index).value()

    # Useful in GDB and maybe one day in LLDB too.

    def child_array(self):
        """If the value is a block which can be regarded as an array,
        return the array as a debugger-native value."""
        if (not self.children):
            return
        if self._tag == TAG_DOUBLE_ARRAY:
            return self._value.double_array(self.num_children)
        elif self._tag == TAG_CLOSURE:
            return self._value.field_array(self._start_env, self.num_children)
        else:
            return self._value.field_array(0, self.num_children)

POOL_WSIZE = 4096

class Finder:
    debug = False # Settable interactively from debugger.

    def __init__(self, target):
        self._sizeclasses = None
        self._wsize_sizeclass = None
        self._target = target

    def sizeclasses(self):
        if self._sizeclasses is None:
            pool_freelist = (self._target.global_variable('pool_freelist').
                             struct())
            self._sizeclasses = (
                pool_freelist['global_avail_pools'].type().size() //
                pool_freelist['global_avail_pools'].sub(0).type().size())
        return self._sizeclasses

    def wsize_sizeclass(self, sz):
        if self._wsize_sizeclass is None:
            self._wsize_sizeclass = (self._target.
                                     global_variable('wsize_sizeclass'))
        return self._wsize_sizeclass.sub(sz).unsigned()

    def _log(self, *args):
        if self.debug:
            print(*args)

    def _found(self, where):
        if self.debug:
            print(f"FOUND 0x{self.address:x} {where}")
        self.found.append(where)
        self.keep_going = self.debug

    def search_pool_list(self, description, pool_list):
        "Search a single pool list for `self.address`."
        count = 0
        while self.keep_going and pool_list.unsigned():
            count += 1
            base = pool_list.unsigned()
            limit = base + POOL_WSIZE * self._target.word_size
            if base < self.address < limit:
                self._found(f"{description}: pool 0x{base:x}-0x{limit:x}")
            pool_list = pool_list.dereference().struct()['next']
        self._log(f"    searched {count} pools of {description}")

    def search_pools(self, description, pools):
        "Search an array `pool *pools[NUM_SIZECLASSES]` for `self.address`."
        self._log(f"  searching {description} pools")
        for i in range(self.sizeclasses()):
            pool_list = pools.sub(i)
            if pool_list.unsigned() == 0:
                continue
            self.search_pool_list(f"{description} "
                                  f"wsize={self.wsize_sizeclass(i)}",
                                  pool_list)
            if not self.keep_going:
                break

    def search_large(self, description, large_list):
        "Search a `large_alloc *` linked list for `self.address`."
        if large_list.unsigned() == 0:
            return
        count = 0
        while self.keep_going and large_list.unsigned():
            count += 1
            base = large_list.unsigned()
            block = base + large_list.dereference().type().size()
            val = self._target.value(block + self._target.word_size)
            val_ptr = val.cast(val.type().pointer())
            oval = Value(val_ptr, self._target)
            limit = block + (oval._wosize + 1) * self._target.word_size
            if base < self.address < limit:
                self._found(f"{description} large 0x{block:x}-0x{limit:x}")
            large_list = large_list.dereference().struct()['next']
        self._log(f"  searched {count} large blocks of {description}")

    def search_heap(self, description, heap_state_p):
        "Searches a single `struct caml_heap_state *` for self.address."
        if heap_state_p.unsigned() == 0:
            self._log(f"shared heap for {description} is NULL")
            return
        heap_state = heap_state_p.dereference().struct()
        if self.keep_going:
            self.search_pools(f"{description} avail",
                              heap_state['avail_pools'])
        if self.keep_going:
            self.search_pools(f"{description} full",
                              heap_state['full_pools'])
        if self.keep_going:
            self.search_pools(f"{description} unswept avail",
                              heap_state['unswept_avail_pools'])
        if self.keep_going:
            self.search_pools(f"{description} unswept full",
                              heap_state['unswept_full_pools'])
        if self.keep_going:
            self.search_large(f"{description}",
                              heap_state['swept_large'])
        if self.keep_going:
            self.search_large(f"{description} unswept",
                              heap_state['unswept_large'])

    def search_domain(self, index, caml_state_p):
        "Search a single domain's heap for `self.address`."
        caml_state = caml_state_p.dereference().struct()
        young_start = caml_state['young_start'].unsigned()
        young_end = caml_state['young_end'].unsigned()
        description = f"domain {index}"
        self._log(f"searching {description}")
        if self.keep_going and (young_start <= self.address <= young_end):
                self._found(f"{description} minor heap "
                            f"0x{young_start:x}-0x{young_end:x}")
        if self.keep_going:
            self.search_heap(description, caml_state['shared_heap'])

    def find(self, expr, val):
        if not val.valid:
            print(f"{expr} not a valid expression")
            return
        if val.immediate:
            print(f"{expr} is immediate: {str(val)}")
            return
        if val.debug is not None:
            print(f"{expr} is a debug padding value: {val.debug}")
            return

        self.address = val.pointer.unsigned()
        mapping = self._target.mapping(self.address)
        if mapping:
            print(f"{expr} {str(val)} is from {mapping}, "
                  "not the heap.")
            return

        self.found = []
        self.keep_going = True

        # Search per-domain heaps.
        all_domains = self._target.global_variable('all_domains')
        caml_params = (self._target.global_variable('caml_params')
                       .dereference().struct())
        max_domains = caml_params['max_domains'].unsigned()
        self._log(f"{max_domains} domains.")
        for i in range(max_domains):
            dom_internal = all_domains.pointer_index(i).struct()
            caml_state_p = dom_internal['state']
            if caml_state_p.unsigned() == 0: # null pointer: no domain
                self._log(f"caml_state for domain {i} is NULL")
                continue
            self.search_domain(i, caml_state_p)
            if not self.keep_going:
                break

        # Global (orphaned) heap
        pool_freelist = self._target.global_variable('pool_freelist').struct()
        if self.keep_going:
            self.search_pools('global avail',
                              pool_freelist['global_avail_pools'])
        if self.keep_going:
            self.search_pools('global full',
                              pool_freelist['global_full_pools'])
        if self.keep_going:
            self.search_large("global",
                              pool_freelist['global_large'])

        if self.found:
            print(f"{expr} {str(val)}: 0x{self.address:x} found:")
            for where in self.found:
                print(f"  {where}")
        else:
            print(f"{expr} {str(val)} not found on heap")
