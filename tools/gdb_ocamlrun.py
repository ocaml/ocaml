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

TAGS = {
    246: 'Lazy_tag',
    247: 'Closure_tag',
    248: 'Object_tag',
    249: 'Infix_tag',
    250: 'Forward_tag',
    251: 'Abstract_tag',
    252: 'String_tag',
    253: 'Double_tag',
    254: 'Double_array_tag',
    255: 'Custom_tag'
}

No_scan_tag = 251


debug_tags = {
    0x00: 'Debug_free_minor',
    0x01: 'Debug_free_major',
    0x03: 'Debug_free_shrink',
    0x04: 'Debug_free_truncat',
    0x10: 'Debug_uninit_minor',
    0x11: 'Debug_uninit_major',
    0x15: 'Debug_uninit_align',
    0x85: 'Debug_filler_align'
}

class DoublePrinter:
    def __init__(self, tag, length, p):
        assert tag in ['Double_tag', 'Double_array_tag']
        self.tag = tag
        self.length = length
        self.p = p

    def children(self):
        pass

    def to_string(self):
        return '%s[%d]' % (self.tag, self.length)

class ConstPrinter:
    def __init__(self, rep):
        self.rep = rep
    def to_string(self):
        return self.rep

class BlockPrinter:
    def __init__(self, val):
        if val & 1 == 1:
            self.tag = 1000
            self.tagname = 'I'
            self.val = val
        else:
            self.p = val.cast(val.type.pointer())
            header = (self.p - 1).dereference()
            self.length = int(header >> 10)
            self.gc = int(header & (3 << 8))
            self.tag = int(header & 255)
            self.tagname = TAGS.get(self.tag, 'Block')

    def children(self):
#        if self.tag < No_scan_tag:
#            fields = self.p.cast(gdb.lookup_type('value').pointer())
#            for i in range(self.length):
#                yield '[%d]' % i, (fields + i).dereference()
#        elif self.tagname == 'Double_array_tag':
#            words_per_double = \
#              gdb.lookup_type('double').sizeof / gdb.lookup_type('value').sizeof
#            fields = self.p.cast(gdb.lookup_type('double').pointer())
#            for i in range(int(self.length / words_per_double)):
#                yield '[%d]' % i, (fields + i).dereference()
#
        return []

    def to_string(self):
        if self.tag == 1000:
            # it's an immediate value
            if gdb.lookup_type('value').sizeof == 8:
                debug_mask = 0xff00ffffff00ffff
                debug_val  = 0xD700D7D7D700D6D7
            else:
                debug_mask = 0xff00ffff
                debug_val  = 0xD700D6D7
            n = self.val
            if (n & debug_mask) == debug_val:
                tag = int((n >> 16) & 0xff)
                return debug_tags.get(tag,
                                      "Debug_tag(0x%x)" % int(tag))
            else:
                return "I(%d)" % int(n >> 1)

        # otherwise, it's a block

        if self.tagname == 'Double_tag':
            d = self.p.cast(gdb.lookup_type('double').pointer()).dereference()
            s = '%f, wosize=1' % d
        elif self.tagname == 'String_tag':
            char = gdb.lookup_type('unsigned char')
            val_size = gdb.lookup_type('value').sizeof
            lastbyte = ((self.p + self.length - 1).cast(char.pointer()) + val_size - 1).dereference()
            length_bytes = self.length * val_size - (lastbyte + 1)
            string = (self.p.cast(char.array(length_bytes).pointer()).dereference())
            s = str(string).strip()
        elif self.tagname == 'Infix_tag':
            s = 'offset=%d' % (-self.length)
        elif self.tagname == 'Custom_tag':
            ops = self.p.dereference().cast(gdb.lookup_type('struct custom_operations').pointer())
            s = '%s, wosize=%d' % (str(ops), self.length)
        elif self.tagname == 'Block':
            s = '%d, wosize=%d' % (self.tag,self.length)
        else:
            s = 'wosize=%d' % self.length

        markbits = gdb.lookup_symbol("caml_global_heap_state")[0].value()
        gc = {
            int(markbits['MARKED']): 'MARKED',
            int(markbits['UNMARKED']): 'UNMARKED',
            int(markbits['GARBAGE']): 'GARBAGE',
            (3 << 8): 'NOT_MARKABLE'
        }
        return '%s(%s, %s)' % (self.tagname, s, gc[self.gc])


    def display_hint (self):
        return 'array'



class Fields(gdb.Function):
    def __init__ (self):
        super (Fields, self).__init__ ("F")

    def invoke (self, val):
        assert str(val.type) == 'value'
        p = val.cast(val.type.pointer())
        header = (p - 1).dereference()
        length = int(header >> 10)

        return p.cast(val.type.array(length - 1).pointer()).dereference()


Fields()


def value_printer(val):
    if str(val.type) != 'value':
        return None


    return BlockPrinter(val)

gdb.pretty_printers = [value_printer]
