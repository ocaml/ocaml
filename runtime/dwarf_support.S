/* DWARF support symbols for multi-object linking */

#if defined(__APPLE__)
    .section __DWARF,__debug_line,regular,debug
    .globl __debug_line_section_base
    .weak_definition __debug_line_section_base
__debug_line_section_base:
#endif
