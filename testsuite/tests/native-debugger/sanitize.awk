# Replace sections of LLDB and GDB output
# This primarily looks for hex addresses, process ids, filepaths and
# other specific details of the machine the test is running on.
{
    # Replace single quoted file paths
    gsub(/'(.*)'/,"'XXXX'")

    # Replace target create for executable
    gsub(/target create "(.*)"/,"target create \"XXXX\"")

    # Replace hex addresses, not offset values less than 4 digits.
    gsub(/0x[0-9a-f][0-9a-f][0-9a-f][0-9a-f]+/, "0x00000000000000")

    # Sanitise executable name in image lookup
    gsub("5 matches found in /(.*):$", "5 matches found in XXXX")

    # Replace debug process forked by lldb
    gsub("Process ([0-9]+)", "Process XXXX")

    # Replace debug process forked by gdb
    gsub("[Inferior 1 (process [0-9]+) exited normally]", "[Inferior 1 (process XXXX) exited normally]")

    # Replace architecture identifiers
    gsub("(x86_64)", "$ARCH")
    gsub("(arm64)", "$ARCH")
    gsub("(riscv64)", "$ARCH")

    # Replace offsets in disassembly output
    gsub(/\<\+[0-9]+\>/, "<+XX>")

    # Replace comments with blank comments
    gsub(/; [a-zA-Z0-9._+ ]+/, ";")

    # Replace line numbers in runtime files - one rule for lldb, one for gdb
    # (it would be better to only match on runtime/*.c, but gsub does not
    # handle ERE)
    gsub(/.c:[0-9]+:[0-9]+/, ".c:XX")
    gsub(/.c:[0-9]+/, ".c:XX")

    # Replace line number when setting breakpoints in GDB.
    gsub(/line [0-9]+/, "line XXX")

    # Work around inconsistent name mangling
    gsub(/c_to_ocaml_[0-9]+/, "c_to_ocaml")

    gsub("warning: This version of LLDB", "This version of LLDB")
    gsub("This version of LLDB has no plugin for the language \"assembler\". Inspection of frame variables will be limited.", "")
    # Replace printed match results
    gsub("1 match found in /(.*):$", "1 match found in \"XXXX\":")

    if ($0 != "")
      print $0
}
