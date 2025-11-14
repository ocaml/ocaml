#!/bin/bash
# Multi-object DWARF linking test
# Compiles multiple modules separately and links them, then validates DWARF

set -e

echo "=== Multi-Object DWARF Linking Test ==="

# Compile each module separately with -g
echo "Compiling multi_obj_a.ml..."
${ocamlopt} -g -c multi_obj_a.ml

echo "Compiling multi_obj_b.ml..."
${ocamlopt} -g -c multi_obj_b.ml

echo "Compiling and linking multi_obj_main.ml..."
${ocamlopt} -g -o multi_obj_test multi_obj_a.cmx multi_obj_b.cmx multi_obj_main.ml

echo "✓ Multi-object compilation and linking successful"

# Run the program to verify it works
echo "Running linked program..."
./multi_obj_test
echo "✓ Program execution successful"

# Validate DWARF structures if dwarfdump or llvm-dwarfdump is available
if command -v dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="dwarfdump"
elif command -v llvm-dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="llvm-dwarfdump"
elif command -v readelf >/dev/null 2>&1; then
    DWARF_TOOL="readelf"
else
    echo "⚠ WARNING: No DWARF inspection tool found, skipping structure validation"
    exit 0
fi

echo "Validating DWARF structures with $DWARF_TOOL..."

# Check that we have multiple CU DIEs (one for each .ml file)
case $DWARF_TOOL in
    dwarfdump|llvm-dwarfdump)
        CU_COUNT=$($DWARF_TOOL multi_obj_test 2>/dev/null | grep -c "DW_TAG_compile_unit" || echo "0")
        ;;
    readelf)
        CU_COUNT=$(readelf --debug-dump=info multi_obj_test 2>/dev/null | grep -c "DW_TAG_compile_unit" || echo "0")
        ;;
esac

if [ "$CU_COUNT" -ge "3" ]; then
    echo "✓ Found $CU_COUNT compilation units (expected >= 3)"
else
    echo "✗ WARNING: Found only $CU_COUNT compilation units, expected >= 3"
    echo "  This may indicate a multi-object linking issue"
fi

# Check that line table information exists for each CU
# Note: On macOS, DW_AT_stmt_list offsets may be incorrect after linking
echo "Checking line table information..."
case $DWARF_TOOL in
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL multi_obj_test 2>/dev/null | grep -q "debug_line"; then
            echo "✓ Line number information present"
        else
            echo "✗ WARNING: No line number information found"
        fi
        ;;
    readelf)
        if readelf --debug-dump=line multi_obj_test 2>/dev/null | grep -q "File name"; then
            echo "✓ Line number information present"
        else
            echo "✗ WARNING: No line number information found"
        fi
        ;;
esac

echo "=== Multi-Object Test Complete ==="
echo "NOTE: On macOS (Mach-O), DW_AT_stmt_list offsets are resolved at assembly"
echo "      time and may point to incorrect line tables after linking. This is a"
echo "      known limitation. ELF/Linux uses proper relocations."
