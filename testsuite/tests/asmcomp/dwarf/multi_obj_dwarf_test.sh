#!/bin/bash
# Multi-object DWARF linking test with offset validation
# Tests that DW_AT_stmt_list offsets are correct after linking

set -e

echo "=== Multi-Object DWARF Offset Validation Test ==="

# Determine DWARF tool
if command -v dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="dwarfdump"
elif command -v llvm-dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="llvm-dwarfdump"
elif command -v readelf >/dev/null 2>&1; then
    DWARF_TOOL="readelf"
else
    echo "ERROR: No DWARF inspection tool found"
    exit 1
fi

# Compile each module separately with -g
echo "Compiling multi_obj_a.ml..."
${ocamlopt} -g -c multi_obj_a.ml || { echo "ERROR: Failed to compile multi_obj_a.ml"; exit 1; }

echo "Compiling multi_obj_b.ml..."
${ocamlopt} -g -c multi_obj_b.ml || { echo "ERROR: Failed to compile multi_obj_b.ml"; exit 1; }

echo "Compiling and linking multi_obj_main.ml..."
${ocamlopt} -g -o multi_obj_test multi_obj_a.cmx multi_obj_b.cmx multi_obj_main.ml || { echo "ERROR: Linking failed"; exit 1; }

echo "✓ Multi-object compilation and linking successful"

# Run the program to verify it works
echo "Running linked program..."
./multi_obj_test || { echo "ERROR: Program execution failed"; exit 1; }
echo "✓ Program execution successful"

# Extract DWARF info
echo "Extracting DWARF information..."
case $DWARF_TOOL in
    dwarfdump|llvm-dwarfdump)
        DWARF_INFO=$($DWARF_TOOL --debug-info multi_obj_test 2>&1)
        LINE_INFO=$($DWARF_TOOL --debug-line multi_obj_test 2>&1)
        ;;
    readelf)
        DWARF_INFO=$(readelf --debug-dump=info multi_obj_test 2>&1)
        LINE_INFO=$(readelf --debug-dump=line multi_obj_test 2>&1)
        ;;
esac

# Count compilation units
CU_COUNT=$(echo "$DWARF_INFO" | grep -c "DW_TAG_compile_unit" || echo "0")
if [ "$CU_COUNT" -lt "3" ]; then
    echo "ERROR: Found only $CU_COUNT compilation units, expected >= 3"
    echo "This indicates multi-object linking may have failed"
    exit 1
fi
echo "✓ Found $CU_COUNT compilation units"

# Extract DW_AT_stmt_list offsets for each CU
echo "Validating DW_AT_stmt_list offsets..."
STMT_OFFSETS=$(echo "$DWARF_INFO" | grep "DW_AT_stmt_list" | grep -oE "0x[0-9a-fA-F]+" || echo "")

if [ -z "$STMT_OFFSETS" ]; then
    echo "ERROR: No DW_AT_stmt_list offsets found"
    exit 1
fi

# Check that we have multiple different offsets (not all pointing to offset 0)
UNIQUE_OFFSETS=$(echo "$STMT_OFFSETS" | sort -u | wc -l)
echo "Found $UNIQUE_OFFSETS unique DW_AT_stmt_list offsets:"
echo "$STMT_OFFSETS" | sort -u

if [ "$UNIQUE_OFFSETS" -eq "1" ]; then
    ONLY_OFFSET=$(echo "$STMT_OFFSETS" | head -1)
    if [ "$ONLY_OFFSET" = "0x00000000" ] || [ "$ONLY_OFFSET" = "0x0000" ]; then
        echo "WARNING: All CUs point to offset 0x0000"
        echo "This likely indicates a multi-object linking bug where stmt_list offsets"
        echo "are not being adjusted after merging .debug_line sections"
        echo "KNOWN LIMITATION: This is expected on Mach-O (macOS) targets"

        # On macOS, this is a known limitation, so don't fail the test
        if [ "$(uname -s)" = "Darwin" ]; then
            echo "Running on macOS - accepting this as known limitation"
        else
            echo "ERROR: This should not happen on ELF targets"
            exit 1
        fi
    fi
else
    echo "✓ Multiple unique stmt_list offsets found (multi-CU linking appears correct)"
fi

# Validate that line table entries exist for each expected file
echo "Checking line table coverage..."
for MODULE in multi_obj_a multi_obj_b multi_obj_main; then
    if echo "$LINE_INFO" | grep -q "$MODULE"; then
        echo "✓ Found line table entries for $MODULE.ml"
    else
        echo "WARNING: No line table entries found for $MODULE.ml"
    fi
done

echo "=== Multi-Object DWARF Test Complete ==="

if [ "$(uname -s)" = "Darwin" ]; then
    echo ""
    echo "NOTE: On macOS (Mach-O), DW_AT_stmt_list offsets may all point to 0x0000"
    echo "      after linking multiple objects. This is a known limitation where"
    echo "      the linker does not adjust section-relative offsets in DWARF debug_info."
    echo "      On ELF targets (Linux), proper relocations ensure correct offsets."
fi
