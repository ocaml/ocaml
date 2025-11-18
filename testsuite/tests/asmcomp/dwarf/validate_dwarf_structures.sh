#!/bin/bash
# Comprehensive DWARF structure validation test
# Validates that critical DWARF bugs are fixed by inspecting emitted debug info

set -e

TESTFILE="${1:-dwarf_validation}"
COMPILER="${2:-../../ocamlopt.opt}"
ARCH="${3:-$(uname -m)}"

echo "=== DWARF Structure Validation Test ==="
echo "Test file: $TESTFILE.ml"
echo "Compiler: $COMPILER"
echo "Architecture: $ARCH"
echo

# Check if binary exists from test framework, otherwise compile
if [ -f "${TESTFILE}.opt" ]; then
    echo "✓ Using pre-compiled ${TESTFILE}.opt from test framework"
    BINARY="${TESTFILE}.opt"
elif [ -f "${TESTFILE}.exe" ]; then
    echo "✓ Using ${TESTFILE}.exe"
    BINARY="${TESTFILE}.exe"
else
    echo "Compiling with -g..."
    $COMPILER -g -o ${TESTFILE}.exe ${TESTFILE}.ml
    echo "✓ Compilation successful"
    BINARY="${TESTFILE}.exe"
fi
echo

# Determine which tools are available
if command -v dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="dwarfdump"
elif command -v llvm-dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="llvm-dwarfdump"
elif command -v readelf >/dev/null 2>&1; then
    DWARF_TOOL="readelf"
else
    echo "ERROR: No DWARF inspection tool found (dwarfdump, llvm-dwarfdump, or readelf)"
    exit 1
fi
echo "Using tool: $DWARF_TOOL"
echo

FAILURES=0

# Test 1: Verify DW_AT_language is 0x8001 (DW_LANG_OCaml vendor extension)
echo "Test 1: DW_AT_language encoding..."
case $DWARF_TOOL in
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -q "DW_AT_language.*0x8001"; then
            echo "✓ DW_AT_language correctly encodes OCaml (0x8001)"
        else
            echo "✗ FAILED: DW_AT_language not 0x8001 (may be truncated to 0x01)"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
    readelf)
        if readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -q "DW_AT_language.*: 32769"; then
            echo "✓ DW_AT_language correctly encodes OCaml (32769 = 0x8001)"
        else
            echo "✗ FAILED: DW_AT_language not 32769/0x8001"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
esac

# Test 2: Verify DW_AT_frame_base is present in subprogram DIEs
echo "Test 2: DW_AT_frame_base in subprogram DIEs..."
case $DWARF_TOOL in
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -A20 "DW_TAG_subprogram" | grep -q "DW_AT_frame_base"; then
            echo "✓ DW_AT_frame_base present in subprogram DIEs"
        else
            echo "✗ FAILED: DW_AT_frame_base missing (DW_OP_fbreg will be undefined)"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
    readelf)
        if readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -A20 "DW_TAG_subprogram" | grep -q "DW_AT_frame_base"; then
            echo "✓ DW_AT_frame_base present in subprogram DIEs"
        else
            echo "✗ FAILED: DW_AT_frame_base missing"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
esac

# Test 3: Verify DW_TAG_variable entries exist
echo "Test 3: DW_TAG_variable entries..."
case $DWARF_TOOL in
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -q "DW_TAG_variable"; then
            echo "✓ DW_TAG_variable entries present"
        else
            echo "✗ FAILED: No DW_TAG_variable entries found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
    readelf)
        if readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -q "DW_TAG_variable"; then
            echo "✓ DW_TAG_variable entries present"
        else
            echo "✗ FAILED: No DW_TAG_variable entries found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
esac

# Test 4: Verify address size in CU header matches architecture
echo "Test 4: CU header address size..."
EXPECTED_SIZE=8
if [ "$ARCH" = "i386" ] || [ "$ARCH" = "i686" ] || echo "$ARCH" | grep -q "^armv7"; then
    EXPECTED_SIZE=4
fi

case $DWARF_TOOL in
    dwarfdump|llvm-dwarfdump)
        ACTUAL_SIZE=$($DWARF_TOOL ${BINARY} 2>/dev/null | grep -m1 "address_size" | sed 's/.*address_size = 0x\([0-9a-f]*\).*/\1/' | xargs printf "%d")
        if [ "$ACTUAL_SIZE" = "$EXPECTED_SIZE" ]; then
            echo "✓ Address size correct ($EXPECTED_SIZE bytes for $ARCH)"
        else
            echo "✗ FAILED: Address size is $ACTUAL_SIZE, expected $EXPECTED_SIZE for $ARCH"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
    readelf)
        # readelf shows address size in CU header
        ACTUAL_SIZE=$(readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -m1 "Address size" | awk '{print $3}')
        if [ "$ACTUAL_SIZE" = "$EXPECTED_SIZE" ]; then
            echo "✓ Address size correct ($EXPECTED_SIZE bytes for $ARCH)"
        else
            echo "✗ FAILED: Address size is $ACTUAL_SIZE, expected $EXPECTED_SIZE"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
esac

# Test 5: Verify DW_AT_stmt_list is present (when line info exists)
echo "Test 5: DW_AT_stmt_list in CU DIE..."
case $DWARF_TOOL in
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -m1 "DW_TAG_compile_unit" -A10 | grep -q "DW_AT_stmt_list"; then
            echo "✓ DW_AT_stmt_list present in CU DIE"
        else
            echo "⚠ WARNING: DW_AT_stmt_list not found (may be OK if no line data)"
        fi
        ;;
    readelf)
        if readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -m1 "DW_TAG_compile_unit" -A10 | grep -q "DW_AT_stmt_list"; then
            echo "✓ DW_AT_stmt_list present in CU DIE"
        else
            echo "⚠ WARNING: DW_AT_stmt_list not found"
        fi
        ;;
esac

# Test 6: Verify .debug_line section exists and is non-empty
echo "Test 6: Line number information..."
case $DWARF_TOOL in
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -q "debug_line"; then
            echo "✓ Line number information present"
        else
            echo "✗ FAILED: No line number information found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
    readelf)
        if readelf --debug-dump=line ${BINARY} 2>/dev/null | grep -q "File name"; then
            echo "✓ Line number information present"
        else
            echo "✗ FAILED: No line number information found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
esac

# Summary
echo
echo "=== Test Summary ==="
if [ $FAILURES -eq 0 ]; then
    echo "✓ All tests passed!"
    exit 0
else
    echo "✗ $FAILURES test(s) failed"
    exit 1
fi
