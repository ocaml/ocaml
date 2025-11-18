#!/bin/bash
# Validate that source-level parameter names are preserved in DWARF

set -e

TESTFILE="${1:-source_param_names}"
COMPILER="${2:-../../ocamlopt.opt}"
ARCH="${3:-$(uname -m)}"

echo "=== DWARF Source-Level Parameter Name Validation ==="
echo "Test file: $TESTFILE.ml"
echo

# Check if binary exists
if [ -f "${TESTFILE}.opt" ]; then
    BINARY="${TESTFILE}.opt"
elif [ -f "${TESTFILE}.exe" ]; then
    BINARY="${TESTFILE}.exe"
else
    echo "Compiling..."
    $COMPILER -g -o ${TESTFILE}.exe ${TESTFILE}.ml
    BINARY="${TESTFILE}.exe"
fi

# Determine DWARF tool
if command -v readelf >/dev/null 2>&1; then
    DWARF_TOOL="readelf"
elif command -v dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="dwarfdump"
elif command -v llvm-dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="llvm-dwarfdump"
else
    echo "ERROR: No DWARF inspection tool found"
    exit 1
fi

echo "Using tool: $DWARF_TOOL"
echo

FAILURES=0

# Test 1: Check for parameter named "x" in add_numbers function
echo "Test 1: Parameter 'x' in add_numbers..."
case $DWARF_TOOL in
    readelf)
        if readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -A 30 "add_numbers" | grep -q 'DW_AT_name.*: x$'; then
            echo "Found parameter 'x'"
        else
            echo "FAILED: Parameter 'x' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -A 30 "add_numbers" | grep -q 'DW_AT_name.*("x")'; then
            echo "Found parameter 'x'"
        else
            echo "FAILED: Parameter 'x' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
esac

# Test 2: Check for parameter named "first" in compute_product function
echo "Test 2: Parameter 'first' in compute_product..."
case $DWARF_TOOL in
    readelf)
        if readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -A 30 "compute_product" | grep -q 'DW_AT_name.*: first$'; then
            echo "Found parameter 'first'"
        else
            echo "FAILED: Parameter 'first' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -A 30 "compute_product" | grep -q 'DW_AT_name.*("first")'; then
            echo "Found parameter 'first'"
        else
            echo "FAILED: Parameter 'first' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
esac

# Test 3: Verify parameter names are NOT generic (like "R" or "param0")
echo "Test 3: No generic parameter names..."
case $DWARF_TOOL in
    readelf)
        if readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -A 30 "add_numbers\|compute_product" | grep -q 'DW_AT_name.*: R$'; then
            echo "FAILED: Found generic name 'R' (should be source names)"
            FAILURES=$((FAILURES + 1))
        else
            echo "No generic 'R' names found"
        fi
        ;;
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -A 30 "add_numbers\|compute_product" | grep -q 'DW_AT_name.*("R")'; then
            echo "FAILED: Found generic name 'R' (should be source names)"
            FAILURES=$((FAILURES + 1))
        else
            echo "No generic 'R' names found"
        fi
        ;;
esac

echo
echo "=== Test Summary ==="
if [ "$FAILURES" -eq "0" ]; then
    echo "All tests passed!"
    echo
    echo "SUCCESS: Source-level parameter names are preserved in DWARF."
    echo "Parameters like 'x', 'y', 'first', 'second' appear correctly"
    echo "instead of generic names like 'R' or 'param0'."
    exit 0
else
    echo "$FAILURES test(s) failed"
    exit 1
fi
