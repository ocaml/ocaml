#!/bin/bash
# Validate that local variable names are preserved in DWARF

set -e

TESTFILE="${1:-local_variables}"
COMPILER="${2:-../../ocamlopt.opt}"
ARCH="${3:-$(uname -m)}"

echo "=== DWARF Local Variable Name Validation ==="
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

# Test 1: Check for local variable "sum" in compute_sum function
echo "Test 1: Local variable 'sum' in compute_sum..."
case $DWARF_TOOL in
    readelf)
        if readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -A 50 "compute_sum" | grep -q 'DW_AT_name.*: sum$'; then
            echo "✓ Found local variable 'sum'"
        else
            echo "✗ FAILED: Local variable 'sum' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -A 50 "compute_sum" | grep -q 'DW_AT_name.*(\"sum\")'; then
            echo "✓ Found local variable 'sum'"
        else
            echo "✗ FAILED: Local variable 'sum' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
esac

# Test 2: Check for local variable "doubled" in compute_sum function
echo "Test 2: Local variable 'doubled' in compute_sum..."
case $DWARF_TOOL in
    readelf)
        if readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -A 50 "compute_sum" | grep -q 'DW_AT_name.*: doubled$'; then
            echo "✓ Found local variable 'doubled'"
        else
            echo "✗ FAILED: Local variable 'doubled' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -A 50 "compute_sum" | grep -q 'DW_AT_name.*(\"doubled\")'; then
            echo "✓ Found local variable 'doubled'"
        else
            echo "✗ FAILED: Local variable 'doubled' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
esac

# Test 3: Check for local variable "temp1" in process_values function
echo "Test 3: Local variable 'temp1' in process_values..."
case $DWARF_TOOL in
    readelf)
        if readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -A 50 "process_values" | grep -q 'DW_AT_name.*: temp1$'; then
            echo "✓ Found local variable 'temp1'"
        else
            echo "✗ FAILED: Local variable 'temp1' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -A 50 "process_values" | grep -q 'DW_AT_name.*(\"temp1\")'; then
            echo "✓ Found local variable 'temp1'"
        else
            echo "✗ FAILED: Local variable 'temp1' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
esac

# Test 4: Check for parameters in compute_sum (x, y)
echo "Test 4: Parameters 'x' and 'y' in compute_sum..."
case $DWARF_TOOL in
    readelf)
        if readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -A 50 "compute_sum" | grep -q 'DW_AT_name.*: x$' && \
           readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -A 50 "compute_sum" | grep -q 'DW_AT_name.*: y$'; then
            echo "✓ Found parameters 'x' and 'y'"
        else
            echo "✗ FAILED: Parameters 'x' and 'y' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
    dwarfdump|llvm-dwarfdump)
        if $DWARF_TOOL ${BINARY} 2>/dev/null | grep -A 50 "compute_sum" | grep -q 'DW_AT_name.*(\"x\")' && \
           $DWARF_TOOL ${BINARY} 2>/dev/null | grep -A 50 "compute_sum" | grep -q 'DW_AT_name.*(\"y\")'; then
            echo "✓ Found parameters 'x' and 'y'"
        else
            echo "✗ FAILED: Parameters 'x' and 'y' not found"
            FAILURES=$((FAILURES + 1))
        fi
        ;;
esac

echo
echo "=== Test Summary ==="
if [ "$FAILURES" -eq "0" ]; then
    echo "✓ All tests passed!"
    echo
    echo "SUCCESS: Local variable names are preserved in DWARF."
    echo "Both parameters and local let bindings appear with their source names."
    exit 0
else
    echo "✗ $FAILURES test(s) failed"
    exit 1
fi
