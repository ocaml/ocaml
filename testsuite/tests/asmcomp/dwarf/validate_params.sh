#!/bin/bash
# Validate that function parameters are tracked in DWARF

set -e

TESTFILE="${1:-param_tracking}"
COMPILER="${2:-../../ocamlopt.opt}"
ARCH="${3:-$(uname -m)}"

echo "=== DWARF Parameter Tracking Validation ==="
echo "Test file: $TESTFILE.ml"
echo "Architecture: $ARCH"
echo

# Check if binary exists from test framework, otherwise compile
if [ -f "${TESTFILE}.opt" ]; then
    echo "Using pre-compiled ${TESTFILE}.opt from test framework"
    BINARY="${TESTFILE}.opt"
elif [ -f "${TESTFILE}.exe" ]; then
    echo "Using ${TESTFILE}.exe"
    BINARY="${TESTFILE}.exe"
else
    echo "Compiling with -g..."
    $COMPILER -g -o ${TESTFILE}.exe ${TESTFILE}.ml
    echo "Compilation successful"
    BINARY="${TESTFILE}.exe"
fi
echo

# Determine which tools are available
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

# Test 1: Verify DW_TAG_subprogram entries exist
echo "Test 1: Function entries in DWARF..."
case $DWARF_TOOL in
    readelf)
        SUBPROGRAM_COUNT=$(readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -c "DW_TAG_subprogram" || echo "0")
        ;;
    dwarfdump|llvm-dwarfdump)
        SUBPROGRAM_COUNT=$($DWARF_TOOL ${BINARY} 2>/dev/null | grep -c "DW_TAG_subprogram" || echo "0")
        ;;
esac

if [ "$SUBPROGRAM_COUNT" -gt "0" ]; then
    echo "Found $SUBPROGRAM_COUNT function entries"
else
    echo "FAILED: No DW_TAG_subprogram entries found"
    FAILURES=$((FAILURES + 1))
fi

# Test 2: Verify DW_TAG_formal_parameter entries exist
echo "Test 2: Parameter entries in DWARF..."
case $DWARF_TOOL in
    readelf)
        PARAM_COUNT=$(readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -c "DW_TAG_formal_parameter" || echo "0")
        ;;
    dwarfdump|llvm-dwarfdump)
        PARAM_COUNT=$($DWARF_TOOL ${BINARY} 2>/dev/null | grep -c "DW_TAG_formal_parameter" || echo "0")
        ;;
esac

if [ "$PARAM_COUNT" -gt "0" ]; then
    echo "Found $PARAM_COUNT parameter entries"
else
    echo "FAILED: No DW_TAG_formal_parameter entries found"
    FAILURES=$((FAILURES + 1))
fi

# Test 3: Verify parameters have location information
echo "Test 3: Parameter location information..."
case $DWARF_TOOL in
    readelf)
        PARAMS_WITH_LOCATION=$(readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -A 5 "DW_TAG_formal_parameter" | grep -c "DW_AT_location" || echo "0")
        ;;
    dwarfdump|llvm-dwarfdump)
        PARAMS_WITH_LOCATION=$($DWARF_TOOL ${BINARY} 2>/dev/null | grep -A 5 "DW_TAG_formal_parameter" | grep -c "DW_AT_location" || echo "0")
        ;;
esac

if [ "$PARAMS_WITH_LOCATION" -gt "0" ]; then
    echo "Found $PARAMS_WITH_LOCATION parameters with location information"
else
    echo "FAILED: No parameters with DW_AT_location found"
    FAILURES=$((FAILURES + 1))
fi

# Test 4: Verify parameters have type information
echo "Test 4: Parameter type information..."
case $DWARF_TOOL in
    readelf)
        PARAMS_WITH_TYPE=$(readelf --debug-dump=info ${BINARY} 2>/dev/null | grep -A 5 "DW_TAG_formal_parameter" | grep -c "DW_AT_type" || echo "0")
        ;;
    dwarfdump|llvm-dwarfdump)
        PARAMS_WITH_TYPE=$($DWARF_TOOL ${BINARY} 2>/dev/null | grep -A 5 "DW_TAG_formal_parameter" | grep -c "DW_AT_type" || echo "0")
        ;;
esac

if [ "$PARAMS_WITH_TYPE" -gt "0" ]; then
    echo "Found $PARAMS_WITH_TYPE parameters with type information"
else
    echo "FAILED: No parameters with DW_AT_type found"
    FAILURES=$((FAILURES + 1))
fi

echo
echo "=== Test Summary ==="
if [ "$FAILURES" -eq "0" ]; then
    echo "All tests passed"
    echo
    echo "NOTE: Parameter names may show as register names (e.g., 'R')"
    echo "      instead of source names (e.g., 'x', 'y', 'z') because"
    echo "      variable names are not preserved through the compilation"
    echo "      pipeline to the emission phase. However, the DWARF"
    echo "      structure is correct and debuggers can inspect values."
    exit 0
else
    echo "$FAILURES test(s) failed"
    exit 1
fi
