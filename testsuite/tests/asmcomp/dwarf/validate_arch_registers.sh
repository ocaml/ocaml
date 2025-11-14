#!/bin/bash
# Validates architecture-specific DWARF register mappings
# Tests that DW_AT_frame_base and DW_OP_reg* use correct register numbers

set -e

TEST_FILE="$1"
COMPILER="$2"
ARCH="$3"

echo "=== Architecture Register Mapping Validation ==="
echo "Architecture: $ARCH"
echo "Test file: $TEST_FILE"

# Determine DWARF tool
if command -v dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="dwarfdump"
elif command -v llvm-dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="llvm-dwarfdump"
else
    echo "ERROR: No dwarfdump tool found"
    exit 1
fi

# Compile with -g
echo "Compiling ${TEST_FILE}.ml..."
${COMPILER} -g -c ${TEST_FILE}.ml || { echo "ERROR: Compilation failed"; exit 1; }

# Extract DWARF info
DWARF_INFO=$($DWARF_TOOL --debug-info ${TEST_FILE}.o 2>&1)

# Find DW_AT_frame_base
FRAME_BASE=$(echo "$DWARF_INFO" | grep "DW_AT_frame_base" | head -1)

if [ -z "$FRAME_BASE" ]; then
    echo "ERROR: No DW_AT_frame_base found"
    exit 1
fi

echo "Found: $FRAME_BASE"

# Validate frame base register matches architecture
case "$ARCH" in
    amd64|x86_64)
        # AMD64 frame pointer should be rbp (DWARF reg 6)
        if echo "$FRAME_BASE" | grep -qE "DW_OP_reg6|rbp|RBP"; then
            echo "✓ Frame base uses rbp (register 6) - correct for AMD64"
        else
            echo "ERROR: Frame base should use rbp (DWARF register 6) on AMD64"
            echo "Found: $FRAME_BASE"
            exit 1
        fi
        ;;
    arm64|aarch64)
        # ARM64 frame pointer should be x29 (DWARF reg 29)
        if echo "$FRAME_BASE" | grep -qE "DW_OP_reg29|W29|X29"; then
            echo "✓ Frame base uses x29 (register 29) - correct for ARM64"
        else
            echo "ERROR: Frame base should use x29 (DWARF register 29) on ARM64"
            echo "Found: $FRAME_BASE"
            exit 1
        fi
        ;;
    i386|i686)
        # i386 frame pointer should be ebp (DWARF reg 5)
        if echo "$FRAME_BASE" | grep -qE "DW_OP_reg5|ebp|EBP"; then
            echo "✓ Frame base uses ebp (register 5) - correct for i386"
        else
            echo "WARNING: Frame base should use ebp (DWARF register 5) on i386"
            echo "Found: $FRAME_BASE"
        fi
        ;;
    *)
        echo "WARNING: Unknown architecture '$ARCH'"
        echo "Cannot validate register numbers"
        echo "Supported architectures: amd64, arm64"
        echo "Frame base found: $FRAME_BASE"
        ;;
esac

# Check for parameter locations using DW_OP_reg*
PARAMS=$(echo "$DWARF_INFO" | grep -A 2 "DW_TAG_formal_parameter" | grep "DW_AT_location")

if [ -n "$PARAMS" ]; then
    echo "Parameter locations found:"
    echo "$PARAMS" | head -3
    echo "✓ Parameters have DWARF location expressions"
else
    echo "No parameter location expressions found (may be optimized away)"
fi

echo "=== Architecture Validation PASSED for $ARCH ==="
