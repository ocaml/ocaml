#!/bin/bash
# Comprehensive DWARF inspection test
# Validates .debug_info, .debug_line, relocations, and typed offsets

set -e

TEST_NAME="$1"
COMPILER="$2"
ARCH="$3"

echo "=== Comprehensive DWARF Inspection Test ==="
echo "Test: $TEST_NAME"
echo "Compiler: $COMPILER"
echo "Architecture: $ARCH"

# Determine DWARF tool
if command -v dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="dwarfdump"
elif command -v llvm-dwarfdump >/dev/null 2>&1; then
    DWARF_TOOL="llvm-dwarfdump"
else
    echo "ERROR: No dwarfdump tool found"
    exit 1
fi

# Compile test program
echo "Compiling ${TEST_NAME}.ml..."
${COMPILER} -g -c ${TEST_NAME}.ml || { echo "ERROR: Compilation failed"; exit 1; }

echo "✓ Compilation successful"

# Check object file has DWARF sections
echo "Checking DWARF sections in object file..."
if command -v readelf >/dev/null 2>&1; then
    # Linux/ELF
    SECTIONS=$(readelf -S ${TEST_NAME}.o | grep -c "debug_" || echo "0")
elif command -v otool >/dev/null 2>&1; then
    # macOS/Mach-O
    SECTIONS=$(otool -l ${TEST_NAME}.o | grep -c "sectname __debug" || echo "0")
else
    echo "ERROR: No section inspection tool found"
    exit 1
fi

if [ "$SECTIONS" -lt "3" ]; then
    echo "ERROR: Found only $SECTIONS debug sections, expected at least 3"
    exit 1
fi
echo "✓ Found $SECTIONS debug sections"

# Validate .debug_info structure
echo "Validating .debug_info..."
DWARF_DUMP=$($DWARF_TOOL --debug-info ${TEST_NAME}.o 2>&1)

# Check for compilation unit
if ! echo "$DWARF_DUMP" | grep -q "DW_TAG_compile_unit"; then
    echo "ERROR: No DW_TAG_compile_unit found"
    exit 1
fi
echo "✓ Found DW_TAG_compile_unit"

# Validate DW_AT_language is NOT truncated (should be 0x8001, not 0x0001 or 0x0023)
LANG_LINE=$(echo "$DWARF_DUMP" | grep "DW_AT_language" | head -1)
if echo "$LANG_LINE" | grep -qi "fortran"; then
    echo "ERROR: DW_AT_language shows Fortran (0x0023), should be OCaml vendor extension (0x8001)"
    echo "Found: $LANG_LINE"
    exit 1
fi
echo "✓ DW_AT_language is not Fortran"

# Check DW_AT_stmt_list exists
if ! echo "$DWARF_DUMP" | grep -q "DW_AT_stmt_list"; then
    echo "ERROR: No DW_AT_stmt_list found"
    exit 1
fi
echo "✓ Found DW_AT_stmt_list"

# Validate subprogram entries
if ! echo "$DWARF_DUMP" | grep -q "DW_TAG_subprogram"; then
    echo "ERROR: No DW_TAG_subprogram found"
    exit 1
fi
echo "✓ Found DW_TAG_subprogram"

# Validate DW_AT_frame_base exists in subprograms
SUBPROGRAM_START=$(echo "$DWARF_DUMP" | grep -n "DW_TAG_subprogram" | head -1 | cut -d: -f1)
if [ -n "$SUBPROGRAM_START" ]; then
    SUBPROGRAM_SECTION=$(echo "$DWARF_DUMP" | tail -n +$SUBPROGRAM_START | head -20)
    if ! echo "$SUBPROGRAM_SECTION" | grep -q "DW_AT_frame_base"; then
        echo "ERROR: No DW_AT_frame_base in DW_TAG_subprogram"
        exit 1
    fi
    echo "✓ Found DW_AT_frame_base in subprogram"
fi

# Validate address size matches architecture
ADDR_SIZE_LINE=$(echo "$DWARF_DUMP" | grep "addr_size" | head -1)
if echo "$ARCH" | grep -qi "64"; then
    if ! echo "$ADDR_SIZE_LINE" | grep -q "0x08"; then
        echo "ERROR: Address size should be 8 bytes for 64-bit architecture"
        echo "Found: $ADDR_SIZE_LINE"
        exit 1
    fi
    echo "✓ Address size is 8 bytes (64-bit)"
elif echo "$ARCH" | grep -qi "32\|i386\|i686"; then
    if ! echo "$ADDR_SIZE_LINE" | grep -q "0x04"; then
        echo "ERROR: Address size should be 4 bytes for 32-bit architecture"
        echo "Found: $ADDR_SIZE_LINE"
        exit 1
    fi
    echo "✓ Address size is 4 bytes (32-bit)"
fi

# Validate .debug_line section
echo "Validating .debug_line..."
LINE_DUMP=$($DWARF_TOOL --debug-line ${TEST_NAME}.o 2>&1)

if ! echo "$LINE_DUMP" | grep -q "file_names\|File name"; then
    echo "ERROR: No file names found in .debug_line"
    exit 1
fi
echo "✓ Found file names in .debug_line"

# Validate relocations (if readelf available)
if command -v readelf >/dev/null 2>&1; then
    echo "Validating ELF relocations..."
    RELOCS=$(readelf -r ${TEST_NAME}.o 2>&1 || echo "")
    if echo "$RELOCS" | grep -q "debug"; then
        echo "✓ Found debug section relocations"
        # Check for section-relative relocations
        if echo "$RELOCS" | grep -qi "R_.*_32\|R_.*_64"; then
            echo "✓ Found address relocations in debug sections"
        fi
    fi
elif command -v otool >/dev/null 2>&1; then
    echo "Checking Mach-O relocations..."
    RELOCS=$(otool -r ${TEST_NAME}.o 2>&1 || echo "")
    if echo "$RELOCS" | grep -q "sect"; then
        echo "✓ Found section relocations"
    fi
fi

# Validate base types exist
if ! echo "$DWARF_DUMP" | grep -q "DW_TAG_base_type"; then
    echo "WARNING: No DW_TAG_base_type found (basic types missing)"
else
    echo "✓ Found DW_TAG_base_type (basic types present)"
fi

echo "=== DWARF Inspection Test PASSED ==="
