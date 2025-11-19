"""LLDB helpers for inspecting OCaml values via DWARF (no external tools)."""

from __future__ import annotations

import dataclasses
import struct
from typing import Dict, List, Optional, Sequence, Tuple

import lldb

OCAML_CATEGORY = "OCaml"
MAX_LIST_ELEMENTS = 32
MAX_TUPLE_ELEMENTS = 16
MAX_STRING_BYTES = 256

DW_TAG_COMPILE_UNIT = 0x11
DW_TAG_SUBPROGRAM = 0x2E
DW_TAG_FORMAL_PARAMETER = 0x05
DW_TAG_VARIABLE = 0x34
DW_TAG_POINTER_TYPE = 0x0F
DW_TAG_BASE_TYPE = 0x24
DW_TAG_TYPEDEF = 0x16
DW_TAG_STRUCTURE_TYPE = 0x13
DW_TAG_UNION_TYPE = 0x17
DW_TAG_ENUMERATION_TYPE = 0x04

DW_AT_NAME = 0x03
DW_AT_LOW_PC = 0x11
DW_AT_HIGH_PC = 0x12
DW_AT_LOCATION = 0x02
DW_AT_STR_OFFSETS_BASE = 0x72  # DWARF v5 str offsets base (OCaml toolchain)
DW_AT_ADDR_BASE = 0x98
DW_AT_TYPE = 0x49
DW_AT_LOCLISTS_BASE = 0x8C

DW_FORM_ADDR = 0x01
DW_FORM_BLOCK2 = 0x03
DW_FORM_BLOCK4 = 0x04
DW_FORM_DATA2 = 0x05
DW_FORM_DATA4 = 0x06
DW_FORM_DATA8 = 0x07
DW_FORM_STRING = 0x08
DW_FORM_BLOCK = 0x09
DW_FORM_BLOCK1 = 0x0A
DW_FORM_DATA1 = 0x0B
DW_FORM_FLAG = 0x0C
DW_FORM_SDATA = 0x0D
DW_FORM_STRP = 0x0E
DW_FORM_UDATA = 0x0F
DW_FORM_REF_ADDR = 0x10
DW_FORM_REF1 = 0x11
DW_FORM_REF2 = 0x12
DW_FORM_REF4 = 0x13
DW_FORM_REF8 = 0x14
DW_FORM_REF_UDATA = 0x15
DW_FORM_SEC_OFFSET = 0x17
DW_FORM_EXPRLOC = 0x18
DW_FORM_FLAG_PRESENT = 0x19
DW_FORM_STRX = 0x1A
DW_FORM_ADDRX = 0x1B
DW_FORM_STRX1 = 0x25
DW_FORM_STRX2 = 0x26
DW_FORM_STRX3 = 0x27
DW_FORM_STRX4 = 0x28
DW_FORM_ADDRX1 = 0x29
DW_FORM_ADDRX2 = 0x2A
DW_FORM_ADDRX3 = 0x2B
DW_FORM_ADDRX4 = 0x2C
DW_FORM_IMPLICIT_CONST = 0x21
DW_FORM_LOCLISTX = 0x22

DW_LLE_END_OF_LIST = 0x00
DW_LLE_BASE_ADDRESSX = 0x01
DW_LLE_STARTX_ENDX = 0x02
DW_LLE_STARTX_LENGTH = 0x03
DW_LLE_OFFSET_PAIR = 0x04
DW_LLE_DEFAULT_LOCATION = 0x05
DW_LLE_BASE_ADDRESS = 0x06
DW_LLE_START_END = 0x07
DW_LLE_START_LENGTH = 0x08

DW_OP_REG0 = 0x50
DW_OP_REGX = 0x90
DW_OP_FBREG = 0x91
DW_OP_CONSTS = 0x11


class _ByteReader:
    def __init__(self, data: bytes, offset: int = 0):
        self._data = data
        self.offset = offset

    def remaining(self) -> int:
        return max(0, len(self._data) - self.offset)

    def read_u8(self) -> int:
        if self.offset >= len(self._data):
            raise ValueError("read past end")
        value = self._data[self.offset]
        self.offset += 1
        return value

    def read_u16(self) -> int:
        start = self.offset
        self.offset += 2
        if self.offset > len(self._data):
            raise ValueError("read past end")
        return int.from_bytes(self._data[start:self.offset], "little")

    def read_u32(self) -> int:
        start = self.offset
        self.offset += 4
        if self.offset > len(self._data):
            raise ValueError("read past end")
        return int.from_bytes(self._data[start:self.offset], "little")

    def read_u64(self) -> int:
        start = self.offset
        self.offset += 8
        if self.offset > len(self._data):
            raise ValueError("read past end")
        return int.from_bytes(self._data[start:self.offset], "little")

    def read_address(self, size: int) -> int:
        if size == 4:
            return self.read_u32()
        if size == 8:
            return self.read_u64()
        start = self.offset
        self.offset += size
        if self.offset > len(self._data):
            raise ValueError("read past end")
        return int.from_bytes(self._data[start:self.offset], "little")

    def read_bytes(self, size: int) -> bytes:
        start = self.offset
        self.offset += size
        if self.offset > len(self._data):
            raise ValueError("read past end")
        return self._data[start:self.offset]

    def read_cstring(self) -> str:
        end = self._data.find(b"\x00", self.offset)
        if end < 0:
            raise ValueError("unterminated string")
        string = self._data[self.offset:end]
        self.offset = end + 1
        return string.decode("utf-8", errors="ignore")

    def read_uleb128(self) -> int:
        result = 0
        shift = 0
        while True:
            byte = self.read_u8()
            result |= (byte & 0x7F) << shift
            if byte & 0x80 == 0:
                break
            shift += 7
        return result

    def read_sleb128(self) -> int:
        result = 0
        shift = 0
        while True:
            byte = self.read_u8()
            result |= (byte & 0x7F) << shift
            shift += 7
            if byte & 0x80 == 0:
                if byte & 0x40:
                    result |= -1 << shift
                break
        return result


@dataclasses.dataclass
class _AbbrevAttribute:
    attr: int
    form: int
    implicit_const: Optional[int] = None


@dataclasses.dataclass
class _AbbrevEntry:
    tag: int
    has_children: bool
    attributes: Sequence[_AbbrevAttribute]


@dataclasses.dataclass
class _VariableLocation:
    kind: str
    value: int


@dataclasses.dataclass
class _DWARFVariable:
    name: str
    is_param: bool
    type_name: Optional[str] = None
    location_expr: Optional[bytes] = None
    loclist_offset: Optional[int] = None


@dataclasses.dataclass
class _TypeEntry:
    tag: int
    name: Optional[str] = None
    type_ref: Optional[int] = None


@dataclasses.dataclass
class _UnitContext:
    addr_size: int
    version: int
    str_offsets_base: Optional[int] = None
    addr_base: Optional[int] = None
    loclists_base: Optional[int] = None
    cu_offset: int = 0


@dataclasses.dataclass
class _LocListEntry:
    start: int
    end: int
    expr: bytes


@dataclasses.dataclass
class _DWARFFunction:
    name: str
    low_pc: int
    high_pc: int
    context: _UnitContext
    variables: List[_DWARFVariable] = dataclasses.field(default_factory=list)

    def contains(self, address: int) -> bool:
        return self.low_pc <= address < self.high_pc

    def add_variable(self, var: _DWARFVariable) -> None:
        self.variables.append(var)


@dataclasses.dataclass
class _OCamlValueDesc:
    display: str
    runtime_type: str


class _DWARFModuleParser:
    def __init__(self, module: lldb.SBModule):
        self.module = module
        self.functions: List[_DWARFFunction] = []
        self._abbrev_tables: Dict[int, Dict[int, _AbbrevEntry]] = {}
        self._debug_info = _read_section_bytes(module, ["__debug_info", ".debug_info"]) or b""
        self._debug_abbrev = _read_section_bytes(module, ["__debug_abbrev", ".debug_abbrev"]) or b""
        self._str_offsets = _read_section_bytes(module, ["__debug_str_offs", ".debug_str_offsets"]) or b""
        self._debug_str = _read_section_bytes(module, ["__debug_str", ".debug_str"]) or b""
        self._debug_loclists = _read_section_bytes(module, ["__debug_loclists", ".debug_loclists"]) or b""
        self._debug_loc = _read_section_bytes(module, ["__debug_loc", ".debug_loc"]) or b""
        self._debug_addr = _read_section_bytes(module, ["__debug_addr", ".debug_addr"]) or b""
        self._type_entries: Dict[int, _TypeEntry] = {}
        self._pending_type_links: List[Tuple[_DWARFVariable, int]] = []
        self._addr_tables: Dict[int, List[int]] = {}
        self._loclists_contributions: Dict[int, Tuple[int, List[int]]] = {}
        self._loclist_cache: Dict[int, List[_LocListEntry]] = {}
        if not self._debug_info or not self._debug_abbrev:
            return
        reader = _ByteReader(self._debug_info)
        while reader.remaining() > 4:
            unit_start = reader.offset
            unit_length = reader.read_u32()
            if unit_length == 0 or unit_start + 4 + unit_length > len(self._debug_info):
                break
            unit_end = reader.offset + unit_length
            version = reader.read_u16()
            if version >= 5:
                unit_type = reader.read_u8()
                addr_size = reader.read_u8()
                abbrev_offset = reader.read_u32()
            else:
                abbrev_offset = reader.read_u32()
                addr_size = reader.read_u8()
            context = _UnitContext(addr_size=addr_size, version=version, cu_offset=reader.offset)
            abbrev_table = self._get_abbrev_table(abbrev_offset)
            self._parse_cu(reader, unit_end, abbrev_table, context)
            reader.offset = unit_end
        self._resolve_pending_types()

    def _get_abbrev_table(self, offset: int) -> Dict[int, _AbbrevEntry]:
        table = self._abbrev_tables.get(offset)
        if table is not None:
            return table
        reader = _ByteReader(self._debug_abbrev, offset)
        entries: Dict[int, _AbbrevEntry] = {}
        while reader.remaining() > 0:
            code = reader.read_uleb128()
            if code == 0:
                break
            tag = reader.read_uleb128()
            has_children = reader.read_u8() != 0
            attrs: List[_AbbrevAttribute] = []
            while True:
                attr = reader.read_uleb128()
                form = reader.read_uleb128()
                if attr == 0 and form == 0:
                    break
                implicit = None
                if form == DW_FORM_IMPLICIT_CONST:
                    implicit = reader.read_sleb128()
                attrs.append(_AbbrevAttribute(attr, form, implicit))
            entries[code] = _AbbrevEntry(tag, has_children, attrs)
        self._abbrev_tables[offset] = entries
        return entries

    def _parse_cu(
        self,
        reader: _ByteReader,
        unit_end: int,
        abbrev_table: Dict[int, _AbbrevEntry],
        context: _UnitContext,
    ) -> None:
        if reader.offset >= unit_end:
            return
        die_offset = reader.offset
        code = reader.read_uleb128()
        if code == 0:
            return
        entry = abbrev_table.get(code)
        if not entry:
            return
        attrs = self._read_attributes(reader, entry, context)
        if DW_AT_STR_OFFSETS_BASE in attrs:
            context.str_offsets_base = attrs[DW_AT_STR_OFFSETS_BASE]
        if DW_AT_ADDR_BASE in attrs:
            context.addr_base = attrs[DW_AT_ADDR_BASE]
        if DW_AT_LOCLISTS_BASE in attrs:
            context.loclists_base = attrs[DW_AT_LOCLISTS_BASE]
        self._record_type_die(entry.tag, attrs, die_offset)
        if entry.has_children:
            self._parse_children(reader, unit_end, abbrev_table, context, None)

    def _parse_children(
        self,
        reader: _ByteReader,
        unit_end: int,
        abbrev_table: Dict[int, _AbbrevEntry],
        context: _UnitContext,
        current_function: Optional[_DWARFFunction],
    ) -> None:
        while reader.offset < unit_end:
            die_offset = reader.offset
            code = reader.read_uleb128()
            if code == 0:
                return
            entry = abbrev_table.get(code)
            if not entry:
                return
            attrs = self._read_attributes(reader, entry, context)
            self._record_type_die(entry.tag, attrs, die_offset)
            child_function = current_function
            if entry.tag == DW_TAG_SUBPROGRAM:
                name = _coerce_str(attrs.get(DW_AT_NAME))
                low_pc = attrs.get(DW_AT_LOW_PC)
                high_pc = attrs.get(DW_AT_HIGH_PC)
                if (
                    isinstance(name, str)
                    and name
                    and isinstance(low_pc, int)
                    and isinstance(high_pc, int)
                    and high_pc > low_pc
                ):
                    ctx_copy = _UnitContext(
                        addr_size=context.addr_size,
                        version=context.version,
                        str_offsets_base=context.str_offsets_base,
                        addr_base=context.addr_base,
                        loclists_base=context.loclists_base,
                        cu_offset=context.cu_offset,
                    )
                    fn = _DWARFFunction(name=name, low_pc=low_pc, high_pc=high_pc, context=ctx_copy)
                    self.functions.append(fn)
                    child_function = fn
                else:
                    child_function = None
            elif entry.tag in (DW_TAG_FORMAL_PARAMETER, DW_TAG_VARIABLE):
                if current_function and DW_AT_LOCATION in attrs:
                    name = _coerce_str(attrs.get(DW_AT_NAME))
                    type_ref = attrs.get(DW_AT_TYPE)
                    type_name = self._resolve_type(type_ref)
                    location_attr = attrs.get(DW_AT_LOCATION)
                    location_expr: Optional[bytes] = None
                    loclist_offset: Optional[int] = None
                    if isinstance(location_attr, bytes):
                        location_expr = location_attr
                    elif isinstance(location_attr, int):
                        loclist_offset = location_attr
                    if name:
                        var = _DWARFVariable(
                            name=name,
                            is_param=(entry.tag == DW_TAG_FORMAL_PARAMETER),
                            type_name=type_name,
                            location_expr=location_expr,
                            loclist_offset=loclist_offset,
                        )
                        current_function.add_variable(var)
                        if type_name is None and isinstance(type_ref, int):
                            self._pending_type_links.append((var, type_ref))
            if entry.has_children:
                self._parse_children(reader, unit_end, abbrev_table, context, child_function)

    def _read_attributes(
        self, reader: _ByteReader, entry: _AbbrevEntry, context: _UnitContext
    ) -> Dict[int, object]:
        attrs: Dict[int, object] = {}
        for spec in entry.attributes:
            if spec.form == DW_FORM_IMPLICIT_CONST:
                attrs[spec.attr] = spec.implicit_const
            else:
                attrs[spec.attr] = self._read_form(reader, spec.form, context)
        return attrs

    def _read_form(self, reader: _ByteReader, form: int, context: _UnitContext) -> object:
        addr_size = context.addr_size
        if form == DW_FORM_ADDR:
            return reader.read_u32() if addr_size == 4 else reader.read_u64()
        if form == DW_FORM_DATA1:
            return reader.read_u8()
        if form == DW_FORM_DATA2:
            return reader.read_u16()
        if form == DW_FORM_DATA4:
            return reader.read_u32()
        if form == DW_FORM_DATA8:
            return reader.read_u64()
        if form == DW_FORM_STRING:
            return reader.read_cstring()
        if form == DW_FORM_FLAG:
            return reader.read_u8() != 0
        if form == DW_FORM_FLAG_PRESENT:
            return True
        if form == DW_FORM_SDATA:
            return reader.read_sleb128()
        if form == DW_FORM_UDATA:
            return reader.read_uleb128()
        if form == DW_FORM_STRP:
            offset = reader.read_u32()
            return self._read_debug_str(offset)
        if form == DW_FORM_SEC_OFFSET:
            return reader.read_u32()
        if form == DW_FORM_LOCLISTX:
            index = reader.read_uleb128()
            return self._loclist_offset_from_index(context, index)
        if form == DW_FORM_EXPRLOC:
            length = reader.read_uleb128()
            return reader.read_bytes(length)
        if form == DW_FORM_BLOCK1:
            size = reader.read_u8()
            return reader.read_bytes(size)
        if form == DW_FORM_BLOCK2:
            size = reader.read_u16()
            return reader.read_bytes(size)
        if form == DW_FORM_BLOCK4:
            size = reader.read_u32()
            return reader.read_bytes(size)
        if form == DW_FORM_BLOCK:
            size = reader.read_uleb128()
            return reader.read_bytes(size)
        if form == DW_FORM_REF_ADDR:
            return reader.read_u32() if addr_size == 4 else reader.read_u64()
        if form == DW_FORM_REF1:
            return context.cu_offset + reader.read_u8()
        if form == DW_FORM_REF2:
            return context.cu_offset + reader.read_u16()
        if form == DW_FORM_REF4:
            return context.cu_offset + reader.read_u32()
        if form == DW_FORM_REF8:
            return context.cu_offset + reader.read_u64()
        if form == DW_FORM_REF_UDATA:
            return context.cu_offset + reader.read_uleb128()
        if form in (DW_FORM_STRX, DW_FORM_STRX1, DW_FORM_STRX2, DW_FORM_STRX3, DW_FORM_STRX4):
            if form == DW_FORM_STRX:
                index = reader.read_uleb128()
            elif form == DW_FORM_STRX1:
                index = reader.read_u8()
            elif form == DW_FORM_STRX2:
                index = reader.read_u16()
            elif form == DW_FORM_STRX3:
                index = int.from_bytes(reader.read_bytes(3), "little")
            else:
                index = reader.read_u32()
            return self._read_strx(index, context)
        if form in (DW_FORM_ADDRX, DW_FORM_ADDRX1, DW_FORM_ADDRX2, DW_FORM_ADDRX3, DW_FORM_ADDRX4):
            # Consume bytes even though we don't use the address table yet
            if form == DW_FORM_ADDRX:
                reader.read_uleb128()
            elif form == DW_FORM_ADDRX1:
                reader.read_u8()
            elif form == DW_FORM_ADDRX2:
                reader.read_u16()
            elif form == DW_FORM_ADDRX3:
                reader.read_bytes(3)
            else:
                reader.read_u32()
            return 0
        raise ValueError(f"Unsupported DWARF form {form}")

    def _read_strx(self, index: int, context: _UnitContext) -> str:
        base = context.str_offsets_base
        if base is None:
            return ""
        entry_size = 4
        offset = base + index * entry_size
        if offset + entry_size > len(self._str_offsets):
            return ""
        str_offset = int.from_bytes(
            self._str_offsets[offset : offset + entry_size], "little"
        )
        return self._read_debug_str(str_offset)

    def _read_debug_str(self, offset: int) -> str:
        if offset >= len(self._debug_str):
            return ""
        end = self._debug_str.find(b"\x00", offset)
        if end == -1:
            end = len(self._debug_str)
        return self._debug_str[offset:end].decode("utf-8", errors="ignore")

    def _decode_location(self, expr: bytes) -> Optional[_VariableLocation]:
        if not expr:
            return None
        reader = _ByteReader(expr)
        opcode = reader.read_u8()
        if DW_OP_REG0 <= opcode <= DW_OP_REG0 + 31:
            return _VariableLocation("reg", opcode - DW_OP_REG0)
        if opcode == DW_OP_REGX:
            return _VariableLocation("reg", reader.read_uleb128())
        if opcode == DW_OP_FBREG:
            return _VariableLocation("fbreg", reader.read_sleb128())
        if opcode == DW_OP_CONSTS:
            return _VariableLocation("const", reader.read_sleb128())
        return None

    def _get_location_expr(
        self, var: _DWARFVariable, context: _UnitContext, pc: int
    ) -> Optional[bytes]:
        if var.location_expr:
            return var.location_expr
        if var.loclist_offset is not None:
            entries = self._loclist_cache.get(var.loclist_offset)
            if entries is None:
                entries = self._parse_loclist_entries(var.loclist_offset, context)
                self._loclist_cache[var.loclist_offset] = entries or []
            if entries:
                for entry in entries:
                    if entry.start <= pc < entry.end:
                        return entry.expr
                # Fallback: locate default entry (range 0,0)
                for entry in entries:
                    if entry.start == 0 and entry.end == 0:
                        return entry.expr
        return None

    def _parse_loclist_entries(
        self, offset: int, context: _UnitContext
    ) -> Optional[List[_LocListEntry]]:
        if offset < len(self._debug_loclists):
            return self._parse_debug_loclists(offset, context)
        if offset < len(self._debug_loc):
            return self._parse_debug_loc(offset, context.addr_size)
        return None

    def _read_loc_expr(self, reader: _ByteReader) -> bytes:
        length = reader.read_uleb128()
        return reader.read_bytes(length)

    def _parse_debug_loclists(
        self, offset: int, context: _UnitContext
    ) -> Optional[List[_LocListEntry]]:
        if offset >= len(self._debug_loclists):
            return None
        reader = _ByteReader(self._debug_loclists, offset)
        entries: List[_LocListEntry] = []
        base = 0
        while reader.remaining() > 0:
            kind = reader.read_u8()
            if kind == DW_LLE_END_OF_LIST:
                break
            if kind == DW_LLE_BASE_ADDRESSX:
                base = self._addr_from_table(context, reader.read_uleb128())
                continue
            if kind == DW_LLE_BASE_ADDRESS:
                base = reader.read_address(context.addr_size)
                continue
            if kind == DW_LLE_DEFAULT_LOCATION:
                expr = self._read_loc_expr(reader)
                entries.append(_LocListEntry(0, 0, expr))
                continue
            if kind == DW_LLE_OFFSET_PAIR:
                start = base + reader.read_uleb128()
                end = base + reader.read_uleb128()
                expr = self._read_loc_expr(reader)
                entries.append(_LocListEntry(start, end, expr))
                continue
            if kind == DW_LLE_STARTX_ENDX:
                start = self._addr_from_table(context, reader.read_uleb128())
                end = self._addr_from_table(context, reader.read_uleb128())
                expr = self._read_loc_expr(reader)
                entries.append(_LocListEntry(start, end, expr))
                continue
            if kind == DW_LLE_STARTX_LENGTH:
                start = self._addr_from_table(context, reader.read_uleb128())
                length = reader.read_uleb128()
                expr = self._read_loc_expr(reader)
                entries.append(_LocListEntry(start, start + length, expr))
                continue
            if kind == DW_LLE_START_END:
                start = reader.read_address(context.addr_size)
                end = reader.read_address(context.addr_size)
                expr = self._read_loc_expr(reader)
                entries.append(_LocListEntry(start, end, expr))
                continue
            if kind == DW_LLE_START_LENGTH:
                start = reader.read_address(context.addr_size)
                length = reader.read_uleb128()
                expr = self._read_loc_expr(reader)
                entries.append(_LocListEntry(start, start + length, expr))
                continue
            # Unknown entry type – stop parsing to avoid corrupt reads.
            break
        return entries

    def _parse_debug_loc(
        self, offset: int, addr_size: int
    ) -> Optional[List[_LocListEntry]]:
        if offset >= len(self._debug_loc):
            return None
        reader = _ByteReader(self._debug_loc, offset)
        sentinel = (1 << (addr_size * 8)) - 1
        base = 0
        entries: List[_LocListEntry] = []
        while reader.remaining() >= addr_size * 2 + 2:
            start = reader.read_address(addr_size)
            end = reader.read_address(addr_size)
            if start == 0 and end == 0:
                break
            if start == sentinel:
                base = end
                continue
            length = reader.read_u16()
            if reader.remaining() < length:
                break
            expr = reader.read_bytes(length)
            lo = start if base == 0 else base + start
            hi = end if base == 0 else base + end
            entries.append(_LocListEntry(lo, hi, expr))
        return entries

    def _addr_from_table(self, context: _UnitContext, index: int) -> int:
        if context.addr_base is None or not self._debug_addr:
            return 0
        entries = self._addr_tables.get(context.addr_base)
        if entries is None:
            entries = self._parse_addr_table(context.addr_base, context.addr_size)
            self._addr_tables[context.addr_base] = entries
        if 0 <= index < len(entries):
            return entries[index]
        return 0

    def _parse_addr_table(self, base: int, addr_size: int) -> List[int]:
        reader = _ByteReader(self._debug_addr, base)
        length = reader.read_u32()
        contrib_end = reader.offset + length
        reader.read_u16()  # version
        addr_size_in_table = reader.read_u8()
        reader.read_u8()  # seg_size
        if addr_size_in_table != addr_size:
            addr_size = addr_size_in_table
        entries: List[int] = []
        while reader.offset < contrib_end:
            entries.append(int.from_bytes(reader.read_bytes(addr_size), "little"))
        return entries

    def _loclist_offset_from_index(self, context: _UnitContext, index: int) -> Optional[int]:
        base = context.loclists_base
        if base is None:
            return None
        contrib = self._loclists_contributions.get(base)
        if contrib is None:
            contrib = self._parse_loclists_contribution(context, base)
            if contrib is None:
                return None
            self._loclists_contributions[base] = contrib
        entries_base, offsets = contrib
        if 0 <= index < len(offsets):
            return entries_base + offsets[index]
        return None

    def _parse_loclists_contribution(
        self, context: _UnitContext, base: int
    ) -> Optional[Tuple[int, List[int]]]:
        if not self._debug_loclists or base >= len(self._debug_loclists):
            return None
        reader = _ByteReader(self._debug_loclists, base)
        if reader.remaining() < 4:
            return None
        length = reader.read_u32()
        is_dwarf64 = False
        if length == 0xFFFFFFFF:
            if reader.remaining() < 8:
                return None
            length = reader.read_u64()
            is_dwarf64 = True
        contrib_end = reader.offset + length
        if contrib_end > len(self._debug_loclists):
            contrib_end = len(self._debug_loclists)
        if reader.remaining() < 4:
            return None
        reader.read_u16()  # version
        addr_size_in_section = reader.read_u8()
        reader.read_u8()  # segment selector size
        if addr_size_in_section and addr_size_in_section != context.addr_size:
            context.addr_size = addr_size_in_section
        if reader.remaining() < 4:
            return None
        offset_entry_count = reader.read_u32()
        offsets: List[int] = []
        entry_size = 8 if is_dwarf64 else 4
        for _ in range(offset_entry_count):
            if reader.remaining() < entry_size:
                return None
            if entry_size == 4:
                offsets.append(reader.read_u32())
            else:
                offsets.append(reader.read_u64())
        entries_base = reader.offset
        if entries_base > contrib_end:
            return None
        return entries_base, offsets

    def _record_type_die(self, tag: int, attrs: Dict[int, object], die_offset: int) -> None:
        if tag in {
            DW_TAG_BASE_TYPE,
            DW_TAG_TYPEDEF,
            DW_TAG_STRUCTURE_TYPE,
            DW_TAG_UNION_TYPE,
            DW_TAG_ENUMERATION_TYPE,
            DW_TAG_POINTER_TYPE,
        }:
            name = _coerce_str(attrs.get(DW_AT_NAME))
            type_ref = attrs.get(DW_AT_TYPE)
            ref_value = type_ref if isinstance(type_ref, int) else None
            self._type_entries[die_offset] = _TypeEntry(
                tag=tag,
                name=name or None,
                type_ref=ref_value,
            )

    def _resolve_type(self, ref: object) -> Optional[str]:
        if isinstance(ref, int):
            return self._format_type_name(ref, set())
        return None

    def _resolve_pending_types(self) -> None:
        for var, ref in self._pending_type_links:
            name = self._resolve_type(ref)
            if name:
                var.type_name = name

    def _format_type_name(self, offset: int, seen: Optional[set]) -> Optional[str]:
        if seen is None:
            seen = set()
        if offset in seen:
            return None
        seen.add(offset)
        entry = self._type_entries.get(offset)
        if entry is None:
            return None
        if entry.tag == DW_TAG_BASE_TYPE:
            return entry.name or "base"
        if entry.tag == DW_TAG_TYPEDEF:
            if entry.name:
                return entry.name
            if entry.type_ref is not None:
                return self._format_type_name(entry.type_ref, seen)
            return None
        if entry.tag == DW_TAG_POINTER_TYPE:
            target = self._format_type_name(entry.type_ref, seen) if entry.type_ref is not None else None
            return f"{target}*" if target else "void*"
        if entry.tag == DW_TAG_STRUCTURE_TYPE:
            return entry.name or "struct"
        if entry.tag == DW_TAG_UNION_TYPE:
            return entry.name or "union"
        if entry.tag == DW_TAG_ENUMERATION_TYPE:
            return entry.name or "enum"
        return entry.name


def _coerce_str(value: object) -> str:
    return value if isinstance(value, str) else ""


def _read_section_bytes(module: lldb.SBModule, names: Sequence[str]) -> Optional[bytes]:
    for name in names:
        section = module.FindSection(name)
        if section and section.IsValid():
            data = section.GetSectionData()
            if data.IsValid():
                error = lldb.SBError()
                blob = data.ReadRawData(error, 0, section.GetByteSize())
                if not error.Fail():
                    return bytes(blob)
    dwarf = module.FindSection("__DWARF")
    if dwarf and dwarf.IsValid():
        for name in names:
            subsection = dwarf.FindSubSection(name)
            if subsection and subsection.IsValid():
                data = subsection.GetSectionData()
                if data.IsValid():
                    error = lldb.SBError()
                    blob = data.ReadRawData(error, 0, subsection.GetByteSize())
                    if not error.Fail():
                        return bytes(blob)
    return None


def _module_key(module: lldb.SBModule) -> str:
    spec = module.GetFileSpec()
    path = ""
    if spec and spec.IsValid():
        path = f"{spec.GetDirectory()}/{spec.GetFilename()}"
    uuid = module.GetUUIDString() or ""
    return f"{path}:{uuid}"


_DWARF_CACHE: Dict[str, Optional[_DWARFModuleParser]] = {}


def _get_dwarf_module(frame: lldb.SBFrame) -> Optional[_DWARFModuleParser]:
    module = frame.GetModule()
    if not module or not module.IsValid():
        return None
    key = _module_key(module)
    if key not in _DWARF_CACHE:
        try:
            _DWARF_CACHE[key] = _DWARFModuleParser(module)
        except Exception:
            _DWARF_CACHE[key] = None
    return _DWARF_CACHE[key]


class _Architecture:
    def __init__(self, triple: str):
        triple = triple.lower()
        if "arm64" in triple or "aarch64" in triple:
            self.kind = "arm64"
        elif "x86_64" in triple or "amd64" in triple:
            self.kind = "x86_64"
        else:
            self.kind = "unknown"

    def register_name(self, dwarf_reg: int) -> Optional[str]:
        if self.kind == "arm64":
            if dwarf_reg == 29:
                return "x29"
            if dwarf_reg == 31:
                return "sp"
            if 0 <= dwarf_reg <= 28:
                return f"x{dwarf_reg}"
            return None
        if self.kind == "x86_64":
            mapping = {
                0: "rax",
                1: "rdx",
                2: "rcx",
                3: "rbx",
                4: "rsi",
                5: "rdi",
                6: "rbp",
                7: "rsp",
                8: "r8",
                9: "r9",
                10: "r10",
                11: "r11",
                12: "r12",
                13: "r13",
                14: "r14",
                15: "r15",
            }
            return mapping.get(dwarf_reg)
        return None

    def frame_pointer(self) -> Optional[str]:
        if self.kind == "arm64":
            return "x29"
        if self.kind == "x86_64":
            return "rbp"
        return None


def _read_uint64(process, address):
    error = lldb.SBError()
    value = process.ReadUnsignedFromMemory(address, 8, error)
    if error.Fail():
        raise RuntimeError(error.GetCString())
    return value


def _register_unsigned(reg: lldb.SBValue) -> Optional[int]:
    if not reg or not reg.IsValid():
        return None
    value_str = reg.GetValue()
    if value_str:
        try:
            return int(value_str, 0)
        except ValueError:
            pass
    return reg.GetValueAsUnsigned()


def _read_float64(process, address: int) -> float:
    bits = _read_uint64(process, address)
    return struct.unpack("<d", struct.pack("<Q", bits))[0]


def _read_block_header(process, value: int) -> int:
    if value < 8:
        raise RuntimeError("invalid OCaml block address")
    return _read_uint64(process, value - 8)


def _describe_ocaml_value(
    process, value, depth=0, max_depth=4
) -> _OCamlValueDesc:
    if depth > max_depth:
        return _OCamlValueDesc("…", "depth")
    if value & 1:
        return _OCamlValueDesc(f"int({value >> 1})", "int")
    if value == 0:
        return _OCamlValueDesc("null", "immediate")
    try:
        header = _read_block_header(process, value)
    except RuntimeError as exc:
        return _OCamlValueDesc(f"0x{value:x} ({exc})", "unreadable")
    size = header >> 10
    tag = header & 0xFF
    if tag == 0:
        list_desc = _describe_list(process, value, depth, max_depth)
        if list_desc:
            return list_desc
        return _describe_tuple(process, value, size, depth, max_depth)
    if tag == 251:
        return _OCamlValueDesc(f"<abstract {size} words>", "abstract")
    if tag == 252:
        return _describe_string(process, value, size)
    if tag == 253:
        try:
            number = _read_float64(process, value + 8)
            return _OCamlValueDesc(f"float({number})", "float")
        except RuntimeError as exc:  # noqa: BLE001
            return _OCamlValueDesc(f"float<{exc}>", "float")
    if tag == 254:
        return _describe_float_array(process, value, size)
    if tag == 247:
        return _describe_closure(process, value, size)
    if tag == 248:
        return _OCamlValueDesc("<object>", "object")
    if tag == 246:
        return _OCamlValueDesc("<lazy>", "lazy")
    if tag == 245:
        return _OCamlValueDesc("<continuation>", "continuation")
    return _OCamlValueDesc(f"block(tag={tag}, size={size})", "block")


def _describe_tuple(process, value, size, depth, max_depth):
    if size == 0:
        return _OCamlValueDesc("()", "tuple[0]")
    limit = min(size, MAX_TUPLE_ELEMENTS)
    elems = []
    for i in range(limit):
        field_addr = value + 8 * i
        try:
            field_val = _read_uint64(process, field_addr)
        except RuntimeError as exc:  # noqa: BLE001
            elems.append(f"<{exc}>")
            continue
        elems.append(_describe_ocaml_value(process, field_val, depth + 1, max_depth).display)
    if size > MAX_TUPLE_ELEMENTS:
        elems.append("…")
    return _OCamlValueDesc(f"({', '.join(elems)})", f"tuple[{size}]")


def _describe_list(process, value, depth, max_depth) -> Optional[_OCamlValueDesc]:
    elems = []
    current = value
    steps = 0
    while current != 1 and current != 0 and steps < MAX_LIST_ELEMENTS:
        try:
            header = _read_block_header(process, current)
        except RuntimeError:
            return None
        tag = header & 0xFF
        size = header >> 10
        if tag != 0 or size != 2:
            return None
        try:
            head = _read_uint64(process, current)
            tail = _read_uint64(process, current + 8)
        except RuntimeError:
            return None
        elems.append(
            _describe_ocaml_value(process, head, depth + 1, max_depth).display
        )
        current = tail
        steps += 1
    if steps == 0:
        return None
    if current not in (0, 1):
        elems.append("…")
    return _OCamlValueDesc(f"[{'; '.join(elems)}]", "list")


def _describe_string(process, value, size) -> _OCamlValueDesc:
    length = min(size * 8, MAX_STRING_BYTES)
    error = lldb.SBError()
    data = process.ReadMemory(value, length, error)
    if error.Fail():
        return _OCamlValueDesc(f"string<{error.GetCString()}>", "string")
    terminator = data.find(b"\x00")
    visible = data[: terminator if terminator >= 0 else len(data)]
    suffix = "…" if terminator == -1 and len(data) == length else ""
    try:
        text = visible.decode("utf-8", errors="replace")
    except Exception:  # noqa: BLE001
        text = visible.decode("latin-1", errors="replace")
    return _OCamlValueDesc(f'"{text}{suffix}"', "string")


def _describe_float_array(process, value, size) -> _OCamlValueDesc:
    elems = []
    limit = min(size, MAX_TUPLE_ELEMENTS)
    for i in range(limit):
        addr = value + 8 * i
        try:
            elems.append(_read_float64(process, addr))
        except RuntimeError as exc:  # noqa: BLE001
            elems.append(f"<{exc}>")
            break
    display = ", ".join(str(elem) for elem in elems)
    if size > limit:
        display += ", …"
    return _OCamlValueDesc(f"floatarray[{size}]({display})", "float array")


def _describe_closure(process, value, size) -> _OCamlValueDesc:
    try:
        code = _read_uint64(process, value)
        info = _read_uint64(process, value + 8)
        arity = (info >> 56) & 0xFF
        env_words = max(0, size - 2)
        return _OCamlValueDesc(
            f"<closure arity={arity} env={env_words} code=0x{code:x}>", "closure"
        )
    except RuntimeError as exc:  # noqa: BLE001
        return _OCamlValueDesc(f"<closure {exc}>", "closure")


def _format_ocaml_value(process, value, depth=0, max_depth=4):
    return _describe_ocaml_value(process, value, depth, max_depth).display


class OCamlSyntheticChildren:
    def __init__(self, valobj, _dict):
        self.valobj = valobj
        self.children = []
        self._update()

    def _update(self):
        self.children.clear()
        process = self.valobj.GetProcess()
        value = self.valobj.GetValueAsUnsigned()
        if value & 1 or value == 0:
            return
        try:
            header = _read_uint64(process, value)
        except RuntimeError:
            return
        tag = header & 0xFF
        size = header >> 10
        if tag == 0:
            limit = min(size, MAX_TUPLE_ELEMENTS)
            for i in range(limit):
                field_addr = value + 8 * (i + 1)
                child = self.valobj.CreateChildAtOffset(
                    f"[{i}]", field_addr - value, self.valobj.GetType()
                )
                child.SetUnsignedValue(_read_uint64(process, field_addr))
                self.children.append(child)
        elif tag == 248:
            current = value
            idx = 0
            while current != 1 and idx < MAX_LIST_ELEMENTS:
                header = _read_uint64(process, current)
                if (header & 0xFF) != 248:
                    break
                head_addr = current + 8
                child = self.valobj.CreateChildAtOffset(
                    f"[{idx}]", head_addr - value, self.valobj.GetType()
                )
                child.SetUnsignedValue(_read_uint64(process, head_addr))
                self.children.append(child)
                current = _read_uint64(process, current + 16)
                idx += 1

    def num_children(self):
        return len(self.children)

    def get_child_at_index(self, index):
        if 0 <= index < len(self.children):
            return self.children[index]
        return None

    def get_child_index(self, name):
        for i, child in enumerate(self.children):
            if child.GetName() == name:
                return i
        return -1

    def update(self):
        self._update()
        return True


def _find_function(frame: lldb.SBFrame, parser: _DWARFModuleParser) -> Optional[_DWARFFunction]:
    target = frame.GetThread().GetProcess().GetTarget()
    address = frame.GetPCAddress().GetLoadAddress(target)
    for fn in parser.functions:
        if fn.contains(address):
            return fn
    return None


def _evaluate_location(
    frame: lldb.SBFrame,
    process: lldb.SBProcess,
    arch: _Architecture,
    location: Optional[_VariableLocation],
) -> Optional[int]:
    if location is None:
        return None
    if location.kind == "reg":
        reg_name = arch.register_name(location.value)
        if not reg_name:
            return None
        reg = frame.FindRegister(reg_name)
        if (not reg or not reg.IsValid()) and reg_name == "fp":
            reg = frame.FindRegister("x29")
        if not reg or not reg.IsValid():
            return None
        value = _register_unsigned(reg)
        return value
    if location.kind == "fbreg":
        fp_name = arch.frame_pointer()
        if not fp_name:
            return None
        reg = frame.FindRegister(fp_name)
        if (not reg or not reg.IsValid()) and fp_name == "fp":
            reg = frame.FindRegister("x29")
        if not reg or not reg.IsValid():
            return None
        base = _register_unsigned(reg)
        if base is None:
            return None
        address = base + location.value
        try:
            return _read_uint64(process, address)
        except RuntimeError:
            return None
    if location.kind == "const":
        return location.value
    return None


def _format_single_variable(
    frame: lldb.SBFrame,
    process: lldb.SBProcess,
    arch: _Architecture,
    parser: _DWARFModuleParser,
    func: _DWARFFunction,
    var: _DWARFVariable,
    pc: int,
) -> Optional[str]:
    expr = parser._get_location_expr(var, func.context, pc)
    if expr is None:
        return None
    location_desc = parser._decode_location(expr)
    value = _evaluate_location(frame, process, arch, location_desc)
    if value is None:
        return None
    desc = _describe_ocaml_value(process, value)
    kind = "param" if var.is_param else "local"
    type_str = var.type_name or "value"
    return (
        f"{var.name} ({kind}, dwarf={type_str}, runtime={desc.runtime_type}) = {desc.display}"
    )


def _fallback_expression(debugger: lldb.SBDebugger, command: str, result) -> None:
    interpreter = debugger.GetCommandInterpreter()
    fallback = lldb.SBCommandReturnObject()
    interpreter.HandleCommand(f"expression -- {command}", fallback)
    output = fallback.GetOutput()
    error = fallback.GetError()
    if output:
        result.AppendMessage(output.rstrip())
    if error:
        result.SetError(error.rstrip())


def ocaml_value_summary(valobj, _dict):
    try:
        process = valobj.GetProcess()
        value = valobj.GetValueAsUnsigned()
        return _format_ocaml_value(process, value)
    except Exception as exc:  # noqa: BLE001
        return f"<ocaml {exc}>"


def ocaml_vars(debugger, command, exe_ctx, result, _dict):
    del debugger, command
    frame = exe_ctx.frame

    def _print(msg: str):
        result.AppendMessage(msg)

    if not frame or not frame.IsValid():
        _print("No frame available")
        return
    parser = _get_dwarf_module(frame)
    if not parser:
        _print("DWARF data not available for this module")
        return
    func = _find_function(frame, parser)
    if not func:
        _print("No OCaml DWARF function found for this frame")
        return
    process = frame.GetThread().GetProcess()
    target = process.GetTarget()
    arch = _Architecture(target.GetTriple())
    pc = frame.GetPCAddress().GetLoadAddress(target)
    seen = 0
    for var in func.variables:
        formatted = _format_single_variable(frame, process, arch, parser, func, var, pc)
        if not formatted:
            continue
        _print(formatted)
        seen += 1
    if seen == 0:
        _print("No OCaml variables found")


def ocaml_print(debugger, command, exe_ctx, result, _dict):
    frame = exe_ctx.frame
    name = command.strip()
    if not frame or not frame.IsValid() or not name:
        result.AppendMessage("Usage: p <ocaml-variable>")
        return
    parser = _get_dwarf_module(frame)
    if not parser:
        _fallback_expression(debugger, command, result)
        return
    func = _find_function(frame, parser)
    if not func:
        _fallback_expression(debugger, command, result)
        return
    match = next((var for var in func.variables if var.name == name), None)
    if not match:
        _fallback_expression(debugger, command, result)
        return
    process = frame.GetThread().GetProcess()
    target = process.GetTarget()
    arch = _Architecture(target.GetTriple())
    pc = frame.GetPCAddress().GetLoadAddress(target)
    formatted = _format_single_variable(frame, process, arch, parser, func, match, pc)
    if formatted:
        result.AppendMessage(formatted)
    else:
        _fallback_expression(debugger, command, result)


def __lldb_init_module(debugger, _dict):
    debugger.HandleCommand("settings set interpreter.require-overwrite false")
    debugger.HandleCommand(
        f'type summary add -F {__name__}.ocaml_value_summary '
        '-x "^(unsigned )?long$" --category OCaml'
    )
    debugger.HandleCommand(
        f'type synthetic add -l {__name__}.OCamlSyntheticChildren '
        '-x "^(unsigned )?long$" --category OCaml'
    )
    debugger.HandleCommand(f"type category enable {OCAML_CATEGORY}")
    debugger.HandleCommand(
        f'command script add -f {__name__}.ocaml_vars ocaml_vars'
    )
    debugger.HandleCommand(
        f'command script add -f {__name__}.ocaml_print p'
    )
    print("OCaml LLDB helpers loaded (commands: ocaml_vars)")
