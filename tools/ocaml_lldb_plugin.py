"""LLDB helpers for inspecting OCaml values via DWARF (no external tools)."""

from __future__ import annotations

import dataclasses
from typing import Dict, List, Optional, Sequence

import lldb

OCAML_CATEGORY = "OCaml"
MAX_LIST_ELEMENTS = 32
MAX_TUPLE_ELEMENTS = 16

DW_TAG_COMPILE_UNIT = 0x11
DW_TAG_SUBPROGRAM = 0x2E
DW_TAG_FORMAL_PARAMETER = 0x05
DW_TAG_VARIABLE = 0x34

DW_AT_NAME = 0x03
DW_AT_LOW_PC = 0x11
DW_AT_HIGH_PC = 0x12
DW_AT_LOCATION = 0x02
DW_AT_STR_OFFSETS_BASE = 0x72  # DWARF v5 str offsets base (OCaml toolchain)
DW_AT_ADDR_BASE = 0x98

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
    location: Optional[_VariableLocation]


@dataclasses.dataclass
class _DWARFFunction:
    name: str
    low_pc: int
    high_pc: int
    variables: List[_DWARFVariable]

    def contains(self, address: int) -> bool:
        return self.low_pc <= address < self.high_pc

    def add_variable(self, var: _DWARFVariable) -> None:
        self.variables.append(var)


@dataclasses.dataclass
class _UnitContext:
    addr_size: int
    str_offsets_base: Optional[int] = None
    addr_base: Optional[int] = None


class _DWARFModuleParser:
    def __init__(self, module: lldb.SBModule):
        self.module = module
        self.functions: List[_DWARFFunction] = []
        self._abbrev_tables: Dict[int, Dict[int, _AbbrevEntry]] = {}
        self._debug_info = _read_section_bytes(module, ["__debug_info", ".debug_info"]) or b""
        self._debug_abbrev = _read_section_bytes(module, ["__debug_abbrev", ".debug_abbrev"]) or b""
        self._str_offsets = _read_section_bytes(module, ["__debug_str_offs", ".debug_str_offsets"]) or b""
        self._debug_str = _read_section_bytes(module, ["__debug_str", ".debug_str"]) or b""
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
            context = _UnitContext(addr_size=addr_size)
            abbrev_table = self._get_abbrev_table(abbrev_offset)
            self._parse_cu(reader, unit_end, abbrev_table, context)
            reader.offset = unit_end

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
            code = reader.read_uleb128()
            if code == 0:
                return
            entry = abbrev_table.get(code)
            if not entry:
                return
            attrs = self._read_attributes(reader, entry, context)
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
                    fn = _DWARFFunction(name=name, low_pc=low_pc, high_pc=high_pc, variables=[])
                    self.functions.append(fn)
                    child_function = fn
                else:
                    child_function = None
            elif entry.tag in (DW_TAG_FORMAL_PARAMETER, DW_TAG_VARIABLE):
                if current_function and DW_AT_LOCATION in attrs:
                    location_bytes = attrs[DW_AT_LOCATION]
                    location = None
                    if isinstance(location_bytes, bytes):
                        location = self._decode_location(location_bytes)
                    name = _coerce_str(attrs.get(DW_AT_NAME))
                    if name:
                        current_function.add_variable(
                            _DWARFVariable(
                                name=name,
                                is_param=(entry.tag == DW_TAG_FORMAL_PARAMETER),
                                location=location,
                            )
                        )
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
        if form in (DW_FORM_REF_ADDR, DW_FORM_REF4, DW_FORM_REF8):
            return reader.read_u32() if addr_size == 4 else reader.read_u64()
        if form == DW_FORM_REF1:
            return reader.read_u8()
        if form == DW_FORM_REF2:
            return reader.read_u16()
        if form == DW_FORM_REF4:
            return reader.read_u32()
        if form == DW_FORM_REF8:
            return reader.read_u64()
        if form == DW_FORM_REF_UDATA:
            return reader.read_uleb128()
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
                return "fp"
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
            return "fp"
        if self.kind == "x86_64":
            return "rbp"
        return None


def _read_uint64(process, address):
    error = lldb.SBError()
    value = process.ReadUnsignedFromMemory(address, 8, error)
    if error.Fail():
        raise RuntimeError(error.GetCString())
    return value


def _format_ocaml_value(process, value, depth=0, max_depth=4):
    if depth > max_depth:
        return "…"
    if value & 1:
        return f"int({value >> 1})"
    if value == 0:
        return "null"
    try:
        header = _read_uint64(process, value)
    except RuntimeError as exc:
        return f"0x{value:x} ({exc})"
    size = header >> 10
    tag = header & 0xFF
    if tag == 0:
        elems = []
        limit = min(size, MAX_TUPLE_ELEMENTS)
        for i in range(limit):
            field_addr = value + 8 * (i + 1)
            field_val = _read_uint64(process, field_addr)
            elems.append(_format_ocaml_value(process, field_val, depth + 1))
        if size > MAX_TUPLE_ELEMENTS:
            elems.append("…")
        return f"({', '.join(elems)})"
    if tag == 248:
        elems = []
        current = value
        count = 0
        while current != 1 and count < MAX_LIST_ELEMENTS:
            header = _read_uint64(process, current)
            if (header & 0xFF) != 248:
                break
            head = _read_uint64(process, current + 8)
            tail = _read_uint64(process, current + 16)
            elems.append(_format_ocaml_value(process, head, depth + 1))
            current = tail
            count += 1
        if current != 1:
            elems.append("…")
        return f"[{'; '.join(elems)}]"
    if tag in (251, 252):
        try:
            length = size * 8
            error = lldb.SBError()
            data = process.ReadMemory(value + 8, length, error)
            if error.Fail():
                raise RuntimeError(error.GetCString())
            text = data.decode("utf-8", errors="ignore").rstrip("\x00")
            return f'"{text}"'
        except Exception as exc:  # noqa: BLE001
            return f"string<{exc}>"
    return f"block(tag={tag}, size={size})"


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
        return reg.GetValueAsUnsigned()
    if location.kind == "fbreg":
        fp_name = arch.frame_pointer()
        if not fp_name:
            return None
        reg = frame.FindRegister(fp_name)
        if (not reg or not reg.IsValid()) and fp_name == "fp":
            reg = frame.FindRegister("x29")
        if not reg or not reg.IsValid():
            return None
        base = reg.GetValueAsUnsigned()
        address = base + location.value
        try:
            return _read_uint64(process, address)
        except RuntimeError:
            return None
    if location.kind == "const":
        return location.value
    return None


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
    arch = _Architecture(process.GetTarget().GetTriple())
    seen = 0
    for var in func.variables:
        value = _evaluate_location(frame, process, arch, var.location)
        if value is None:
            continue
        summary = _format_ocaml_value(process, value)
        kind = "param" if var.is_param else "local"
        _print(f"{var.name} ({kind}) = {summary}")
        seen += 1
    if seen == 0:
        _print("No OCaml variables found")


def __lldb_init_module(debugger, _dict):
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
    print("OCaml LLDB helpers loaded (commands: ocaml_vars)")
