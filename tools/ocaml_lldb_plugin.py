"""LLDB helpers that expose OCaml variables via DWARF (using llvm-dwarfdump)."""

from __future__ import annotations

import dataclasses
import os
import platform
import re
import shutil
import subprocess
from typing import Dict, List, Optional, Sequence, Tuple

import lldb

OCAML_CATEGORY = "OCaml"
MAX_LIST_ELEMENTS = 32
MAX_TUPLE_ELEMENTS = 16


@dataclasses.dataclass
class _VariableLocation:
    kind: str  # "reg" or "fbreg" or "const"
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


class _DwarfDumpParser:
    def __init__(self, text: str):
        self.functions: List[_DWARFFunction] = []
        self._parse(text)

    def _parse(self, text: str) -> None:
        stack: List[Dict[str, object]] = []

        def pop_until(indent: int) -> None:
            while stack and stack[-1]["indent"] >= indent:
                ctx = stack.pop()
                tag = ctx["tag"]
                obj = ctx.get("obj")
                if tag in ("formal_parameter", "variable") and isinstance(obj, _DWARFVariable):
                    fn_ctx = next((c for c in reversed(stack) if c["tag"] == "subprogram"), None)
                    if fn_ctx and obj.name:
                        fn = fn_ctx.get("obj")
                        if isinstance(fn, _DWARFFunction):
                            fn.variables.append(obj)
                elif tag == "subprogram" and isinstance(obj, _DWARFFunction):
                    if obj.name and obj.low_pc is not None and obj.high_pc is not None:
                        self.functions.append(obj)

        for line in text.splitlines():
            if not line.strip():
                continue
            indent = len(line) - len(line.lstrip(" "))
            stripped = line.strip()
            if stripped.startswith("0x") and "DW_TAG_" in stripped:
                tag = stripped.split("DW_TAG_")[1].split()[0]
                pop_until(indent + 1)
                ctx: Dict[str, object] = {"indent": indent, "tag": tag}
                if tag == "subprogram":
                    ctx["obj"] = _DWARFFunction(name="", low_pc=0, high_pc=0, variables=[])
                elif tag in ("formal_parameter", "variable"):
                    ctx["obj"] = _DWARFVariable(
                        name="",
                        is_param=(tag == "formal_parameter"),
                        location=None,
                    )
                stack.append(ctx)
                continue
            if stripped.startswith("NULL"):
                pop_until(indent + 1)
                continue
            if not stripped.startswith("DW_AT") or not stack:
                continue
            ctx = stack[-1]
            obj = ctx.get("obj")
            tag = ctx["tag"]
            if tag not in ("subprogram", "formal_parameter", "variable"):
                continue
            attr = stripped.split()[0]
            if attr == "DW_AT_name":
                match = re.search(r'"([^"]+)"', stripped)
                if match and obj:
                    if isinstance(obj, _DWARFFunction):
                        obj.name = match.group(1)
                    elif isinstance(obj, _DWARFVariable):
                        obj.name = match.group(1)
            elif attr == "DW_AT_low_pc" and isinstance(obj, _DWARFFunction):
                match = re.search(r"0x([0-9a-fA-F]+)", stripped)
                if match:
                    obj.low_pc = int(match.group(1), 16)
            elif attr == "DW_AT_high_pc" and isinstance(obj, _DWARFFunction):
                match = re.search(r"0x([0-9a-fA-F]+)", stripped)
                if match:
                    obj.high_pc = int(match.group(1), 16)
            elif attr == "DW_AT_location" and isinstance(obj, _DWARFVariable):
                location = _parse_location(stripped)
                if location:
                    obj.location = location

        pop_until(0)


def _parse_location(line: str) -> Optional[_VariableLocation]:
    if "DW_OP_fbreg" in line:
        match = re.search(r"DW_OP_fbreg\s*\(([-\d]+)\)", line)
        if match:
            return _VariableLocation("fbreg", int(match.group(1)))
    match = re.search(r"DW_OP_regx\s*(\d+)", line)
    if match:
        return _VariableLocation("reg", int(match.group(1)))
    match = re.search(r"DW_OP_reg(\d+)", line)
    if match:
        return _VariableLocation("reg", int(match.group(1)))
    match = re.search(r"DW_OP_consts\s*\(([-\d]+)\)", line)
    if match:
        return _VariableLocation("const", int(match.group(1)))
    return None


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


def _find_dwarfdump_command() -> Optional[Sequence[str]]:
    if platform.system() == "Darwin":
        return ["xcrun", "llvm-dwarfdump"]
    for exe in ("llvm-dwarfdump", "dwarfdump"):
        path = shutil.which(exe)
        if path:
            return [path]
    return None


def _symbol_file_path(module: lldb.SBModule) -> Optional[str]:
    spec = module.GetSymbolFileSpec()
    if spec and spec.IsValid():
        return os.path.join(spec.GetDirectory(), spec.GetFilename())
    spec = module.GetFileSpec()
    if spec and spec.IsValid():
        return os.path.join(spec.GetDirectory(), spec.GetFilename())
    return None


def _run_dwarfdump(module: lldb.SBModule) -> Optional[_DwarfDumpParser]:
    cmd = _find_dwarfdump_command()
    if not cmd:
        return None
    path = _symbol_file_path(module)
    if not path:
        return None
    try:
        completed = subprocess.run(
            cmd + ["--debug-info", "--show-form", path],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
    except Exception:
        return None
    return _DwarfDumpParser(completed.stdout)


_DWARF_CACHE: Dict[str, Optional[_DwarfDumpParser]] = {}


def _module_key(module: lldb.SBModule) -> str:
    spec = module.GetFileSpec()
    path = ""
    if spec and spec.IsValid():
        path = os.path.join(spec.GetDirectory(), spec.GetFilename())
    uuid = module.GetUUIDString() or ""
    return f"{path}:{uuid}"


def _get_module_info(frame: lldb.SBFrame) -> Optional[_DwarfDumpParser]:
    module = frame.GetModule()
    if not module or not module.IsValid():
        return None
    key = _module_key(module)
    if key not in _DWARF_CACHE:
        _DWARF_CACHE[key] = _run_dwarfdump(module)
    return _DWARF_CACHE[key]


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


def _find_function(info: _DwarfDumpParser, frame: lldb.SBFrame) -> Optional[_DWARFFunction]:
    target = frame.GetThread().GetProcess().GetTarget()
    pc = frame.GetPCAddress().GetLoadAddress(target)
    for fn in info.functions:
        if fn.contains(pc):
            return fn
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
    info = _get_module_info(frame)
    if not info:
        _print("DWARF data not available for this module")
        return
    function = _find_function(info, frame)
    if not function:
        _print("No OCaml DWARF function found for this frame")
        return
    process = frame.GetThread().GetProcess()
    arch = _Architecture(process.GetTarget().GetTriple())
    seen = 0
    for var in function.variables:
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
