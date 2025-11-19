#!/usr/bin/env python3
"""Run a minimal LLDB session to check whether the sandbox lets us debug."""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


def _run_lldb(commands: list[str]) -> tuple[int, str, str]:
    args: list[str] = ["lldb", "-b"]
    for command in commands:
        args.extend(["-o", command])
    proc = subprocess.run(args, capture_output=True, text=True)
    return proc.returncode, proc.stdout, proc.stderr


def _escape_lldb_pattern(text: str) -> str:
    return text.replace("\\", "\\\\").replace("$", r"\$")


def run(args: argparse.Namespace) -> None:
    script_dir = Path(__file__).resolve().parent
    plugin_cmd = []
    if args.plugin:
        plugin_path = Path(args.plugin).resolve()
        plugin_cmd = [
            "settings set interpreter.require-overwrite false",
            f"command script import -r {plugin_path}",
        ]

    if args.symbol:
        regex = args.symbol.replace("$", ".*")
        bp_cmd = f"breakpoint set --func-regex {regex}"
    else:
        file_path = Path(args.file).resolve()
        bp_cmd = f"br s -f {file_path} -l {args.line}"

    commands = (
        [f"target create {args.target}"]
        + plugin_cmd
        + [
            bp_cmd,
            "run",
            "ocaml_vars",
            "process kill",
        ]
    )

    code, stdout, stderr = _run_lldb(commands)
    sys.stdout.write(stdout)
    sys.stderr.write(stderr)
    if code != 0:
        raise RuntimeError("LLDB command sequence failed")


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--plugin", help="Path to ocaml_lldb_plugin.py")
    parser.add_argument("--target", required=True, help="Binary to debug")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--symbol", help="Symbol name for breakpoint")
    group.add_argument("--file", help="Source file for breakpoint")
    parser.add_argument("--line", type=int, default=1, help="Line number")
    return parser.parse_args(argv)


def main() -> None:
    args = parse_args(sys.argv[1:])
    run(args)


if __name__ == "__main__":
    main()
