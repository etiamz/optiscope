#!/usr/bin/env python3

# $ black -l 80 command/test_gc.py

import os
import re
import shlex
import subprocess
import sys
from pathlib import Path

# The system command to execute with `run`.
type Command = list[str | Path]

_ROOT = Path(__file__).resolve().parent.parent

_NORMAL_FORM = "(λ (λ (λ ((2 (0 0)) 0))))"

_MAX_N = 16


def main() -> None:
    compiler = compiler_command()
    target = _ROOT / "target" / "test-gc"
    target.mkdir(parents=True, exist_ok=True)
    object_file = target / "optiscope.o"
    executable_filename = target / "gc"

    run([*compiler, "-c", "optiscope.c", "-o", object_file])

    for n in range(_MAX_N + 1):
        print(f"Testing `examples/gc.c` on `N={n}`...", file=sys.stderr)
        run(
            [
                *compiler,
                f"-DN={n}",
                "examples/gc.c",
                object_file,
                "-o",
                executable_filename,
            ]
        )
        stdout, stderr = run([executable_filename])
        if stdout.strip() != _NORMAL_FORM:
            sys.exit(f"`N={n}`: expected {_NORMAL_FORM!r}, got {stdout!r}")
        match = re.search(
            r"^ *Total rewrites: *([0-9]+) *$",
            stderr,
            re.MULTILINE,
        )
        if match is None:
            sys.exit(f"`N={n}`: cannot match: {stderr!r}")
        got = int(match[1])
        check_total_rewrites(n, got)
        print(f"Good: {got} total rewrites.", file=sys.stderr)

    print("All good.", file=sys.stderr)


def check_total_rewrites(n: int, got: int) -> None:
    expected = (60, 104)[n] if n < 2 else 106 * n - 8
    if got != expected:
        sys.exit(f"`N={n}`: expected {expected} total rewrites, got {got}")


def compiler_command() -> Command:
    compiler = shlex.split(os.environ.get("CC") or "gcc")
    additional_options = (
        ["-Wno-deprecated-declarations", "-Wno-c11-extensions"]
        if sys.platform == "darwin"
        else []
    )
    options = [
        "-I.",
        "-Wall",
        "-Wextra",
        "-pedantic",
        "-std=c99",
        "-g",
        "-DOPTISCOPE_ENABLE_STATS",
        "-fsanitize=address,undefined",
        "-fno-sanitize-recover=all",
        *additional_options,
    ]
    return [*compiler, *options]


def run(command: Command) -> tuple[str, str]:
    process = subprocess.run(
        command, cwd=_ROOT, capture_output=True, text=True, encoding="utf-8"
    )
    if process.returncode != 0:
        sys.exit(
            f"{show_command(command)} exited with {process.returncode}: {process.stderr!r}"
        )
    return process.stdout, process.stderr


def show_command(command: Command) -> str:
    return " ".join(map(str, command))


if __name__ == "__main__":
    main()
