from pathlib import Path
from typing import Literal, Optional, TypedDict, cast

from common import *
from parselog import (
    TestCategoriser,
    categorise_kani,
    categorise_miri,
    categorise_rusteria,
)


CmdExec = tuple[Literal["exec"], tuple[SuiteName]]
CmdAll = tuple[Literal["all"], tuple]
CmdEval = tuple[Literal["eval"], tuple[SuiteName, int]]
CmdEvalDiff = tuple[Literal["eval-diff"], tuple[Path, Path]]
CmdBenchmark = tuple[Literal["benchmark"], tuple]
Cmd = CmdExec | CmdAll | CmdEval | CmdEvalDiff | CmdBenchmark


class CliOpts(TypedDict):
    cmd: Cmd
    tool: ToolName
    tool_cmd: list[str]
    filters: list[str]
    exclusions: list[str]
    tag: Optional[str]
    no_skips: bool
    timeout: Optional[int]
    test_folder: Optional[Path]
    categorise: TestCategoriser


class ArgError(Exception):
    def __init__(self, msg: str):
        super().__init__(msg)
        self.msg = msg

    def __str__(self):
        return self.msg


class FakeCliOpts:
    def __getattribute__(self, _: str, /):
        raise RuntimeError("Call parse_flags before accessing OPTS")


def parse_flags():
    opts: CliOpts = {
        "cmd": cast(Cmd, None),
        "tool": "Rusteria",
        "tool_cmd": [],
        "filters": [],
        "exclusions": [],
        "tag": None,
        "no_skips": False,
        "timeout": None,
        "test_folder": None,
        "categorise": categorise_rusteria,
    }

    sys.argv.pop(0)  # remove script name
    if len(sys.argv) == 0:
        raise ArgError("missing command")
    arg = sys.argv.pop(0)
    if arg in SUITE_NAMES:
        opts["cmd"] = ("exec", (arg,))
    elif arg == "all":
        opts["cmd"] = ("all", ())
    elif arg == "eval":
        if len(sys.argv) == 0:
            raise ArgError("missing test suite name: kani or miri")
        suite = sys.argv.pop(0)
        if suite not in SUITE_NAMES:
            raise ArgError("invalid test suite name, expected kani or miri")
        opts["cmd"] = ("eval", (suite, 5))
    elif arg == "eval-diff":
        if len(sys.argv) < 2:
            raise ArgError("missing paths to two evaluation CSV files")
        file1 = Path(sys.argv.pop(0))
        file2 = Path(sys.argv.pop(0))
        opts["cmd"] = ("eval-diff", (file1, file2))
    elif arg == "benchmark":
        opts["cmd"] = ("benchmark", ())
    else:
        raise ArgError(
            f"Unknown command, expected {', '.join(SUITE_NAMES)}, all, eval or eval-diff"
        )

    prev = None
    args = sys.argv.copy()

    def pop():
        nonlocal prev
        if len(args) == 0:
            raise ArgError(f"{RED}Unexpected end of arguments after {prev}")
        prev = args[0]
        return args.pop(0)

    with_miri = False
    with_kani = False
    with_obol = False
    cmd_flags: list[str] = []
    while len(args) > 0:
        arg = pop()
        if arg == "--":
            cmd_flags = args
            break
        elif arg == "-f":
            opts["filters"].append(pop())
        elif arg == "-e":
            opts["exclusions"].append(pop())
        elif arg == "-i":
            if opts["cmd"][0] != "eval":
                raise ArgError(f"{RED}The -i flag is only valid with the eval command.")
            suite = opts["cmd"][1][0]
            opts["cmd"] = ("eval", (suite, int(pop())))
        elif arg == "--tag":
            opts["tag"] = pop()
        elif arg == "--folder":
            folder = Path(pop()).resolve()
            if not folder.is_dir():
                raise ArgError(
                    f"{RED}The folder {folder} does not exist or is not a directory."
                )
            opts["test_folder"] = folder
        elif arg == "--no-skip" or arg == "--no-skips":
            opts["no_skips"] = True
        elif arg == "--timeout":
            opts["timeout"] = int(pop())
        elif arg == "--miri":
            with_miri = True
        elif arg == "--kani":
            with_kani = True
        elif arg == "--obol":
            with_obol = True

        else:
            raise ArgError(f"{RED}Unknown flag: {arg}")

    if with_miri + with_kani + with_obol > 1:
        raise ArgError(f"{RED}Can't use both Kani, Miri or Obol!")

    if with_kani:
        opts = opts_for_kani(opts)

    elif with_miri:
        opts = opts_for_miri(opts)

    else:
        opts = opts_for_rusteria(opts, force_obol=with_obol)

    opts["tool_cmd"] += cmd_flags
    return opts


def opts_for_rusteria(opts: CliOpts, *, force_obol: bool = False) -> CliOpts:
    opts = {
        **opts,
        "tool": "Rusteria",
        "tool_cmd": [
            "soteria-rust",
            "rustc",
            "--compact",
            "--no-color",
            "--log-compilation",
            "--solver-timeout=5000",
        ],
        "categorise": categorise_rusteria,
    }
    if force_obol:
        opts["tool_cmd"].append("--obol")
    return opts


def opts_for_kani(opts: CliOpts) -> CliOpts:
    return {
        **opts,
        "tool": "Kani",
        "tool_cmd": [
            "kani",
            "-Z=unstable-options",
            "--harness-timeout=5s",
        ],
        "categorise": categorise_kani,
    }


def opts_for_miri(opts: CliOpts) -> CliOpts:
    return {
        **opts,
        "tool": "Miri",
        "tool_cmd": [
            str((PWD / ".." / ".." / ".." / "miri" / "miri").resolve()),
            "run",
            "-Awarnings",
        ],
        "categorise": categorise_miri,
    }
