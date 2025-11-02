from dataclasses import dataclass, asdict

"""Utility functions for Soteria-C scripts."""

import sys
import os
from abc import ABC, abstractmethod
from pathlib import Path

PURPLE = "\033[0;35m"
RED = "\033[0;31m"
ORANGE = "\033[38;5;208m"
YELLOW = "\033[38;5;220m"
GREEN = "\033[0;32m"
CYAN = "\033[0;36m"
BLUE = "\033[0;34m"
MAGENTA = "\033[0;95m"
GRAY = "\033[0;90m"
BOLD = "\033[1m"
RESET = "\033[0m"

# if piping output, remove colors:
NO_COLOR = not sys.stdout.isatty()
if NO_COLOR:
    PURPLE = RED = ORANGE = YELLOW = GREEN = CYAN = BLUE = GRAY = BOLD = RESET = ""


class PrintersMixin(ABC):

    @abstractmethod
    def print_prefix(self) -> str:
        pass

    def print_message(self, message: str):
        print(f"{self.print_prefix()}{message}")

    def print_info(self, message: str):
        self.print_message(f"{CYAN}{message}{RESET}")

    def print_warning(self, message: str):
        self.print_message(f"{YELLOW}{message}{RESET}")

    def print_error(self, message: str):
        self.print_message(f"{RED}{message}{RESET}")

    def print_success(self, message: str):
        self.print_message(f"{GREEN}{message}{RESET}")

    def run_command(self, cmd: str):
        self.print_message(f"{MAGENTA}Running: {cmd}{RESET}")
        os.system(cmd)


class GlobalPrinter(PrintersMixin):

    def print_prefix(self) -> str:
        return f"{BOLD}{CYAN}Global{RESET} - "


global_printer = GlobalPrinter()


@dataclass
class Stats:
    branch_number: int
    steps_number: int
    unexplored_branch_number: int
    sat_unknowns: int
    exec_time: float
    sat_time: float
    give_up_reasons: dict[str, int]
    missing_without_fixes: list[str]

    def as_dict(self):
        return asdict(self)

    @classmethod
    def from_dict(cls, data: dict):
        if "give_up_reasons" in data:
            # map each field to its length
            data["give_up_reasons"] = {
                k: len(v) if isinstance(v, list) else v
                for k, v in data["give_up_reasons"].items()
            }
        return cls(**data)

    @classmethod
    def empty(cls):
        return cls(
            branch_number=0,
            steps_number=0,
            exec_time=0.0,
            give_up_reasons={},
            missing_without_fixes=[],
            unexplored_branch_number=0,
            sat_unknowns=0,
            sat_time=0.0,
        )


def merge_stats(a: Stats, b: Stats) -> Stats:
    return Stats(
        branch_number=a.branch_number + b.branch_number,
        steps_number=a.steps_number + b.steps_number,
        exec_time=a.exec_time + b.exec_time,
        give_up_reasons={
            k: a.give_up_reasons.get(k, 0) + b.give_up_reasons.get(k, 0)
            for k in set(a.give_up_reasons) | set(b.give_up_reasons)
        },
        missing_without_fixes=list(
            set(a.missing_without_fixes + b.missing_without_fixes)
        ),
        unexplored_branch_number=a.unexplored_branch_number
        + b.unexplored_branch_number,
        sat_unknowns=a.sat_unknowns + b.sat_unknowns,
        sat_time=a.sat_time + b.sat_time,
    )


def ask_and_remove(path: Path):
    if path.exists():
        response = input(f"Are you sure you want to remove '{path}'? (y/N): ")
        if response.lower() in ["y", "yes"]:
            os.system(f"rm -rf {str(path)}")
