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


def merge_dicts(into: dict[str, int], from_: dict[str, int]) -> dict[str, int]:
    """Merge two TypedDicts."""
    for key, value in from_.items():
        if key in into:
            into[key] += value
        else:
            into[key] = value
    return into


def ask_and_remove(path: Path):
    response = input(f"Are you sure you want to remove '{path}'? (y/N): ")
    if response.lower() in ["y", "yes"]:
        os.system(f"rm -rf {str(path)}")
