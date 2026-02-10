"""Utility functions for Soteria-C scripts."""

import sys
import os
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Any
from collections import Counter

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


def normalize_stats(data: dict[str, Any]) -> dict[str, Any]:
    """
    Normalize a stats dictionary by:
    1. Removing the 'soteria-c.give-up-reasons' key entirely
    2. Converting 'soteria.give-up-reasons' from a list to a count dictionary
    
    Args:
        data: Raw statistics dictionary
        
    Returns:
        Normalized statistics dictionary
    """
    result = {}
    
    for key, value in data.items():
        # Remove soteria-c.give-up-reasons entirely
        if key == "soteria-c.give-up-reasons":
            continue
            
        # Convert soteria.give-up-reasons from list to count dict
        if key == "soteria.give-up-reasons" and isinstance(value, list):
            result[key] = dict(Counter(value))
        else:
            result[key] = value
            
    return result


def merge_entry(entry1: Any, entry2: Any) -> Any:
    """
    Merge two stat entries based on their types, following the OCaml logic:
    - Int: addition
    - Float: addition
    - List (StrSeq): concatenation
    - Dict (Map): recursive merge
    - Other: create list of both values
    
    Args:
        entry1: First entry
        entry2: Second entry
        
    Returns:
        Merged entry
        
    Raises:
        ValueError: If entries are incompatible types
    """
    # Both are ints
    if isinstance(entry1, int) and isinstance(entry2, int):
        return entry1 + entry2
    
    # Both are floats
    if isinstance(entry1, float) and isinstance(entry2, float):
        return entry1 + entry2
    
    # Both are lists (StrSeq)
    if isinstance(entry1, list) and isinstance(entry2, list):
        return entry1 + entry2
    
    # Both are dicts (Map) - merge recursively
    if isinstance(entry1, dict) and isinstance(entry2, dict):
        return merge_stats(entry1, entry2)
    
    # Incompatible types - create a list containing both
    if isinstance(entry1, list) and not isinstance(entry2, list):
        return entry1 + [entry2]
    if not isinstance(entry1, list) and isinstance(entry2, list):
        return [entry1] + entry2
    
    # Neither is a list - create a new list
    return [entry1, entry2]


def merge_stats(stats1: dict[str, Any], stats2: dict[str, Any]) -> dict[str, Any]:
    """
    Merge two statistics dictionaries generically based on entry types.
    
    Args:
        stats1: First statistics dictionary
        stats2: Second statistics dictionary
        
    Returns:
        Merged statistics dictionary
    """
    result = {}
    
    # Get all keys from both dictionaries
    all_keys = set(stats1.keys()) | set(stats2.keys())
    
    for key in all_keys:
        if key in stats1 and key in stats2:
            # Both have this key - merge the entries
            result[key] = merge_entry(stats1[key], stats2[key])
        elif key in stats1:
            # Only in stats1
            result[key] = stats1[key]
        else:
            # Only in stats2
            result[key] = stats2[key]
            
    return result


def ask_and_remove(path: Path):
    if path.exists():
        response = input(f"Are you sure you want to remove '{path}'? (y/N): ")
        if response.lower() in ["y", "yes"]:
            os.system(f"rm -rf {str(path)}")
