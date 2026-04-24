#!/usr/bin/env python3

# Using the exclusions defined in test_exclusions.py, prune the tests in MIRI_PATH and
# KANI_PATH to remove excluded tests

from os import remove
from pathlib import Path

from common import RED, pprint
from config import KANI_PATH, MIRI_PATH
from test_exclusions import KANI_EXCLUSIONS, MIRI_EXCLUSIONS

# These paths are excluded because they don't contain tests but we still need them
# for the suites to run!

KANI_KEEPS = [
    "/Helpers/",
    "/UnsizedCoercion/defs.rs",
]

MIRI_KEEPS = [
    "/ui.rs",
    "/utils/",
]


# Remove all files in dir that are not .rs files or are .rs files that match the exclusions but not the keeps
def clear(dir: Path, exclusions: list[str], keeps: list[str]):
    for path in dir.rglob("*"):
        if path.is_dir():
            continue

        if path.suffix != ".rs" or (
            any(e in str(path) for e in exclusions)
            and not any(k in str(path) for k in keeps)
        ):
            pprint(f"{RED}Removing {path}")
            remove(path)


if __name__ == "__main__":
    clear(MIRI_PATH, MIRI_EXCLUSIONS, MIRI_KEEPS)
    clear(KANI_PATH, KANI_EXCLUSIONS, KANI_KEEPS)
