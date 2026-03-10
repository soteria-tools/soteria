#!/usr/bin/env python3

# Using the exclusions defined in test_exclusions.py, prune the tests in MIRI_PATH and
# KANI_PATH to remove excluded tests

from os import remove

from common import RED
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

if __name__ == "__main__":
    for path in MIRI_PATH.rglob("*.rs"):
        print(path)
        if any(e in str(path) for e in MIRI_EXCLUSIONS) and not any(
            k in str(path) for k in MIRI_KEEPS
        ):
            print(f"{RED}Removing {path}")
            remove(path)

    for path in KANI_PATH.rglob("*.rs"):
        print(path)
        if any(e in str(path) for e in KANI_EXCLUSIONS) and not any(
            k in str(path) for k in KANI_KEEPS
        ):
            print(f"{RED}Removing {path}")
            remove(path)
