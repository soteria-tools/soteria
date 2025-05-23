#!/usr/bin/env python3

from common import *

KANI_PATH = os.path.join(PWD, "..", "..", "..", "kani", "tests", "kani")
MIRI_PATH = os.path.join(PWD, "..", "..", "..", "miri", "tests")


def exec_tests(
    file: str,
    *,
    expect_failure: bool,
): ...


def main():
    files = os.listdir(KANI_PATH)
    for file in files:
        print(file)


if __name__ == "__main__":
    main()
