#!/usr/bin/env python3

import argparse
import builtins
import sys

if not sys.version.startswith("3.11"):
    print("This script requires Python 3.11")
    sys.exit(1)

parser = argparse.ArgumentParser()
parser.add_argument("folder_path", help="Path to the folder")
args = parser.parse_args()

builtin_names = [name for name in dir(builtins) if (not name.startswith("__"))]
builtin_names.append("__import__")
builtin_names.sort()


print("\n".join(builtin_names))


def f(x):
    return isinstance(x, type)
