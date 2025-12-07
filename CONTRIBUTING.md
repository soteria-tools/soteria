# Contributing to Soteria

This document is a work in progress...

## Coding Guidelines

### Exceptions

- The library and executable should *never* raise uncaught exceptions unless they correspond to an internal error that should be fixed in the tool. For instance, it is ok to raise an exception for patterns that are believed to be unreachable.

## Contributor License Agreement (CLA)

Before we can accept your contribution, you must agree to our [Contributor License Agreement](./CLA.md).
By opening a pull request, you confirm that you have read and accepted the terms of the CLA.
This ensures that we can safely use your contribution in the project under the Apache-2.0 license