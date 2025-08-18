# Contributing to Soteria

## Coding Guidelines

### Exceptions

- The library and executable should *never* raise uncaught exceptions unless they correspond to an internal error that should be fixed in the tool. For instance, it is ok to raise an exception for patterns that are believed to be unreachable.