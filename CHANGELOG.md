# Changelog

All notable changes to Soteria, Soteria C, Soteria Rust, and Soteria PHP are
recorded here. They are released together under a single version number. The
format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/); see
[RELEASING.md](./RELEASING.md) for the versioning policy and the release process.

Add entries for your change under `## [Unreleased]` as part of your pull
request.

## [Unreleased]

### Added

- Soteria PHP: added standalone macOS arm64 and Linux x86_64 packages with
  bundled Z3 and locked PHP-Parser dependencies, package smoke tests, and
  nightly/versioned release assets. A compatible 64-bit system PHP runtime is
  still required.

### Changed

- Soteria Rust: renamed `--recursive-validity` to `--reference-to-invalid-memory` to better reflect that it's a lint (allow/deny/warn) and not a flag. It is also now set to **allow** by default, to improve performance.

## [0.1.0] - 2026-06-27

This is our first release! We will be releasing regularly, under the `0.x` series.

### Added

- Switched to [OCaml 5.5.0](https://ocaml.org/releases/5.5.0)! 
- `soteria-c version` and `soteria-rust version` now print the release
  version.

### Changed

- Minor performance improvements in Soteria Rust.
