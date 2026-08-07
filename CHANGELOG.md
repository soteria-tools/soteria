# Changelog

All notable changes to Soteria, Soteria Rust and Soteria Rust are recorded here.
The three are released together under a single version number. The format is
based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/); see
[RELEASING.md](./RELEASING.md) for the versioning policy and the release
process.

Add entries for your change under `## [Unreleased]` as part of your pull
request.

## [Unreleased]

## [0.2.1] - 2026-08-07

### Changed

- Changed the dependencies of Soteria's test, removing an unresolved import in the absence of `soteria-c`.
- Explicitly set `zarith`'s min version to `1.13`

## [0.2.0] - 2026-08-07

A small patch update while we work on large scale changes. A Soteria Rust flag was changed, and we saw overall performance improvements of Soteria Rust and C, by up to 20% and 8% respectively!

### Added

- The binary packages now ship `LICENSE`, `NOTICE`, `THIRD_PARTY_NOTICES` and the `LICENSES/` and `third-party-licenses/` directories.

### Changed

- Soteria Rust: renamed `--recursive-validity` to `--reference-to-invalid-memory` to better reflect that it's a lint (allow/deny/warn) and not a flag. It is also now set to **allow** by default, to improve performance.
- Update `THIRD_PARTY_NOTICES` to be up to date.
- Minor soundness fixes.
- Minor performance improvements to Soteria Rust and Soteria C.

## [0.1.0] - 2026-06-27

This is our first release! We will be releasing regularly, under the `0.x` series.

### Added

- Switched to [OCaml 5.5.0](https://ocaml.org/releases/5.5.0)! 
- `soteria-c version` and `soteria-rust version` now print the release
  version.

### Changed

- Minor performance improvements in Soteria Rust.
