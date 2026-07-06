# Releasing Soteria

The `soteria` library and the `soteria-c` / `soteria-rust` tools are released
together under a single version number.

## Versions

We are in the `0.x` series. From a release `0.N.M` the only permitted bumps are:

- **`0.(N+1).0`** for changes that are **breaking** (CLI, output format, or
  library API);
- **`0.N.(M+1)`** for **non-breaking** changes.

We enforce these rules in `scripts/check_semver_bump.py`, which the release 
workflow runs.

The version number is specified in entry `SOTERIA_VERSION` in
`scripts/versions.json`. It is propagated across the repo with 
`scripts/versionsync.py`, and CI checks they stay in sync. 

## Changelog

`CHANGELOG.md` follows [Keep a Changelog](https://keepachangelog.com). Every PR
that changes behaviour adds a bullet under `## [Unreleased]`. CI
(`.github/workflows/changelog.yml`) only checks the file is well-formed.

## Cutting a release

1. **Open a release PR** that:
   - bumps `SOTERIA_VERSION` in `scripts/versions.json`, by doing
     `scripts/version_sync.py set SOTERIA_VERSION X.Y.Z`
   - renames `## [Unreleased]` in `CHANGELOG.md` to
     `## [X.Y.Z] - YYYY-MM-DD` and adds a fresh empty `## [Unreleased]` above
     it.
2. **Merge it to `main`** and wait for CI to pass.
3. **Run the release workflow**: Actions → *Release* → *Run workflow* (on
   `main`). It will:
   - read `SOTERIA_VERSION`, verify the bump is legal and the tag is new;
   - rebuild and re-test everything (`build.yml` + `test-packages.yml`);
   - create the immutable tag `vX.Y.Z` at the tested commit and a GitHub
     Release with the changelog section and the prebuilt binaries attached.

The packaging/upload logic is shared with the nightly workflow via
`.github/actions/publish-release`.

## Publishing to OPAM (optional)

For now we only publish `soteria` on the 
[OPAM repository](https://github.com/ocaml/opam-repository).

To publish `soteria`, tick **publish_opam** when running the release workflow.
It runs `opam publish`, submitting a PR to `ocaml/opam-repository`. The current
token is set up to use the account of [Opale](https://github.com/N1ark), so the
PR is from her fork.

In the future we may switch from `opam publish` to a custom workflow so that
we can publish from a bot account instead of a personal one, but for now this is
easiest.

Prerequisites:

- An `OPAM_PUBLISH_TOKEN` repository secret: a GitHub PAT belonging to an
  account that can fork `ocaml/opam-repository`. Note: opam-publish forks to
  the **token owner's personal account** and opens the PR from there.
- Every `soteria` dependency must already be released on OPAM; pinned
  dependencies are not supported.
