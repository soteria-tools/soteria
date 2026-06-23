# Releasing Soteria

The `soteria` library and the `soteria-c` / `soteria-rust` tools are released
together under a single version number.

## Versioning policy

We are in the `0.x` series. From a release `0.N.M` the only permitted bumps are:

- **`0.(N+1).0`** for changes that are **breaking** (CLI, output format, or
  library API);
- **`0.N.(M+1)`** for **non-breaking** changes.

The first release is `0.1.0`. The major version stays `0` for now. These rules
are enforced by `scripts/check_semver_bump.py`, which the release workflow runs.

## The version number lives in one place

`scripts/versions.json` → `SOTERIA_VERSION` is the single source of truth. It is
propagated to `soteria/lib/version.ml` (which backs `soteria-c --version` and
`soteria-rust --version`) by `scripts/versionsync.py`, and CI checks they stay
in sync. Do not edit `version.ml` by hand.

## Changelog discipline

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
2. **Merge it to `main`** and wait for CI to go green.
3. **Run the release workflow**: Actions → *Release* → *Run workflow* (on
   `main`). It will:
   - read `SOTERIA_VERSION`, verify the bump is legal and the tag is new;
   - rebuild and re-test everything (`build.yml` + `test-packages.yml`);
   - create the immutable tag `vX.Y.Z` at the tested commit and a GitHub
     Release with the changelog section, the four platform zips, and
     `SHA256SUMS.txt`.

The packaging/upload logic is shared with the nightly workflow via
`.github/actions/publish-release`.

## Publishing the library to OPAM (optional)

Only `soteria` can go to the central `ocaml/opam-repository`: `soteria-c` and
`soteria-rust` depend on Charon/Obol through git `pin-depends`, which the
opam-repository rejects. The tools are distributed as the prebuilt binaries
attached to the release (plus `opam pin` for source installs).

To publish `soteria`, tick **publish_opam** when running the release workflow.
It runs `opam publish` (which `--no-confirmation` explicitly supports for CI),
submitting a PR to `ocaml/opam-repository`.

Prerequisites:

- An `OPAM_PUBLISH_TOKEN` repository secret: a GitHub PAT belonging to an
  account that can fork `ocaml/opam-repository`. The workflow passes it as the
  `OPAM_PUBLISH_GH_TOKEN` variable that opam-publish reads. Note: opam-publish
  always forks to the **token owner's personal account** (it has no option to
  fork into an organization, so the staging fork cannot live under
  `soteria-tools`) and opens the PR from there. The fork is only a transient
  staging area — the contribution itself is the PR to `ocaml/opam-repository` —
  so choose whichever account you're comfortable hosting it under.
- Every `soteria` dependency must already be released in the official
  opam-repository. This was verified for the current dependency set — all of
  them resolve from the default `opam.ocaml.org` repository — but re-check if
  you add a new dep.

`soteria.opam` is generated from `dune-project` (`generate_opam_files`) and
passes `opam lint`; the version and source `url` are filled in by `opam publish`
from the tag, so they are intentionally absent from the checked-in file. A
maintainer still reviews and merges the PR on the opam-repository side.

## Binaries: what users still need

The release zips are **not** fully self-contained: `soteria-rust` needs a
matching **Charon** binary (and the Obol frontend) at runtime. The exact
commits used for each release are recorded in the release notes
("Frontend versions") for reproducibility.

The binaries are **not codesigned/notarized** yet. On macOS, users may need to
clear the quarantine attribute:

```bash
xattr -d com.apple.quarantine soteria-rust
```

Codesigning is a planned follow-up.

## Website

The downloads page should read GitHub Releases (the API or the
`releases/latest` redirect) rather than hard-coding links, so it tracks
releases automatically. API documentation is published as latest only.

## One-time repository setup

- **Tag protection / rulesets** so `v*` tags can't be deleted or force-moved.
- The `OPAM_PUBLISH_TOKEN` secret, if OPAM publishing is wanted.
