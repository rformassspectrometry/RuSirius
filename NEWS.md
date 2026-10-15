# Version 1.1

## Changes in 1.1.0

- Updated the RSirius dependency to the 6.5.4 client (commit `846e9fb`),
  matching the current Sirius 6.5.x line, and bumped the Docker image to
  Sirius 6.5.4.
- Corrected the documented Sirius requirement to 6.5 in DESCRIPTION, README
  and the vignettes (there is no Sirius 6.4 release).
- The RSirius client interface used by RuSirius (endpoints, SDK methods and
  model classes) is unchanged in 6.5.4; API 3.2 only adds new paged
  endpoints. Updated the RSirius version check in `test-api-responses.R`.
- `results()` with `result.type = "structureDb"` or `"deNovo"` now fetches
  all structure candidates of a feature in a single API call (new per-feature
  endpoints `GetStructureCandidates` / `GetDeNovoStructureCandidates`)
  instead of one call per formula. The output is the same: the top
  `topStructure` candidates per formula, joined onto the top `topFormula`
  formulas (de novo results may now include an empty `structureName`
  column).
- Fixed the Docker start script for Sirius 6.5: a `-p` placed before the
  subcommand now means `--project`, so the service is started with
  `sirius service --port 9999`.
- Fixed `run()` ignoring `alignedFeaturesIds` and `compoundsIds`: the ids
  were dropped when building the job (`alignedFeaturesIds` vs the API field
  `alignedFeatureIds`), so every job ran on the whole project. Jobs now only
  process the selected features/compounds; the default (all features) is
  unchanged. Added tests checking the ids reach the submitted job.
- `openProject()` now checks the `path` before closing the open projects, so
  a wrong path no longer closes the project you are working on.
- The "Retrieving Results" vignette now uses the `test_spectra` project
  computed in the "Importing Spectra" vignette (the `my_analysis` project it
  used never contained any data).
- Fixed `createDb()` ignoring `location`: the path was passed as the
  client's `data_file` (where the HTTP response is saved), so Sirius always
  wrote to its default `custom.siriusdb` and creating a second database
  failed. `createDb()` also now reports the server's error message.
- `deleteFeatures()` (and `import(deleteExistingFeatures = TRUE)`) now
  deletes all features in a single API call instead of one call per feature,
  and updates the feature map once; faster for large projects.
- `removeDb()` gains a `delete` argument (default `FALSE`) to also delete the
  database file from disk.
- The "Custom Databases" vignette now downloads the MassBank EU library from
  GNPS instead of looking for an MGF file that is not part of the package.

# Version 1.0

## Changes in 1.0.5

- Fixed errors in `summary()` and `results()` when the Sirius API returns
  heterogeneous payloads across features, e.g. "Can't combine
  `confidenceExactMatch` <double> and <character>" or "arguments imply
  differing number of rows: 1, 0" (#54, fixed by @jmorim in #55). API
  payloads are now normalized with `.row_from_payload()` and
  `.standardize_columns()` before being bound into a `data.frame`, and
  `confidenceExactMatch` / `confidenceApproxMatch` are always coerced to
  numeric.
- Applied the same normalization to the remaining result types:
  `summary(result.type = "spectralDbMatch")` and `results()` with
  `result.type = "formulaId"`, `"spectralDbMatch"` and `"fragTree"`.

## Changes in 1.0.4

- Bumped the Docker image from Sirius 6.3.4 to 6.3.7.
- Fixed `run(wait = TRUE)` hanging forever ("progress never reaches DONE").
  The wait loop previously used `while (state != "DONE")`, which looped
  indefinitely on jobs that ended in `FAILED`/`CANCELED`, crashed with
  "argument is of length zero" when the server returned no progress object,
  and hung silently when a web-service tool (CSI:FingerID, CANOPUS,
  structure DB search, MSNovelist) stalled. `run()` now:
  - returns on `DONE` and stops with an informative error (including the
    server's error message) on `FAILED`/`CANCELED`;
  - tolerates transient/missing progress reads and aborts gracefully after
    repeated failures;
  - accepts a `timeout` (default `Inf`) so a stalled job no longer hangs the
    session indefinitely, plus `pollInterval` and `verbose` arguments.
- Fixed `run(configFile = ...)`: the call to `StartJobFromConfig` no longer
  matched the updated RSirius client signature (it passed the fetched config
  object as the config name and `recompute` as the request body). It now
  passes the config name and the feature/compound id list correctly.
- Added tests for the new job-waiting helpers in `test-helpers.R`.

## Changes in 1.0.3

- Switched license to the Apache License 2.0 (matching RSirius) and added the
  full license text as a plain-text `LICENSE` file in the repository root.

## Changes in 1.0.2

- Added `path_to_sirius` parameter to `Sirius()` to allow specifying a custom
  path to the Sirius executable. Useful when running on a cluster or any
  environment where the executable is not on `PATH`.

## Changes in 1.0.1

- Bumped SIRIUS dependency to 6.4 and updated RSirius pin to the
  corresponding tag (commit `6470a73`).

## Changes in 1.0.0

- Publication-ready release.
- Fixed `summary()` masking `base::summary()`: now registered as a proper S4
  method for the `Sirius` class.
- Fixed bug in `saveConfig()` using `@` instead of `$` for API access.
- Fixed newline escape in `Sirius()` retry message.
- Moved `jsonlite` from `Depends` to `Imports`.
- Lowered minimum R version to >= 4.4.0.
- Added `biocViews` to DESCRIPTION.
- Replaced placeholder `@references` with real citations in all parameter
  documentation.
- Added missing `@return` tags for `import()` and `predictParam()`.
- Added `inst/CITATION` file.
- Updated README for release (Active badge, Contributing section).
- Added some sanity check for spectra object used to import features.

# Version 0.2

## Changes in 0.2.6

- Rewrote `Dockerfile` to install Sirius 6.3.4 directly (replaces conda).
  Sirius REST API now auto-starts on port 9999 via s6-overlay init script.
- All vignettes now use `port = 9999` in `Sirius()` calls.
- Added pre-computed vignette infrastructure (`.Rmd.orig` + `pre-render.R`)
  so pkgdown articles display real output.

## Changes in 0.2.5

- Fixed a bug where calling `run()` with only `msNovelistParams` (or
  `structureDbSearchParams`) would silently skip formula identification and
  fingerprint prediction because `formulaIdParams` and `predictParams`
  default to `NA` in `run()`. `config()` now automatically enables
  prerequisite parameters when downstream tools require them:
  `deNovoStructureParam` / `structureDbSearchParam` → enables `predictParam`;
  any of those or `predictParam` → enables `formulaIdParam`.
  Explicitly supplied parameter objects are never overridden.

## Changes in 0.2.4

- Adduct strings are now automatically normalized to the canonical SIRIUS
  format with spaces around operators (e.g. `"[M+H]+"` → `"[M + H]+"`).
  Both compact and spaced formats are now accepted in `import()`, `run()`,
  and `config()`.
- Added `candidateFormulas` parameter to `formulaIdParam()`. This allows
  restricting SIRIUS to specific molecular formulas instead of de novo
  generation. The formulas are passed via `JobSubmission$configMap` since
  SIRIUS does not accept them as a standard `formulaIdParams` field.

## Changes in 0.2.3

- Fixed a bug where importing a single MS2 spectrum (no MS1) resulted in
  0 features in the project, causing `run()` to fail with
  "Either 'compoundsIds' or 'alignedFeaturesIds' must be provided."
- `import()` now correctly handles single-level MS2-only data by grouping
  spectra via `.groupMSnIndex()` (previously only triggered for multi-level
  MSn data).
- `.createfeatures()` now always sets `ionMass` from `precursorMz` when MSn
  data is available. Previously `ionMass` was hardcoded to `0`, which caused
  the Sirius API to silently discard features without MS1 data. For MS1-only
  features, `ionMass` remains `0` so Sirius can derive it from the MS1
  spectrum.
- Updated RSirius dependency to latest release (`3.1+sirius6.3.3`, commit
  `2c983e6`). The new version exposes `gui_api` with proper `OpenGui`,
  `CloseGui`, and `GetGuis` endpoints.
- Simplified `openGUI()` and `closeGUI()` to use the official `gui_api`
  endpoints instead of manually constructing HTTP requests via `.callGuiApi()`.
- Removed the internal `.callGuiApi()` helper (no longer needed).
- Added `docker-push.yaml` GitHub Actions workflow for automated Docker image
  builds on push to `main`.

## Changes in 0.2.2

- Added support for importing MSn-only data (MS2, MS2+MS3, etc.) without
  requiring MS1 spectra. The SIRIUS API accepts features with no MS1 data.
- Added `.groupMSnIndex()` helper that automatically groups MSn-only spectra by
  acquisition order when no `ms_column_name` is provided and no MS1 data is
  present.
- `.createfeatures()` now includes MS3+ spectra (msLevel >= 2) in the
  `ms2Spectra` field sent to the API.
- Fixed a bug in `import()` where `ms_column_name` was used before being
  assigned when the input `Spectra` contained only a single MS level.
- Added "Importing MS2-only or MSn-only data" section to the ImportSpectra
  vignette.
- Added `test-msn-import.R` test file with 30 expectations covering
  MSn-only grouping, feature creation, and API integration.

## Changes in 0.2.1

- Replaced `msdata` dependency with `MsDataHub` for example data in vignettes.
- Added "Getting Started" vignette covering basic connection and project
  management.
- Added "Retrieving Results" vignette explaining result types and retrieval.
- Added "Custom Databases" vignette for creating and using custom databases.
- Updated Dockerfile for Bioconductor 3.22.

## Changes in 0.2.0

- **BREAKING**: Updated RSirius dependency from commit `c35ebbb` to `e4acd1b`
  (API version 3.1, Sirius 6.3.3 support).
- **BREAKING**: Package dependency renamed from `Rsirius` to `RSirius`
  (capital S) to match upstream rename.
- Updated Sirius version requirement from 6.1 to 6.3.
- Updated the import to take a single `Spectra` object. See documentation
  for more information.
- Added a vignette for a basic example.
- Updated to use `Spectra::fragmentGroupIndex()` for grouping MS1/MS2 spectra.
- Added comprehensive test suite with testthat for parameter classes,
  helper functions, and integration tests.

# Version 0.1

## Changes in 0.1.5

- Refactor the default path for opening/saving projects. It will now save by
  default in the current directory if the `path =` parameter is not precised.
- Can now import directly from a Spectra object, without having to process
  beforehand. Still needs to be improved, see issue [#27](https://github.com/rformassspectrometry/RuSirius/issues/27)


## Changes in 0.1.4

- Now supports import for negative polarity.
  See [#24](https://github.com/rformassspectrometry/RuSirius/issues/24)
- Addition of critical issue in the readme file (line 115).
  Related to issue [#19](https://github.com/rformassspectrometry/RuSirius/issues/19).
- Fix issue on `show()` method.


## Changes in 0.1.3

- Now can choose or not to delete features already present when importing new
  ones. Through parameter `deleteExistingFeatures =` in `import()`. Default is
  `TRUE`.
- Addition of functionalities to create, delete and list databases.
  See [#15](https://github.com/rformassspectrometry/RuSirius/issues/15)
- Can now import feature with set adducts if known.
  See [#14](https://github.com/rformassspectrometry/RuSirius/issues/14)
- Small fixes for `show()` method of `Sirius` object.
  See [#8](https://github.com/rformassspectrometry/RuSirius/issues/8)
- Fix spectral Library matching and now can see a summary of full results using
  `summary()` and `results()` function and precising
  `result.type = "spectralDbMatch"`.
- Added results() for fragmentation tree. accessible using
  `result.type = "fragTree"`.
  See [#18](https://github.com/rformassspectrometry/RuSirius/issues/18)
- Added a DockerFile, but cannot seem to start Sirius properly. Work in Progress
  See [#17](https://github.com/rformassspectrometry/RuSirius/issues/17)

## Changes in 0.1.2

- Fix installation of Spectra dev version
- Addition of code to install devel version of xcms in readme.md

## Changes in 0.1.1

- Addition of package used in the vignettes to DESCRIPITON.
- Change vignette builder to Rmarkdown. Vignettes now go through the R command
  checks.

## Changes in 0.1.0

- Beginning of RuSirius :sunglasses:
