# Version 0.2

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
