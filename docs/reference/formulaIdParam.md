# Identifying molecular formula

This function configures the parameters for molecular formula annotation
in Sirius. Molecular formula identification is done using isotope
pattern analysis on the MS1 data as well as fragmentation tree
computation on the MS2 data. The score of a molecular formula candidate
is a combination of the isotope pattern score and the fragmentation tree
score.

## Usage

``` r
formulaIdParam(
  instrument = c("QTOF", "ORBITRAP", "FTICR"),
  numberOfCandidates = 10,
  numberOfCandidatesPerIonization = 1,
  massAccuracyMS2ppm = 10,
  isotopeMs2Settings = c("IGNORE", "FILTER", "SCORE"),
  filterByIsotopePattern = TRUE,
  enforceElGordoFormula = TRUE,
  performBottomUpSearch = TRUE,
  performDeNovoBelowMz = 400,
  formulaSearchDBs = character(0),
  applyFormulaConstraintsToDBAndBottomUpSearch = FALSE,
  enforcedFormulaConstraints = c("H", "C", "N", "O", "P"),
  fallbackFormulaConstraints = c("S"),
  detectableElements = c("B", "S", "Cl", "Se", "Br"),
  ilpTimeout = FALSE,
  numberOfSecondsPerDecomposition = 0,
  numberOfSecondsPerInstance = 0,
  useHeuristic = TRUE,
  useHeuristicAboveMz = 300,
  useOnlyHeuristicAboveMz = 650,
  injectSpecLibMatchFormulas = TRUE,
  minScoreToInjectSpecLibMatch = 0.7,
  minPeaksToInjectSpecLibMatch = 6
)
```

## Arguments

- instrument:

  `character(1)` The type of mass spectrometer used for the analysis.
  Options include `"QTOF"`, `"ORBITRAP"`, and `"FTICR"`. This choice
  mainly affects the allowed mass deviation. If you are unsure about the
  instrument, use the default value `"QTOF"`.

- numberOfCandidates:

  `integer(1)` The number of formula candidates to keep in the result
  list. Default is `10`.

- numberOfCandidatesPerIonization:

  `integer(1)` Forces SIRIUS to report at least this number of
  candidates per ionization.

- massAccuracyMS2ppm:

  `numeric(1)` The maximum allowed mass deviation (in parts per million,
  ppm) for molecular formulas. Only formulas within this mass window are
  considered. Default is `10`.

- isotopeMs2Settings:

  `character(1)` Specifies how isotope patterns in MS/MS should be
  handled. Default is `"IGNORE"`. Options:

         - `"FILTER"`: Excludes formulas if the theoretical isotope pattern
           doesn't match.
         - `"SCORE"`: Uses isotope patterns for scoring, useful for clear
           MS/MS isotope patterns.
         - `"IGNORE"`: Ignores isotope patterns in MS/MS.

- filterByIsotopePattern:

  `logical` When `TRUE`, filters molecular formulas by comparing their
  theoretical isotope patterns to the measured ones, excluding those
  that don't match. Default is `TRUE`.

- enforceElGordoFormula:

  `logical` El Gordo may predict that an MS/MS spectrum is a lipid
  spectrum. If enabled, the corresponding molecular formula will be
  enforeced as molecular formula candidate. Default is `TRUE`.

- performBottomUpSearch:

  `logical` If `TRUE`, enables molecular formula generation through a
  bottom-up search. Default is `TRUE`.

- performDeNovoBelowMz:

  `numeric(1)` Specifies the m/z below which de novo molecular formula
  generation is enabled. Set to `0` to disable de novo molecular formula
  generation. Default is `400`.

- formulaSearchDBs:

  `list(character)` A list of structure databases (e.g., `"CHEBI"`,
  `"HMDB"`) from which molecular formulas are extracted to reduce the
  search space. Use only if necessary, as de novo formula annotation is
  usually more effective. Default is `character(0)`.

- applyFormulaConstraintsToDBAndBottomUpSearch:

  `logical` If `TRUE`, applies formula (element) constraints to both
  database search and bottom-up search, in addition to de novo
  generation. Default is `FALSE`.

- enforcedFormulaConstraints:

  `character` Specifies the elements that are always considered when
  auto-detecting the formula. Enforced elements are always included in
  the formula, even if the compound is already assigned to a specific
  molecular formula. Default is `H,C,N,O,P`.

- fallbackFormulaConstraints:

  `character` Specifies the elements that are used as fallback when
  auto-detection fails (e.g., no isotope pattern). Default is `S`.

- detectableElements:

  `list(character)` Defines the elements that can be added to the
  chemical alphabet when detected in the spectrum, such as from isotope
  patterns. Default is `c("B", "S", "Cl", "Se", "Br")`.

- ilpTimeout:

  `logical` The timeout settings for the integer linear programming
  (ILP) solver. If `TRUE`, it should include timeout parameters such as
  `numberOfSecondsPerDecomposition` and `numberOfSecondsPerInstance`.

- numberOfSecondsPerDecomposition:

  `numeric`

- numberOfSecondsPerInstance:

  `numeric`

- useHeuristic:

  `logical` If `TRUE`, enables the use of heuristics in molecular
  formula annotation. When enabled, additional thresholds like
  `useHeuristicAboveMz` and `useOnlyHeuristicAboveMz` can be set.

- useHeuristicAboveMz:

  `numeric` The m/z threshold above which heuristic is used. Default is
  `300`.

- useOnlyHeuristicAboveMz:

  `numeric(1)` The m/z threshold above which only heuristic is used.
  Default is `650`.

- injectSpecLibMatchFormulas:

  `logical` If `TRUE`, formula candidates matching spectral library
  entries above a certain similarity threshold will be preserved for
  further analysis, regardless of score or filter settings. Default is
  `TRUE`.

- minScoreToInjectSpecLibMatch:

  `numeric(1)` The similarity threshold for injecting spectral library
  match formulas. If the score is above this threshold, the formula will
  be preserved. Default is `0.7`.

- minPeaksToInjectSpecLibMatch:

  `integer` The minimum number of matching peaks required to inject
  spectral library match formulas into further analysis.

## Value

An object of class `formulaIdParam` with the specified parameters.

## Note

For more information, see the Sirius
[documentation](https://v6.docs.sirius-ms.io/methods-background).

## General parameters

We advise to set up these following parameter to fit your specific
study.

## Advanced parameters

If you want to specify these parameters we advise you read the Sirius
documentation to learn how to adapt them to your dataset and annotation
needs.

## References

reference

## Examples

``` r
# Example of creating a formulaIdParam object
param <- formulaIdParam(instrument = "QTOF",
                        numberOfCandidates = 5,
                        enforceElGordoFormula = TRUE)
```
