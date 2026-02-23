# Run job on Sirius.

This function configures the job submission to Sirius. It creates an
object of class `config` that can be used to submit a job Sirius, it can
also be saved and reused later on through the `saveJobConfig()`function.
For example on how to use, see the vignette.

Depending on what task you want to perform, you can specify the
following parameters:

- [`spectraMatchingParam`](https://rformassspectrometry.github.io/RuSirius/reference/spectraMatchingParam.md):
  Allows to perform matching betweeen spectra input to spectral
  libraries.

- [`formulaIdParam`](https://rformassspectrometry.github.io/RuSirius/reference/formulaIdParam.md):
  Allows to generate molecular formula candidates for each features.

- [`zodiacParam`](https://rformassspectrometry.github.io/RuSirius/reference/zodiacParam.md):
  Allows to perform re-ranking of formula candidates using *Zodiac*. It
  is advised to only perform it if De Novo structure annotation is run
  later.

- [`predictParam`](https://rformassspectrometry.github.io/RuSirius/reference/predictParam.md):
  Allows to perform molecular fingeprint prediction using *CSI:FingerID*
  and compound classification using *CANOPUS*.

- [`structureDbSearchParam`](https://rformassspectrometry.github.io/RuSirius/reference/structureDbSearchParam.md):
  Allows to perform structure annotation based on the fingerprint
  identifications.

- [`deNovoStructureParam`](https://rformassspectrometry.github.io/RuSirius/reference/deNovoStructureParam.md):
  Allows to perform de novo structure generation using the *MSNovelist*
  tool.

## Usage

``` r
run(
  sirius,
  compoundsIds = character(),
  alignedFeaturesIds = featuresId(sirius),
  fallbackAdducts = c("[M + H]+", "[M - H]-", "[M + Na]+", "[M + K]+"),
  enforceAdducts = character(),
  detectableAdducts = c("[M + H3N + H]+", "[M - H4O2 + H]+", "[M - H2O - H]-",
    "[M - H3N - H]-", "[M + Cl]-", "[2M + K]+", "[M + K]+", "[2M + Cl]-",
    "[M + C2H4O2 - H]-", "[M + H]+", "[2M + H]+", "[M - CH3 - H]-", "[M - H]-",
    "[M + Na]+", "[M - H2O + H]+"),
  spectraSearchParams = NA,
  formulaIdParams = NA,
  zodiacParams = NA,
  predictParams = NA,
  structureDbSearchParams = NA,
  msNovelistParams = NA,
  recompute = FALSE,
  configFile = character(),
  wait = TRUE
)

config(
  compoundsIds = character(),
  alignedFeaturesIds = character(),
  fallbackAdducts = c("[M + H]+", "[M - H]-", "[M + Na]+", "[M + K]+"),
  enforceAdducts = character(),
  detectableAdducts = c("[M + H3N + H]+", "[M - H4O2 + H]+", "[M - H2O - H]-",
    "[M - H3N - H]-", "[M + Cl]-", "[2M + K]+", "[M + K]+", "[2M + Cl]-",
    "[M + C2H4O2 - H]-", "[M + H]+", "[2M + H]+", "[M - CH3 - H]-", "[M - H]-",
    "[M + Na]+", "[M - H2O + H]+"),
  formulaIdParams = formulaIdParam(),
  zodiacParams = NA,
  predictParams = NA,
  structureDbSearchParams = NA,
  msNovelistParams = NA,
  spectraSearchParams = NA,
  recompute = FALSE
)
```

## Arguments

- sirius:

  `Sirius` object, the connection to the Sirius server.

- compoundsIds:

  `character` vector, the ids of the compounds to process.

- alignedFeaturesIds:

  `character` vector, the ids of the aligned features to process. By
  default, computes all.

- fallbackAdducts:

  `character` vector, fallback adducts are considered if the auto
  detection did not find any indication for an ion mode.

- enforceAdducts:

  `character` vector, the adducts to enforce. They are always
  considered.

- detectableAdducts:

  `character` vector, detectable adducts are only considered if there is
  an indication in the MS1 scan (e.g. correct mass delta).

- spectraSearchParams:

  object of class `spectraMatchingParam`, containing the parameters for
  the spectra matching.

- formulaIdParams:

  object of class `formulaIdParam`, containing the parameters for the
  molecular formula identification.

- zodiacParams:

  object of class `zodiacParam`, containing the parameters for the
  Zodiac re-ranking.

- predictParams:

  object of class `predictParam`, containing the parameters for the
  molecular fingerprint prediction and compound classification.

- structureDbSearchParams:

  object of class `structureDbParam`, containing the parameters for the
  structure annotation.

- msNovelistParams:

  object of class `deNovoStructureParam`, containing the parameters for
  the de novo structure generation.

- recompute:

  `logical`, whether to recompute the job , only necessary if the
  configfile comes from a saved file. Default is FALSE.

- configFile:

  `character`, the path to the configuration file to use for the job
  submission, optional.

- wait:

  `logical`, whether to wait for the job to finish. Default is `TRUE`

## Value

The job ID of the submitted job, it can be inputted in the
[`jobInfo()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md)
function to retrieve the job information. To retrieve results see
[`results`](https://rformassspectrometry.github.io/RuSirius/reference/results.md)
documentation.

## Only formula identification

If you only want to perform formula identification, you can by only
inputing the `formulaIdParam` object. In combination, you can also input
the `spectraMatchingParam` object to perform spectral matching and
subsequently compare the results.

## Structure annotation

To performe structure annotation, you need input the `formulaIdParam`
object, as well as the `predictParam` object to perform molecular
fingerprint prediction and compound classification. These results will
then subsequently be used to perform structure annotation using the
`structureDbSearchParam` object.

## De Novo structure annotation

To perform de novo structure annotation, you need to input the
`formulaIdParam` object, it is also advised in this case to perform
re-ranking using the `zodiacParam` object. The molecular fingerprint
prediction and compound classification can be performed using the
`predictParam` object. The `deNovoStructureParam` object is then used to
perform the de novo structure annotation.
