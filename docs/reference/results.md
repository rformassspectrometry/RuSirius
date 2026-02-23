# Fetch Results from Sirius

Functions to retrieve results from the *Sirius* project, allowing
customization of the result type, feature selection, and return format.

Get best match results for a Sirius project. Choose which type of
results to fetch with the *result.type* parameter. "structure" and
"deNovo" will both also give information on the formula.

## Usage

``` r
summary(
  sirius,
  result.type = c("formulaId", "structure", "deNovo", "spectralDbMatch")
)

results(
  sirius,
  features = character(),
  result.type = c("formulaId", "structureDb", "compoundClass", "deNovo",
    "spectralDbMatch", "fragTree"),
  return.type = c("list", "data.frame"),
  topFormula = 5,
  topStructure = 5,
  topSpectralMatches = 5
)
```

## Arguments

- sirius:

  A `Sirius` object.

- result.type:

  `character(1)` specifying the type of results to fetch. Options are
  "formulaId", "structureDb", "compoundClass", "deNovo",
  "spectralDbMatch", and "fragTree". Defaults to "formulaId".

- features:

  [`character()`](https://rdrr.io/r/base/character.html) vector
  specifying feature IDs to retrieve results for. Defaults to all
  available features.

- return.type:

  `character(1)` specifying return format. Either "data.frame" (default)
  or "list".

- topFormula:

  `numeric(1)` Maximum number of formula candidates to retrieve per
  feature. Defaults to 5.

- topStructure:

  `numeric(1)` Maximum number of structure candidates per formula.
  Defaults to 5.

- topSpectralMatches:

  `numeric(1)` Maximum number of spectral matches per feature. Defaults
  to 5.

## Value

A data.frame or list of results, depending on `return.type`.

a data.frame with the summary of the results. Important column is the
*ApproximateConfidence* column, which give a score of how possible all
the identifications for this feature are. The *exactConfidence* column
is a score of how possible the top identification is.
