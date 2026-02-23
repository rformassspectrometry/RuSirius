# Spectra database matching

This function is to set up the parameter for matching to spectra
databases. This needs to be run first. Spectral library matching is
performed using the cosine score with squared peak intensities, ignoring
the precursor peak.

Note that spectral library matches are added as annotations to
CSI:FingerID results and do not influence the ranking of structure
candidates.

## Usage

``` r
spectraMatchingParam(
  spectraSearchDBs = c("BIO", "massbank"),
  peakDeviationPpm = 10,
  precursorDeviationPpm = 10,
  scoring = c("MODIFIED_COSINE", "INTENSITY", "GAUSSIAN")
)
```

## Arguments

- spectraSearchDBs:

  `character` vector of the databases to search. The default is
  `c("BIO", "massbank")`. Other values can be found by running
  `listDatabases()`.

- peakDeviationPpm:

  `numeric` Maximum allowed mass deviation in ppm for matching peaks.

- precursorDeviationPpm:

  `numeric` Maximum allowed mass deviation in ppm for matching the
  precursor. If not specified, the same value as for the peaks is used.

- scoring:

  `character` The scoring function to use. Possible values are
  `MODIFIED_COSINE`, `GAUSSIAN`, `INTENSITY`. The default is
  `MODIFIED_COSINE`.

## Value

An object of class `spectraMatchingParam`

## Note

For more information, see the Sirius
[documentation](https://v6.docs.sirius-ms.io/methods-background).

## Examples

``` r
# Example of setting up the parameters for spectra matching
param <- spectraMatchingParam(spectraSearchDBs = c("BIO", "massbank"),
                              peakDeviationPpm = 10,
                              precursorDeviationPpm = 10,
                              scoring = "MODIFIED_COSINE")
```
