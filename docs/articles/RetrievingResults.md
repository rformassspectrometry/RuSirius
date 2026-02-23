# Retrieving Results from Sirius

``` r
library(RSirius)
library(RuSirius)
```

## Introduction

**Note:** This vignette code is not evaluated during package checks as
it requires a running Sirius instance with completed computations.

After running Sirius computations (see the “Importing Spectra”
vignette), you can retrieve various types of results. RuSirius provides
two main functions:

- [`summary()`](https://rformassspectrometry.github.io/RuSirius/reference/results.md) -
  Quick overview of top results per feature
- [`results()`](https://rformassspectrometry.github.io/RuSirius/reference/results.md) -
  Detailed results with multiple candidates

## Connecting to a Project

First, connect to an existing Sirius project that has completed
computations:

``` r
srs <- Sirius(projectId = "my_analysis", path = getwd())
```

## Quick Summary with `summary()`

The
[`summary()`](https://rformassspectrometry.github.io/RuSirius/reference/results.md)
function provides the top annotation for each feature. This is useful
for a quick overview of your results.

### Formula Identification Summary

``` r
# Top formula candidates
summary_formula <- summary(srs, result.type = "formulaId")
head(summary_formula)
```

Key columns include `confidenceApproxMatch` (overall confidence for the
feature) and `confidenceExactMatch` (confidence for the top hit).

### Structure Database Summary

``` r
# Top structure hits
summary_structure <- summary(srs, result.type = "structure")
```

### De Novo Structure Summary

``` r
# Top MSNovelist predictions
summary_denovo <- summary(srs, result.type = "deNovo")
```

### Spectral Library Match Summary

``` r
# Best spectral matches
summary_spectral <- summary(srs, result.type = "spectralDbMatch")
```

## Detailed Results with `results()`

The
[`results()`](https://rformassspectrometry.github.io/RuSirius/reference/results.md)
function returns multiple candidates per feature, giving you more
options to explore.

### Formula Candidates

``` r
# Get top 5 formula candidates per feature
formulas <- results(srs, 
                    result.type = "formulaId",
                    topFormula = 5,
                    return.type = "data.frame")
head(formulas)
```

### Structure Database Results

``` r
# Get structure candidates for each formula
structures <- results(srs,
                      result.type = "structureDb",
                      topFormula = 3,
                      topStructure = 5,
                      return.type = "data.frame")
```

Key columns: `inchiKey`, `smiles`, `structureName`, `csiScore`.

### Compound Class Predictions (CANOPUS)

``` r
# Get predicted compound classes
classes <- results(srs,
                   result.type = "compoundClass",
                   topFormula = 1,
                   return.type = "data.frame")
```

Returns ClassyFire and NPC classifications with confidence scores.

### De Novo Structures (MSNovelist)

``` r
# Get de novo predicted structures
denovo <- results(srs,
                  result.type = "deNovo",
                  topFormula = 1,
                  topStructure = 5,
                  return.type = "data.frame")
```

### Spectral Library Matches

``` r
# Get spectral library matches
spectral <- results(srs,
                    result.type = "spectralDbMatch",
                    topSpectralMatches = 5,
                    return.type = "data.frame")
```

### Fragmentation Trees

``` r
# Get fragmentation tree data
fragtrees <- results(srs,
                     result.type = "fragTree",
                     topFormula = 1,
                     return.type = "data.frame")
```

## Filtering by Feature

You can retrieve results for specific features only:

``` r
# Get feature IDs
feature_ids <- featuresId(srs)

# Get results for first two features only
subset_results <- results(srs,
                          features = feature_ids[1:2],
                          result.type = "structureDb",
                          return.type = "data.frame")
```

## Return Types

Results can be returned as a data.frame (default) or as a list:

``` r
# As list - useful for per-feature processing
results_list <- results(srs,
                        result.type = "formulaId",
                        return.type = "list")

# Access results for a specific feature
results_list[["feature_id_here"]]
```

## Mapping to Original IDs

If you imported data from xcms, results include the original xcms
feature IDs in the `xcms_fts` column for easy mapping back to your
original data.

``` r
# Get the feature mapping
mapFeatures(srs)
```

## Session Info

``` r
sessionInfo()
```

    ## R version 4.5.2 (2025-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 26200)
    ## 
    ## Matrix products: default
    ##   LAPACK version 3.12.1
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/Paris
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] BiocStyle_2.38.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] digest_0.6.39       desc_1.4.3          R6_2.6.1           
    ##  [4] bookdown_0.46       fastmap_1.2.0       xfun_0.56          
    ##  [7] cachem_1.1.0        knitr_1.51          htmltools_0.5.9    
    ## [10] rmarkdown_2.30      lifecycle_1.0.5     cli_3.6.5          
    ## [13] sass_0.4.10         pkgdown_2.2.0       textshaping_1.0.4  
    ## [16] jquerylib_0.1.4     systemfonts_1.3.1   compiler_4.5.2     
    ## [19] tools_4.5.2         ragg_1.5.0          bslib_0.10.0       
    ## [22] evaluate_1.0.5      yaml_2.3.12         BiocManager_1.30.27
    ## [25] otel_0.2.0          jsonlite_2.0.0      rlang_1.1.7        
    ## [28] fs_1.6.6            htmlwidgets_1.6.4
