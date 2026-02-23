# Using Custom Databases in Sirius

``` r
library(RSirius)
library(RuSirius)
library(MsDataHub)
library(Spectra)
```

## Introduction

**Note:** This vignette code is not evaluated during package checks as
it requires a running Sirius instance.

Sirius can search against custom databases in addition to the built-in
databases (BIO, PubChem, etc.). This is useful when you have:

- A list of suspect compounds specific to your study
- A custom spectral library (e.g., from MassBank)
- Target compounds you want to prioritize in the search

This vignette demonstrates how to create and use custom databases, and
shows the impact on structure identification results.

## Managing Databases

### Listing Available Databases

``` r
srs <- Sirius()

# List all searchable databases
dbs <- listDbs(srs)
dbs[, c("databaseId", "displayName")]
```

### Database Information

``` r
# Get details about a specific database
infoDb(srs, databaseId = "BIO")
```

## Creating a Custom Database

Custom databases can be created from files containing compound
information. Supported formats include `.tsv`, `.csv`, or `.mgf` files
with structure information.

### From a Compound List (TSV/CSV)

The file should contain columns for compound name, SMILES (or InChI),
and optionally the molecular formula.

``` r
# Create database from a TSV file
createDb(srs,
         databaseId = "my_suspects",
         files = "path/to/suspects.tsv",
         location = getwd())

# Verify it was created
listDbs(srs)
```

### From a Spectral Library (MGF)

Spectral libraries in MGF format can also be imported. An example MGF
file is included in the package:

``` r
# Path to example MassBank MGF file
mgf_file <- system.file("vignettes", "MASSBANKEU.mgf", package = "RuSirius")

createDb(srs,
         databaseId = "massbank_custom",
         files = mgf_file,
         location = getwd())
```

## Comparing Results: Default vs Custom Database

Let’s demonstrate how using a custom database affects structure
identification.

### Setup: Import Sample Data

``` r
# Load example data
dda_file <- MsDataHub::PestMix1_DDA.mzML()
sp <- Spectra(dda_file)
sp <- setBackend(sp, MsBackendMemory())
sp <- filterEmptySpectra(sp)

# Group spectra
idxs <- fragmentGroupIndex(sp)
sp$Msn_idx <- idxs

# Create project and import
srs <- Sirius(projectId = "db_comparison", path = getwd())
sp_subset <- sp[sp$Msn_idx %in% c(421, 707)]
srs <- import(srs, spectra = sp_subset, ms_column_name = "Msn_idx")
```

### Run with Default Database (BIO)

``` r
# Run structure search with BIO database only
run(srs,
    formulaIdParams = formulaIdParam(numberOfCandidates = 5),
    predictParams = predictParam(),
    structureDbSearchParams = structureDbSearchParam(
        structureSearchDbs = c("BIO")
    ),
    recompute = TRUE,
    wait = TRUE)

# Get results
results_bio <- summary(srs, result.type = "structure")
results_bio[, c("alignedFeatureId", "molecularFormula", 
                "structureName", "confidenceExactMatch")]
```

### Run with Custom Database Added

``` r
# Now include custom database in search
run(srs,
    formulaIdParams = formulaIdParam(numberOfCandidates = 5),
    predictParams = predictParam(),
    structureDbSearchParams = structureDbSearchParam(
        structureSearchDbs = c("BIO", "massbank_custom")
    ),
    recompute = TRUE,
    wait = TRUE)

# Get results with custom DB
results_custom <- summary(srs, result.type = "structure")
results_custom[, c("alignedFeatureId", "molecularFormula", 
                   "structureName", "confidenceExactMatch")]
```

### Compare Results

``` r
# Compare confidence scores
comparison <- merge(
    results_bio[, c("alignedFeatureId", "confidenceExactMatch")],
    results_custom[, c("alignedFeatureId", "confidenceExactMatch")],
    by = "alignedFeatureId",
    suffixes = c("_bio", "_custom")
)
comparison
```

Including relevant custom databases can improve identification
confidence when your compounds are well-represented in the custom
database.

## Removing a Database

``` r
# Remove a custom database when no longer needed
removeDb(srs, databaseId = "massbank_custom")

# Verify removal
listDbs(srs)
```

## Best Practices

1.  **Targeted databases**: Create focused databases with compounds
    relevant to your study rather than very large generic databases.

2.  **Quality over quantity**: Ensure your custom database has accurate
    structure information (SMILES/InChI).

3.  **Combine strategically**: Use custom databases alongside BIO for
    best coverage - BIO for general metabolites, custom for your
    specific targets.

4.  **Spectral libraries**: When available, spectral libraries (MGF)
    provide additional matching power through spectral similarity.

## Clean Up

``` r
shutdown(srs)
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
