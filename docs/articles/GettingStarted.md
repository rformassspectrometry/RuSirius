# Getting Started with RuSirius

``` r
library(RSirius)
library(RuSirius)
```

## Introduction

**RuSirius** provides an R interface to the
[Sirius](https://bio.informatik.uni-jena.de/software/sirius/) mass
spectrometry software for metabolite identification. This vignette
covers the basics of connecting to Sirius and managing projects.

**Note:** This vignette code is not evaluated during package checks. To
run it interactively, ensure Sirius 6.3 is installed and running.

## Prerequisites

Before using RuSirius, you need:

1.  **Sirius 6.3** installed on your system. Download from the [official
    releases page](https://github.com/sirius-ms/sirius/releases).
2.  A **Sirius account** (free registration at
    [bright-giant.com](https://bright-giant.com/))

## Connecting to Sirius

The
[`Sirius()`](https://rformassspectrometry.github.io/RuSirius/reference/Sirius.md)
function creates a connection to the Sirius application. If Sirius is
not running, it will attempt to start it automatically.

``` r
# Basic connection - Sirius will start if not running
srs <- Sirius()
srs
```

If you have credentials, you can log in during connection:

``` r
srs <- Sirius(
    username = "your_email@example.com",
    password = "your_password"
)
```

## Checking Connection Status

You can verify your connection is valid:

``` r
# Check if connected
checkConnection(srs)

# View connection details
srs
```

## Managing Projects

Sirius organizes data into projects. You can create new projects or open
existing ones.

### Creating/Opening a Project

``` r
# Create a new project (or open if it exists)
srs <- Sirius(
    projectId = "my_analysis",
    path = getwd()  
)

# Or open a different project later
srs <- openProject(srs, projectId = "another_project", path = getwd())
```

The project file will be saved as `my_analysis.sirius` in the specified
path.

### Project Information

``` r
# Get project details
projectInfo(srs)

# List all open projects
listOpenProjects(srs)
```

## Working with Features

After importing data (see the “Importing Spectra” vignette), you can
manage features:

``` r
# List feature IDs
featuresId(srs)

# Get feature information
featuresInfo(srs)

# Get the mapping between your IDs and Sirius IDs
srs@featureMap
```

## Logging In

If you didn’t provide credentials at connection time:

``` r
srs <- logIn(srs, username = "your_email@example.com", password = "your_password")
```

## Using the GUI

You can open the Sirius graphical interface for visual exploration:

``` r
# Open the GUI
openGUI(srs)

# Close the GUI when done
closeGUI(srs)
```

## Shutting Down

Always properly close your connection when finished:

``` r
# Close project and shutdown
shutdown(srs)

# Or just close the project without shutting down Sirius
shutdown(srs, closeProject = TRUE)
```

## Utility Functions

RuSirius provides several utility functions. See
[`?utils`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md)
for the full list:

- [`checkConnection()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md) -
  Verify connection status
- [`logIn()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md) -
  Log in to Sirius
- [`openProject()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md)
  /
  [`listOpenProjects()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md) -
  Project management
- [`projectInfo()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md) -
  Get project details
- [`featuresId()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md)
  /
  [`featuresInfo()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md) -
  Feature management
- [`deleteFeatures()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md) -
  Remove features
- [`mapFeatures()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md) -
  Get ID mappings
- [`openGUI()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md)
  /
  [`closeGUI()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md) -
  GUI control
- [`shutdown()`](https://rformassspectrometry.github.io/RuSirius/reference/utils.md) -
  Clean shutdown

## Next Steps

- See the **“Importing Spectra into Sirius”** vignette for importing MS
  data
- See
  [`?run`](https://rformassspectrometry.github.io/RuSirius/reference/run.md)
  for running Sirius computations (formula ID, structure search, etc.)
- See
  [`?results`](https://rformassspectrometry.github.io/RuSirius/reference/results.md)
  for retrieving annotation results

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
