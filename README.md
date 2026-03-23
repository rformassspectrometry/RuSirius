# RuSirius

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

**RuSirius** (spelled R-U-Sirius (*are you serious*)) is an R package that
provides an interface to the Sirius software, enabling seamless integration of
**xcms** preprocessing results with Sirius.\
It is built upon **RSirius**, the
REST API library for the R programming language, which you can find
[here](https://github.com/sirius-ms/sirius-client-openAPI/tree/master/client-api_r).

--------------------------------------------------------------------------------

## Installation Instructions

### Option 1: Install Sirius and R-api via Conda

1.  You can install both the Sirius software and the R API using **conda** with
    the following command:

```bash
conda create -n sirius conda-forge::r-sirius-ms
conda activate sirius
```

2.  Then, in an R session, install *RuSirius* and it's dependencies :

```r
library(remotes)
install_github("RforMassSpectrometry/RuSirius")
```

### Option 2: Manual Installation (Recommended for now)

Alternatively, you can manually install Sirius and then RuSirius:

1.  Download the Sirius software from the [official releases
    page](https://github.com/sirius-ms/sirius/releases). *RuSirius* is dependent
    on Sirius 6.3, please ensure you have the right version downloaded.
2.  Install *RuSirius* and it's dependencies (which includes the R-api) by
    running the following code in R:

``` r
library(remotes)
install_github("RforMassSpectrometry/RuSirius")
```

--------------------------------------------------------------------------------

## Usage

RuSirius comes with five vignettes to help you get started:

1.  **Getting Started with RuSirius**
    This
    [vignette](https://rformassspectrometry.github.io/RuSirius/articles/GettingStarted.html)
    covers the basics of connecting to Sirius and managing projects.

2.  **Importing Spectra into Sirius**
    This
    [vignette](https://rformassspectrometry.github.io/RuSirius/articles/ImportSpectra.html)
    shows a basic example for importing spectra data into Sirius.

3.  **Retrieving Results from Sirius**
    This
    [vignette](https://rformassspectrometry.github.io/RuSirius/articles/RetrievingResults.html)
    explains how to retrieve and interpret different result types.

4.  **Using Custom Databases**
    This
    [vignette](https://rformassspectrometry.github.io/RuSirius/articles/CustomDatabases.html)
    demonstrates how to create and use custom databases for structure search.

5.  **Importing Chromatographic Peaks from xcms**
    This
    [vignette](https://rformassspectrometry.github.io/RuSirius/articles/Chromatographic_peak_annotation_public_dataset.html)
    demonstrates how to import chromatographic peaks from **xcms** and use them
    in Sirius.

--------------------------------------------------------------------------------

## Contributing

We welcome contributions and bug reports! If you encounter issues, have
suggestions, or want to contribute code, please open an issue or pull request
on the [GitHub repository](https://github.com/RforMassSpectrometry/RuSirius).

Before submitting a PR, please ensure:

1.  **Tests pass locally**: Run tests **one file at a time** to avoid API
    conflicts. Running all tests together with `devtools::test()` can cause
    SIRIUS startup issues due to port conflicts:

    ```r
    testthat::test_file("tests/testthat/test-api-responses.R")
    testthat::test_file("tests/testthat/test-helpers.R")
    testthat::test_file("tests/testthat/test-import-helpers.R")
    testthat::test_file("tests/testthat/test-input-validation.R")
    testthat::test_file("tests/testthat/test-integration.R")
    testthat::test_file("tests/testthat/test-msn-import.R")
    testthat::test_file("tests/testthat/test-param-classes.R")
    ```
2.  **Pre-rendered vignettes are up to date**: All five vignettes are
    pre-rendered from `.Rmd.orig` sources. If your changes affect package
    output or vignette content, re-run the pre-render script from the package
    root with Sirius running on port 9999:

    ```r
    source("vignettes/pre-render.R")
    ```

    Commit the updated `.Rmd` files alongside your changes.

Note that CI runs `R CMD check` with `--no-vignettes` since vignettes require a
running Sirius instance with authentication.

--------------------------------------------------------------------------------
## Known Issues

### Current Issues:

-   **Vignettes require Sirius login**: Vignettes cannot be built automatically
    in CI as they require Sirius authentication. Pre-rendered vignettes are
    available on the [pkgdown site](https://rformassspectrometry.github.io/RuSirius/).

-   Importing features can be time-consuming. To speed up testing, import only a
    few features at first and limit the process to one MS1 spectrum per feature.
    Further details are provided in the vignettes.

--------------------------------------------------------------------------------

## Docker

A pre-built Docker image is available on Docker Hub, based on the official
Bioconductor docker image `bioconductor/bioconductor_docker:RELEASE_3_22`. It
includes *RuSirius*, all dependencies, and Sirius 6.3.3 (downloaded directly
from the official GitHub releases). The Sirius REST API starts automatically on
port 9999 when the container launches.

### Pulling the image

``` bash
docker pull rformassspectrometry/rusirius:latest
```

### Running the container

Start an RStudio Server session:

``` bash
docker run -e PASSWORD=yourpassword -p 8787:8787 rformassspectrometry/rusirius:latest
```

Then open RStudio Server at <http://localhost:8787> (user: `rstudio`, password:
as set above).

Or start an interactive R session directly:

``` bash
docker run -it rformassspectrometry/rusirius:latest R
```

### Sirius login

Sirius starts automatically inside the container on port 9999. Connect and log
in from the R console:

``` r
library(RuSirius)
srs <- Sirius(projectId = "my_project",
              username = "your_email",
              password = "your_password",
              port = 9999)
```

Note that Sirius requires a valid account — you can register for free at
<https://bright-giant.com>.

### Building the image locally (advanced)

If you prefer to build the image from source instead of pulling from Docker Hub:

``` bash
docker build -t rformassspectrometry/rusirius .
```

This builds the image using the `Dockerfile` in the repository root.

--------------------------------------------------------------------------------

## Thanks

Many many thanks to Markus Fleischauer for his work on the Sirius SDKs, many
more Jonas Emmert who made the R-API usable. And Marcus Ludwig for the help and
support in implementing RuSirius.
