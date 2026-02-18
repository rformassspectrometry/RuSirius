# RuSirius

IMPORTANT: THIS IS ACTIVELY DEVELOPED.

**RuSirius** is an R package that provides an interface to the Sirius software,
enabling seamless integration of **xcms** preprocessing results with Sirius.\
It is built upon **RSirius**, the REST API library for the R programming
language, which you can find
[here](https://github.com/sirius-ms/sirius-client-openAPI/tree/master/client-api_r).

--------------------------------------------------------------------------------

## Installation Instructions

### Option 1: Install Sirius and R-api via Conda

1.  You can install both the Sirius software and the R API using **conda** with
    the following command:

``` bash
conda install conda-forge::r-sirius-ms
```

2.  Then install RuSirius and it's dependencies :

``` r
library(remotes)
BiocManager::install("RforMassSpectrometry/RuSirius")
```

### Option 2: Manual Installation (Recommended for now)

Alternatively, you can manually install Sirius and then RuSirius:

1.  Download the Sirius software from the [official releases
    page](https://github.com/sirius-ms/sirius/releases). RuSirius is dependent
    on Sirius 6.3, please ensure you have the right version downloaded.
2.  Install RuSirius and it's dependencies (which includes the R-api) by running
    the following code in R:

``` r
library(remotes)
BiocManager::install("RforMassSpectrometry/RuSirius")
```

--------------------------------------------------------------------------------

## Usage

RuSirius comes with five vignettes to help you get started:

1.  **Getting Started with RuSirius**
    This
    [vignette](https://github.com/rformassspectrometry/RuSirius/blob/main/vignettes/GettingStarted.Rmd)
    covers the basics of connecting to Sirius and managing projects.

2.  **Importing Spectra into Sirius**
    This
    [vignette](https://github.com/rformassspectrometry/RuSirius/blob/main/vignettes/ImportSpectra.Rmd)
    shows a basic example for importing spectra data into Sirius.

3.  **Retrieving Results from Sirius**
    This
    [vignette](https://github.com/rformassspectrometry/RuSirius/blob/main/vignettes/RetrievingResults.Rmd)
    explains how to retrieve and interpret different result types.

4.  **Using Custom Databases**
    This
    [vignette](https://github.com/rformassspectrometry/RuSirius/blob/main/vignettes/CustomDatabases.Rmd)
    demonstrates how to create and use custom databases for structure search.

5.  **Importing Chromatographic Peaks from xcms**
    This
    [vignette](https://github.com/rformassspectrometry/RuSirius/blob/main/vignettes/Chromatographic_peak_annotation_public_dataset.Rmd)
    demonstrates how to import chromatographic peaks from **xcms** and use them
    in Sirius.

--------------------------------------------------------------------------------

## Testers Needed

We’re looking for testers to help refine and enhance the functionality of this
package, focusing on what users need most. Your feedback is crucial to ensuring
the package meets user expectations and operates smoothly.

### Key Areas for Testing:

1.  **Ease of Installation**
    Start by testing the installation process. Let us know if you encounter any
    issues, especially with package dependencies.

2.  **Exploring Basic Functionalities**
    Run the first two vignettes to familiarize yourself with connecting to Sirius
    and importing spectra data. This will give you a solid foundation for
    understanding how the package works.

3.  **Adapting to Your Dataset**
    If you're working with custom datasets or want to import chromatographic peaks
    from xcms, try the third vignette. It provides the necessary code and steps
    for more advanced use cases.

### Why Your Feedback Matters

The current implementation is in its early stages, focusing on basic
functionality. Your input is essential to making the integration between
**xcms** and Sirius seamless and user-friendly. Whether you have suggestions,
encounter issues, or need additional features, don’t hesitate to reach out. Your
insights will directly shape future updates.

--------------------------------------------------------------------------------

## Known Issues

This is the early stage of integrating Sirius with **RforMassSpectrometry**, and
there's ongoing development to enhance the implementation.

### Current Issues:

-   **GHA and R command checks**: They are failing/incomplete because the
    vignettes cannot be run systematically (requires Sirius login).

-   **Docker image**: A Dockerfile is available to set up the environment, but
    vignettes cannot be run automatically as they require Sirius authentication.
    See the [Docker](#docker) section below for usage instructions.

-   Importing features can be time-consuming. To speed up testing, import only a
    few features at first and limit the process to one MS1 spectrum per feature.
    Further details are provided in the vignettes.

If you encounter other issues, please let us know so we can improve the package!

--------------------------------------------------------------------------------

## Docker

A Docker image is provided based on `bioconductor/bioconductor_docker:RELEASE_3_22`.
It includes RuSirius, all dependencies, and the Sirius 6.3 software (installed
via conda).

### Building the image

``` bash
docker build -t rformassspectrometry/rusirius .
```

### Running the container

``` bash
docker run -e PASSWORD=yourpassword -p 8787:8787 rformassspectrometry/rusirius
```

Then open RStudio Server at <http://localhost:8787> (user: `rstudio`, password:
as set above).

### Sirius login

Vignettes require an active Sirius session. After starting the container, log
in from the R console:

``` r
library(RuSirius)
srs <- Sirius(projectId = "my_project",
              username = "your_email",
              password = "your_password")
```

Note that Sirius requires a valid account — you can register for free at
<https://bright-giant.com>.

--------------------------------------------------------------------------------

## Thanks

Many many thanks to Markus Fleischauer for his work on the Sirius SDKs, many
more Jonas Emmert who made the R-API usable. And Marcus Ludwig for the help and
support in implementing RuSirius.
