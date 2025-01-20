#  RuSirius

IMPORTANT: THIS IS ACTIVELY DEVELOPED.

**RuSirius** is an R package that provides an interface to the Sirius software,
enabling seamless integration of **xcms** preprocessing results with Sirius.\
It is built upon **Rsirius**, the REST API library for the R programming
language, which you can find
[here](https://github.com/sirius-ms/sirius-client-openAPI/tree/master/client-api_r).

--------------------------------------------------------------------------------

## Installation Instructions

### Option 1: Install Sirius and R-api via Conda

You can install RuSirius using **conda** with the following command:

```bash
conda install conda-forge::r-sirius-ms
```
This will install both the Sirius software and the R API.
And then install RuSirius :

```r
library(remotes)
BiocManager::install("RforMassSpectrometry/RuSirius")
```

### Option 2: Manual Installation (Recommended for now)

Alternatively, you can manually install Sirius and then RuSirius:

1.  Download the Sirius software from the [official releases
    page](https://github.com/sirius-ms/sirius/releases). RuSirius is dependent
    on Sirius 6.1, please ensure you have the right version downloaded.
2.  Install the R API adn RuSirius by running the following code in R:

```r
library(remotes)
BiocManager::install("RforMassSpectrometry/RuSirius")
```

--------------------------------------------------------------------------------

## Usage

RuSirius comes with two vignettes to help you get started:

1.  **Importing Chromatographic Peaks from xcms**\
    This
    [vignette](https://github.com/rformassspectrometry/RuSirius/blob/main/vignettes/Chromatographic_peak_annotation_public_dataset.Rmd)
    demonstrates how to import chromatographic peaks from **xcms** and use them
    in Sirius.

2.  **Importing Features from xcms**\
    This
    [vignette](https://github.com/rformassspectrometry/RuSirius/blob/main/vignettes/Feature_annotation_private_dataset.qmd)
    shows how to import features (i.e., corresponding chromatographic peaks)
    from **xcms** and use them in Sirius.\
    *Note:* The second vignette is not fully reproducible yet, as the dataset
    used is not publicly available. However, the code can serve as a template
    for your use case. A public dataset will be added soon.

--------------------------------------------------------------------------------

## Testers Needed

We’re looking for testers to help refine and enhance the functionality of this
package, focusing on what users need most. Your feedback is crucial to ensuring
the package meets user expectations and operates smoothly.

### Key Areas for Testing:

1.  **Ease of Installation**\
    Start by testing the installation process. Let us know if you encounter any
    issues, especially with package dependencies.

2.  **Exploring Basic Functionalities**\
    Run the first vignette suggested above to familiarize yourself with the
    package’s core features. This will give you a solid foundation for
    understanding how it works.

3.  **Adapting to Your Dataset**\
    If you’re working with custom datasets or want to import features instead of
    chromatographic peaks, try the second vignette. It provides the necessary
    code and steps for more advanced use cases.

### Why Your Feedback Matters

The current implementation is in its early stages, focusing on basic
functionality. Your input is essential to making the integration between
**xcms** and Sirius seamless and user-friendly. Whether you have suggestions,
encounter issues, or need additional features, don’t hesitate to reach out. Your
insights will directly shape future updates.

--------------------------------------------------------------------------------

## Known Issues

This is the early stage of integrating Sirius with **RforMassSpectrometry**, and
there’s ongoing development to enhance the implementation.

### Current Issues:

-   **Spectral library matching**: This feature is not working as expected. The
    issue is being actively addressed.

-   Importing features can be time-consuming. To speed up testing, import only a
    few features at first and limit the process to one MS1 spectrum per feature.
    Further details are provided in the vignettes.

If you encounter other issues, please let us know so we can improve the package!

--------------------------------------------------------------------------------

## Thanks

Many many thanks to Markus Fleischauer for his work on the Sirius SDKs, many
more Jonas Emmert who made the R-API usable. And Marcus Ludwig for the help and
support in implementing RuSirius.
