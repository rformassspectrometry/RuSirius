# RuSiRius 

**RuSirius** is an R package that provides an interface to the Sirius software, 
enabling seamless integration of **xcms** preprocessing results with Sirius.  
It is built upon **Rsirius**, the REST API library for the R programming 
language, which you can find [here](https://github.com/sirius-ms/sirius-client-openAPI/tree/master/client-api_r).

---

## Installation Instructions 

### Option 1: Install via Conda
You can install RuSirius using **conda** with the following command:

```bash
conda install conda-forge::r-sirius-ms
```

This will install both the Sirius software and the R API.

### Option 2: Manual Installation
Alternatively, you can manually install the RuSirius R package:

1. Download the Sirius software from the [official releases page](https://github.com/sirius-ms/sirius/releases).
2. Install the R API by running the following code in R:

```r
library(remotes)
install_github(repo = "sirius-ms/sirius-client-openAPI", 
               subdir = "client-api_r/generated", 
               ref = "master", 
               build = TRUE)
```

---

## Usage

RuSirius comes with two vignettes to help you get started:

1. **Importing Chromatographic Peaks from xcms**  
   [Insert link]  
   This vignette demonstrates how to import chromatographic peaks from **xcms** 
   and use them in Sirius.

2. **Importing Features from xcms**  
   [Insert link]  
   This vignette shows how to import features (i.e., chromatographic peaks) 
   from **xcms** and use them in Sirius.  
   *Note:* The second vignette is not fully reproducible yet, as the dataset 
   used is not publicly available. However, the code can serve as a template 
   for your use case. A public dataset will be added soon.

---

## Testers Needed

Right now, the focus is on building functionalities that users need most.  

The current implementations are basic, and your feedback is invaluable in 
making **xcms** and Sirius integration easier.  
If you have suggestions or specific needs, don't hesitate to ask!

---

## Known Issues

This is the early stage of integrating Sirius with **RforMassSpectrometry**,
and there’s ongoing development to enhance the implementation.  

### Current Issues:


- **Spectral library matching**: This feature is not working as expected. 
  The issue is being actively addressed.

- Importing features can be time-consuming. To speed up testing, import only a
  few features at first and limit the process to one MS1 spectrum per feature. 
  Further details are provided in the vignettes.

If you encounter other issues, please let us know so we can improve the package!

---


# RuSiRius 

**RuSirius** is an R package that provides an interface to the Sirius software, enabling seamless integration of **xcms** preprocessing results with Sirius.  
It is built upon **Rsirius**, the REST API library for the R programming language, which you can find [here](https://github.com/sirius-ms/sirius-client-openAPI/tree/master/client-api_r).

---

## Installation Instructions 

### Option 1: Install via Conda
You can install RuSirius using **conda** with the following command:

```bash
conda install conda-forge::r-sirius-ms
```

This will install both the Sirius software and the R API.

### Option 2: Manual Installation
Alternatively, you can manually install the RuSirius R package:

1. Download the Sirius software from the [official releases page](https://github.com/sirius-ms/sirius/releases).
2. Install the R API and RuSirius package by running the following code in R:

```r
library(remotes)
BiocManager::install("RforMassSpectrometry/RuSirius")
```

---

## Usage

RuSirius comes with two vignettes to help you get started:

1. **Importing Chromatographic Peaks from xcms**  
   [Insert link]  
   This vignette demonstrates how to import chromatographic peaks from **xcms** and use them in Sirius.

2. **Importing Features from xcms**  
   [Insert link]  
   This vignette shows how to import features (i.e., chromatographic peaks) from **xcms** and use them in Sirius.  
   *Note:* The second vignette is not fully reproducible yet, as the dataset used is not publicly available. However, the code can serve as a template for your use case. A public dataset will be added soon.

### Important Notes:
- Importing features can be time-consuming. To speed up testing, import only a few features at first and limit the process to one MS1 spectrum per feature. Further details are provided in the vignettes.

---

## Testers Needed

Right now, the focus is on building functionalities that users need most.  

The current implementations are basic, and your feedback is invaluable in making **xcms** and Sirius integration easier.  
If you have suggestions or specific needs, don't hesitate to ask!

---

## Known Issues

This is the early stage of integrating Sirius with **RforMassSpectrometry**, and there’s ongoing development to enhance the implementation.  

### Current Issues:
- **Spectral library matching**: This feature is not working as expected. The issue is being actively addressed.

If you encounter other issues, please let us know so we can improve the package!

---

## Thanks 

Many many thanks to Markus Fleischauer for his work on the Sirius SDKs, many more Jonas Emmert who made the R-API usable. And Marcus Ludwig for the help and support in implementing RuSirius.