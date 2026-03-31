---
title: 'RuSirius: An R Interface between the SIRIUS Mass Spectrometry Annotation Framework and The RforMassSpectrometry Ecosystem'
tags:
  - R
  - mass spectrometry
  - metabolomics
  - compound annotation
  - cheminformatics
  - LC-MS/MS
authors:
  - name: Philippine Louail
    # Add co-authors in alphabetical order.
  - name: Johannes Rainer
affiliations:
date: 19 March 2026
bibliography: paper.bib
---

# Summary

*RuSirius* is an R package that provides a programmatic interface between the
SIRIUS mass spectrometry annotation framework [@duhrkop_sirius_2019] and the
RforMassSpectrometry software ecosystem. SIRIUS is a widely used tool suite for
the computational annotation of tandem mass spectrometry (MS/MS) data,
encompassing molecular formula identification, compound class prediction,
structure database search, de novo structure generation, and spectral library
matching. The RforMassSpectrometry software ecosystem [@rainer_modular_2022]
provides the main infrastructure for handling and analyzing MS data in
R/Bioconductor.  *RuSirius* connects to SIRIUS through its REST API via the
*RSirius* client library <!-- TODO: add RSirius reference -->, enabling users to
control the full SIRIUS annotation pipeline from within R. By directly working
on common R MS objects and translating {converting?} them to SIRIUS-specific
data structures, the package allows users to build end-to-end metabolomics
workflows entirely within R.

# Statement of Need

Untargeted metabolomics experiments generate large volumes of liquid
chromatography–tandem mass spectrometry (LC-MS/MS) data that require
computational annotation methods to assign putative identities to detected
features. SIRIUS [@duhrkop_sirius_2019] is one of the most complete
open-source frameworks for this task, integrating several distinct annotation
tools. While SIRIUS provides a graphical user interface (GUI) and a
command-line interface in Java, neither integrates well into scripted,
reproducible data analysis workflows common in the R ecosystem. The R
programming language is widely used in metabolomics, with established
packages for data preprocessing (*xcms*), a powerful infrastructure for
efficient and scalable MS data handling (*Spectra*, *MsExperiment*), and
annotation (*MetaboAnnotation*, *CompoundDb*) all available as part of the
RforMassSpectrometry initiative [@rainer_modular_2022] and Bioconductor
[@huber_orchestrating_2015]. Comparable computational annotation methods are
however not available in R.

*RuSirius* addresses this gap by letting users import MS data as `Spectra`
objects, run the full SIRIUS annotation pipeline, and retrieve results as R
data frames — all from within a scripted R session. The target audience is
metabolomics researchers who already use *xcms* and the RforMassSpectrometry
ecosystem for data preprocessing and need to integrate SIRIUS annotations
into reproducible, end-to-end workflows. Finally, this integration also avoids
the need of development of similar, but redundant, methodology in R.

# State of the Field

Several tools exist for computational annotation of LC-MS/MS data, but few offer
direct R integration. The *metfrag* R package [@ruttkies_metfrag_2016] provides
in silico fragmentation-based annotation but does not cover formula
identification, compound classification, or de novo structure generation. GNPS
[@wang_sharing_2016] offers web-based spectral library matching and molecular
networking, but interaction from R requires manual file handling. The
*RMassBank* package [@stravs_automatic_2013] focuses on building spectral
libraries from reference standards rather than annotating unknowns. SIRIUS
provides the most comprehensive set of annotation tools available which include
SIRIUS for molecular formula identification via fragmentation tree computation
[@bocker_sirius_2009], ZODIAC for network-based formula re-ranking
[@ludwig_database-independent_2020], CSI:FingerID for molecular fingerprint
prediction and structure database search [@duhrkop_searching_2015], CANOPUS for
compound class prediction [@duhrkop_systematic_2021], and MSNovelist for de novo
structure generation [@stravs_msnovelist_2022], and spectral library matching.

More recently, the SIRIUS developers introduced *RSirius*, an R client library
for the SIRIUS REST API built on R6 classes. *RSirius* provides comprehensive
API coverage, but it does not integrate with the MS data structures used by the
RforMassSpectrometry initiative. These packages (anchored by *xcms*
[@louail_xcms_2025] and *Spectra* [@noauthor_open_2025; @rainer_modular_2022])
follow a modular design in which independent packages interoperate through
common data structures such as the `Spectra` object, enabling researchers to
freely combine preprocessing, annotation, and statistical analysis tools into
customizable and reproducible workflows. Rather than duplicating the API
bindings already provided by *RSirius*, *RuSirius* builds on top of it, adding
the higher-level, idiomatic R interface and data-structure integration that are
needed to connect SIRIUS with this ecosystem. Together with *SpectriPy*
[@graeve_spectripy_2025], *RuSirius* is part of ongoing RforMassSpectrometry
efforts to improve integration and interoperability between MS data analysis
tools across language platforms.

# Software Design

*RuSirius* is structured around a central S4 class, `Sirius`, which
encapsulates the connection to a running SIRIUS instance and tracks project
state. This design reflects a key architectural trade-off: rather than
reimplementing SIRIUS algorithms in R, *RuSirius* delegates computation to
the SIRIUS server via its REST API and focuses on providing idiomatic R
input/output. This keeps the package lightweight and ensures that users always
run the same validated algorithms as the SIRIUS GUI or CLI.

The main user-facing functions follow the typical annotation workflow:

1. **`Sirius()`** establishes a connection, optionally authenticating and
   opening a project.
2. **`import()`** converts `Spectra` objects into SIRIUS feature imports,
   handling MS level grouping, adduct normalization, and chunked upload for
   large datasets.
3. **`run()`** submits annotation jobs with configurable parameter objects
   (`formulaIdParam`, `zodiacParam`, `predictParam`, `structureDbSearchParam`,
   `deNovoStructureParam`, `spectraMatchingParam`) that map directly to SIRIUS
   tool configurations. Prerequisite dependencies between tools are resolved
   automatically (e.g., requesting structure search enables fingerprint
   prediction and formula identification).
4. **`results()`** and **`summary()`** retrieve annotation results as data
   frames, supporting formula candidates, structure candidates, compound
   classes, de novo structures, spectral matches, and fragmentation trees.
5. **Database management** functions (`listDbs()`, `createDb()`, `removeDb()`)
   allow users to create and query custom spectral and structure databases.

Throughout this workflow, *RuSirius* maintains a mapping between user-facing
feature identifiers and SIRIUS-internal feature IDs, allowing seamless
round-tripping between the R session and the SIRIUS project. 
<!-- NOTE: the previous paragraph could be dropped if text needs to be short -->

A Docker image based on the official Bioconductor image is provided, bundling
SIRIUS, *RSirius*, and *RuSirius* with an automatically started REST API
server, enabling reproducible deployment without manual installation.

# Research Impact Statement

*RuSirius* has been developed as part of the RforMassSpectrometry initiative
and is hosted under the *RforMassSpectrometry* GitHub organization.
The package was developed in close collaboration with the current SIRIUS
developers, who recognized the need for an interoperable bridge between
*RSirius* and the RforMassSpectrometry software ecosystem. As documented
by the issue tracker on the GitHub repository, end-user feedback has
guided the implementation throughout development. The
package has been used internally for metabolomics annotation
workflows at Eurac Research and applied in a ring trial analysis
<!-- TODO: add ring trial repo reference (zenodo) -->. Its use within a
complete end-to-end metabolomics workflow is demonstrated in the `metabonaut`
website <!-- TODO: add Zenodo reference for metabonaut -->, which applies
*RuSirius* to real-world data analysis alongside *xcms* and other
RforMassSpectrometry packages. Its integration with *xcms* and *Spectra*
positions it as a seamless annotation step for the thousands of researchers
already using these packages for LC-MS/MS data processing.

# AI Usage Disclosure

All content was written, reviewed, edited, and validated by the
human authors. The core software design decisions, implementation,
and scientific framing are entirely the work of the authors. Claude
was used for reducing text length, for ensuring proper English
phrasing and correcting typos.

# Availability

*RuSirius* is available on GitHub at
<https://github.com/RforMassSpectrometry/RuSirius> under the Artistic-2.0
license. Documentation for all exported functions and five vignettes
covering the main workflows are available at
<https://rformassspectrometry.github.io/RuSirius/>. The `metabonaut`
website <!-- TODO: add Zenodo reference for metabonaut --> demonstrates
its use within a complete end-to-end workflow. A Docker image with
SIRIUS and all dependencies pre-installed is available on Docker Hub at
*rformassspectrometry/rusirius*.

# Acknowledgements

<!-- TODO: add HUMAN doctoral network funding information -->

# References
