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
    orcid: 0009-0007-5429-6846
    affiliation: "1, 2"
  - name: Nicola Dalla Valle
    orcid: 0009-0001-6696-8108
    affiliation: 3
  - name: Jonas A. Emmert
    orcid: 0009-0008-0632-7211
    affiliation: "2, 4"
  - name: Markus Fleischauer
    orcid: 0000-0001-7557-0831
    affiliation: "2, 5"
  - name: Mar Garcia-Aloy
    orcid: 0000-0002-1330-6610
    affiliation: 6
  - name: Marcus Ludwig
    orcid: 0000-0001-9981-2153
    affiliation: 5
  - name: Adriano Rutz
    orcid: 0000-0003-0443-9902
    affiliation: 7
  - name: Sebastian Böcker
    orcid: 0000-0002-9304-8091
    affiliation: 2
  - name: Johannes Rainer
    orcid: 0000-0002-6977-7147
    affiliation: 1
affiliations:
  - name: Institute for Biomedicine, Eurac Research, 39100 Bolzano, Italy
    index: 1
  - name: Chair for Bioinformatics, Institute for Computer Science, Friedrich Schiller University Jena, Jena, Germany
    index: 2
  - name: University of Trento, 38100 Trento, TN, Italy
    index: 3
  - name: International Max Planck Research School "Chemical Communication in Ecological Systems", Max Planck Institute for Chemical Ecology, Jena, Germany
    index: 4
  - name: Bright Giant GmbH, Jena, Germany
    index: 5
  - name: Metabolomics Unit, Research and Innovation Centre, Fondazione Edmund Mach, 38098 San Michele all'Adige (TN), Italy
    index: 6
  - name: Institute for Molecular Systems Biology, ETH Zürich, Otto-Stern-Weg 3, 8093 Zürich, Switzerland
    index: 7
date: 19 March 2026
bibliography: paper.bib
---

# Summary

*RuSirius* is an R package that provides a programmatic interface between the
SIRIUS mass spectrometry annotation framework [@duhrkop_sirius_2019] and the
RforMassSpectrometry software ecosystem. SIRIUS is a widely used tool suite for
the computational annotation of tandem mass spectrometry (MS/MS) data,
encompassing molecular formula identification, compound class prediction,
structure database search, *de novo* structure generation, and spectral library
matching. The RforMassSpectrometry software ecosystem [@rainer_modular_2022]
provides the main infrastructure for handling and analyzing MS data in
R/Bioconductor.  *RuSirius* connects to SIRIUS through its REST API via the
[*RSirius* client library](https://github.com/sirius-ms/sirius-client-openAPI), enabling users to
control the full SIRIUS annotation pipeline from within R. By directly working
on common R MS objects and converting them to SIRIUS-specific
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
packages for data preprocessing (*xcms* [@louail_xcms_2025]), a powerful infrastructure for
efficient and scalable MS data handling (*Spectra* [@rainer_open_2026], *MsExperiment*), and
annotation (*MetaboAnnotation*, *CompoundDb*) all available as part of the
RforMassSpectrometry initiative [@rainer_modular_2022] and Bioconductor
[@huber_orchestrating_2015]. Comparable computational annotation methods are
however not available in R.

*RuSirius* addresses this gap by letting users import MS data as `Spectra`
objects, run the full SIRIUS annotation pipeline, and retrieve results as R
data frames — all from within a scripted R session. The targeted *RuSirius*
users are
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
[@bocker_fragmentation_2016], ZODIAC for network-based formula re-ranking
[@ludwig_database-independent_2020], CSI:FingerID for molecular fingerprint
prediction and structure database search [@duhrkop_searching_2015], CANOPUS for
compound class prediction [@duhrkop_systematic_2021], and MSNovelist for de novo
structure generation [@stravs_msnovelist_2022], and spectral library matching.

More recently, the SIRIUS developers introduced *RSirius*, an R client library
for the SIRIUS REST API built on R6 classes. *RSirius* provides comprehensive
API coverage, but it does not integrate with common MS data structures in R such
as the one defined by the
RforMassSpectrometry initiative. This software ecosystem (anchored by *xcms*
[@louail_xcms_2025] and *Spectra* [@rainer_open_2026; @rainer_modular_2022])
follows a modular design in which independent packages interoperate through
common data structures such as the `Spectra` object, enabling researchers to
freely combine preprocessing, annotation, and statistical analysis tools into
customizable and reproducible workflows. Rather than duplicating the API
bindings already provided by *RSirius*, *RuSirius* builds on top of it, adding
the higher-level, idiomatic R interface and data-structure integration that are
needed to connect SIRIUS with this ecosystem. Together with *SpectriPy*
[@graeve_spectripy_2025], *RuSirius* is part of ongoing RforMassSpectrometry
efforts to improve integration and interoperability between MS data analysis
tools across programming languages and platforms.

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
   classes, *de novo* structures, spectral matches, and fragmentation trees.
5. **Database management** functions (`listDbs()`, `createDb()`, `removeDb()`)
   allow users to create and query custom spectral and structure databases.

Throughout this workflow, *RuSirius* maintains a mapping between user-facing
feature identifiers and SIRIUS-internal feature IDs, allowing seamless
round-tripping between the R session and the SIRIUS project. 


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
workflows at Eurac Research and Fondazione Edmund Mach, and applied in a ring trial analysis
[@philippine_louail_philouailhuman_ring_trial_2026]. Its use within a
complete end-to-end metabolomics workflow is demonstrated in the *Metabonaut*
website [@philippine_louail_rformassspectrometrymetabonaut_2026], which applies
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
license. Documentation for all exported functions and five tutorials (package vignettes)
covering the main workflows are available at
<https://rformassspectrometry.github.io/RuSirius>. The *Metabonaut*
website [@philippine_louail_rformassspectrometrymetabonaut_2026] demonstrates
its use within a complete end-to-end workflow. A Docker image with
SIRIUS and all dependencies pre-installed is available on Docker Hub at
*rformassspectrometry/rusirius*.

# Acknowledgements

This work is funded by the European Union under the HORIZON-MSCA-2021 project
101073062: HUMAN – Harmonising and Unifying Blood Metabolic Analysis Networks.

# Conflicts of Interest

M.F., M.L., and S.B. are co-founders of Bright Giant GmbH.

# References
