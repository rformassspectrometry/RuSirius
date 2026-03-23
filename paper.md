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
    affiliation: 1
    corresponding: true
  - name: Jonas Emmert (?)
    orcid: # TODO: add ORCID
    affiliation: 2
  - name: Markus Fleischauer (?)
    orcid: # TODO: add ORCID
    affiliation: 2
  - name: Adriano Rutz
    orcid: # TODO: add ORCID
    affiliation: 3
  - name: Nils (?)
  - name: Mar
  - name: Johannes Rainer
affiliations:
  - name: Institute for Biomedicine, Eurac Research, Bolzano, Italy
    index: 1
    ror: # TODO: add ROR
  - name: # TODO: add affiliation for Emmert and Fleischauer (e.g. Friedrich Schiller University Jena / Bright Giant GmbH)
    index: 2
    ror: # TODO: add ROR
  - name: # TODO: add affiliation for Rutz
    index: 3
    ror: # TODO: add ROR
date: 19 March 2026
bibliography: paper.bib
---

# Summary

`RuSirius` is an R package that provides a programmatic interface
between the SIRIUS mass spectrometry annotation framework
[@duhrkop_sirius_2019] and the RforMassSpectrometry ecosystem. SIRIUS is a
widely used tool suite for the computational annotation of tandem mass
spectrometry (MS/MS) data, encompassing molecular formula
identification, compound class prediction, structure database search,
de novo structure generation, and spectral library matching.
`RuSirius` connects to SIRIUS through its REST API via the `RSirius`
client library <!-- TODO: add RSirius reference -->, enabling users
to control the full SIRIUS annotation pipeline from within R. The
package integrates natively with the RforMassSpectrometry ecosystem
[@rainer_modular_2022], accepting `Spectra` [@noauthor_open_2025] objects
as input and supporting direct import of chromatographic peak data from
`xcms` [@smith_xcms_2006; @tautenhahn_highly_2008; @louail_xcms_2025]. This design allows researchers to
build end-to-end metabolomics workflows entirely within R, from raw
data preprocessing through compound identification.

# Statement of Need

Untargeted metabolomics experiments generate large volumes of liquid
chromatography–tandem mass spectrometry (LC-MS/MS) data that require
computational annotation to assign putative identities to detected
features. SIRIUS [@duhrkop_sirius_2019] is one of the most comprehensive
open-source frameworks for this task, integrating several annotation
tools: SIRIUS for molecular formula identification via fragmentation
tree computation [@bocker_sirius_2009], ZODIAC for network-based formula
re-ranking [@ludwig_database-independent_2020], CSI:FingerID for molecular fingerprint
prediction and structure database search [@duhrkop_searching_2015], CANOPUS for
compound class prediction [@duhrkop_systematic_2021], MSNovelist for de novo
structure generation [@stravs_msnovelist_2022], and spectral library matching.

While SIRIUS provides a graphical user interface (GUI) and a
command-line interface in Java, neither integrates well into scripted,
reproducible data analysis workflows common in the R ecosystem. The R
programming language is widely used in metabolomics, with established
packages for data preprocessing (`xcms`), spectral handling
(`Spectra`, `MsExperiment`), and annotation (`MetaboAnnotation`)
available through the RforMassSpectrometry initiative [@rainer_modular_2022]
and Bioconductor [@huber_orchestrating_2015].

Recognizing the need for integration with other language platforms,
the SIRIUS developers provide `RSirius`, an auto-generated R client
library that wraps the SIRIUS REST API. While `RSirius` exposes the
full API surface, it is built on R6 classes and follows the structure
of the underlying REST endpoints rather than conventional R idioms.
This makes it powerful but difficult to use for researchers who are
not familiar with API-centric programming, and it does not
interoperate directly with existing RforMassSpectrometry data
structures.

`RuSirius` fills this gap by providing a native R interface to the SIRIUS
REST API. Users can:

- **Connect** to a running SIRIUS instance and manage projects
  programmatically.
- **Import** spectral data directly from `Spectra` objects, including MS1,
  MS2, and MSn data, with support for grouped chromatographic peaks from
  `xcms`.
- **Configure and run** annotation jobs covering formula identification,
  fingerprint prediction, structure search, de novo annotation, spectral
  matching, and compound class prediction.
- **Retrieve results** as R data frames, enabling downstream statistical
  analysis and visualization without leaving the R environment.
- **Manage custom databases** for structure search.

By bridging SIRIUS and the RforMassSpectrometry ecosystem,
`RuSirius` enables fully scripted and reproducible metabolomics
annotation pipelines. A Docker image is provided to simplify
deployment and ensure reproducibility in computational environments.

The package includes comprehensive documentation for all exported
functions and five educational vignettes covering the main workflows,
available on the package website. Its use within a larger end-to-end
analysis is demonstrated in the `metabonaut` tutorial
<!-- TODO: add Zenodo reference for metabonaut -->.

# State of the Field

Several tools exist for computational annotation of LC-MS/MS data, but few
offer direct R integration. The `metfrag` R package [@ruttkies_metfrag_2016] provides
in silico fragmentation-based annotation but does not cover formula
identification, compound classification, or de novo structure generation.
GNPS [@wang_sharing_2016] offers web-based spectral library matching and molecular
networking, but interaction from R requires manual file handling. The
`RMassBank` package [@stravs_automatic_2013] focuses on building spectral libraries
from reference standards rather than annotating unknowns. SIRIUS itself has
been accessible via the command line, but scripting against CLI output is
fragile and version-dependent.

More recently, the SIRIUS developers introduced `RSirius`, an auto-generated
R client for the SIRIUS REST API. While `RSirius` provides comprehensive API
coverage, it operates at a low level and does not integrate with the data
structures used by the RforMassSpectrometry ecosystem. This integration
matters because the RforMassSpectrometry suite (anchored by `xcms`
[@smith_xcms_2006; @tautenhahn_highly_2008; @louail_xcms_2025] and
`Spectra` [@noauthor_open_2025; @rainer_modular_2022]) follows a modular
design in which independent packages interoperate through shared data
structures. Unlike monolithic platforms that bundle all processing steps into
a single application, this modular architecture lets researchers freely
combine preprocessing, annotation, and statistical analysis tools into
custom workflows. Connecting SIRIUS to this ecosystem therefore opens up a
wide range of analytical pipelines that would otherwise require manual data
export and re-import between disconnected software.

`RuSirius` builds on `RSirius` to provide a higher-level, user-facing R
package that gives programmatic access to the full SIRIUS tool suite — formula
identification, ZODIAC re-ranking, fingerprint prediction, CSI:FingerID
structure search, CANOPUS classification, MSNovelist de novo generation, and
spectral library matching — through a stable REST API. By accepting `Spectra`
objects and integrating with `xcms` and `MetaboAnnotation`, `RuSirius` slots
directly into the existing RforMassSpectrometry ecosystem, rather than
creating a parallel workflow.


# Software Design

`RuSirius` is structured around a central S4 class, `Sirius`, which
encapsulates the connection to a running SIRIUS instance and tracks project
state. This design reflects a key architectural trade-off: rather than
reimplementing SIRIUS algorithms in R, `RuSirius` delegates computation to
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
5. **Database management** functions (`listDbs`, `createDb`, `removeDb`)
   allow users to create and query custom spectral and structure databases.

Throughout this workflow, `RuSirius` maintains a mapping between user-facing
feature identifiers and SIRIUS-internal feature IDs, allowing seamless
round-tripping between the R session and the SIRIUS project.

A Docker image based on the official Bioconductor image is provided, bundling
SIRIUS, `RSirius`, and `RuSirius` with an automatically started REST API
server, enabling reproducible deployment without manual installation.

# Research Impact Statement

<!-- TODO: Add evidence of research use, e.g., publications that used
     RuSirius, external adopters, workshop materials, or integration
     into other pipelines. -->

`RuSirius` has been developed as part of the RforMassSpectrometry initiative
and is hosted under the `rformassspectrometry` GitHub organization. The
package has been used internally for metabolomics annotation
workflows at Eurac Research and applied in a ring trial analysis
<!-- TODO: add ring trial repo reference -->. Its integration with
`xcms` and `Spectra` positions it as a seamless annotation step for
the thousands of researchers already using these packages for
LC-MS/MS data processing.

As documented by the issue tracker on the GitHub repository, `RuSirius` was
developed in close collaboration with end users as well as with the current
SIRIUS developers, who recognized the need for an interoperable bridge
between `RSirius` and the RforMassSpectrometry ecosystem.

# AI Usage Disclosure

All content was written, reviewed, edited, and validated by the
human authors. The core software design decisions, implementation,
and scientific framing are entirely the work of the authors. Claude
was used for reducing text length, for ensuring proper English
phrasing and correcting typos.

# Availability

`RuSirius` is available on GitHub at
<https://github.com/RforMassSpectrometry/RuSirius> under the Artistic-2.0
license. Documentation, including five vignettes covering all major
workflows, is available at
<https://rformassspectrometry.github.io/RuSirius/>. A Docker image with
SIRIUS and all dependencies pre-installed is available on Docker Hub at
`rformassspectrometry/rusirius`.

# Acknowledgements

We thank Marcus Ludwig, Markus Fleischauer, Jonas Emmert, and Nils
<!-- TODO: add Nils' surname --> for their support during the
development of `RuSirius` and their original work on the API and
`RSirius`. This work builds on the SIRIUS software developed at
Friedrich Schiller University Jena and Bright Giant GmbH, and on the
RforMassSpectrometry infrastructure maintained by Johannes Rainer and
Laurent Gatto.

<!-- TODO: add HUMAN doctoral network funding information -->

# References
