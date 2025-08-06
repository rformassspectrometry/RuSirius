#' @title Identifying molecular formula
#'
#' @name formulaIdParam
#'
#' @description
#' This function configures the parameters for molecular formula annotation
#' in Sirius.
#' Molecular formula identification is done using isotope pattern analysis on
#' the MS1 data as well as fragmentation tree computation on the MS2 data. The
#' score of a molecular formula candidate is a combination of the isotope
#' pattern score and the fragmentation tree score.
#'
#' @section General parameters:
#'
#' We advise to set up these following parameter to fit your specific study.
#'
#' @param instrument `character(1)` The type of mass spectrometer used for the
#'        analysis. Options include `"QTOF"`, `"ORBITRAP"`, and `"FTICR"`. This
#'        choice mainly affects the allowed mass deviation. If you are unsure
#'        about the instrument, use the default value `"QTOF"`.
#'
#' @param massAccuracyMS2ppm `numeric(1)` The maximum allowed mass deviation
#'        (in parts per million, ppm) for molecular formulas. Only formulas
#'         within this mass window are considered. Default is `10`.
#'
#' @param performBottomUpSearch `logical` If `TRUE`, enables molecular formula
#'        generation through a bottom-up search. Default is `TRUE`.
#'
#' @param performDeNovoBelowMz `numeric(1)` Specifies the m/z below which de
#'        novo molecular formula generation is enabled. Set to `0` to disable de
#'        novo molecular formula generation. Default is `400`.
#'
#' @param detectableElements `list(character)` Defines the elements that can be
#'        added to the chemical alphabet when detected in the spectrum, such as
#'        from isotope patterns. Default is `c("B", "S", "Cl", "Se", "Br")`.
#'
#' @param enforceElGordoFormula `logical` El Gordo may predict that an MS/MS
#'        spectrum is a lipid spectrum. If enabled, the corresponding molecular
#'        formula will be enforeced as molecular formula candidate. Default is
#'        `TRUE`.
#'
#' @section Advanced parameters:
#'
#' If you want to specify these parameters we advise you read the Sirius
#' documentation to learn how to adapt them to your dataset and annotation
#' needs.
#'
#' @param numberOfCandidates `integer(1)` The number of formula candidates to
#'        keep in the result list. Default is `10`.
#'
#' @param numberOfCandidatesPerIonization `integer(1)` Forces SIRIUS to report
#'        at least this number of candidates per ionization.
#'
#' @param isotopeMs2Settings `character(1)` Specifies how isotope patterns in
#'        MS/MS should be handled. Default is `"IGNORE"`.
#'        Options:
#'
#'        - `"FILTER"`: Excludes formulas if the theoretical isotope pattern
#'          doesn't match.
#'        - `"SCORE"`: Uses isotope patterns for scoring, useful for clear
#'          MS/MS isotope patterns.
#'        - `"IGNORE"`: Ignores isotope patterns in MS/MS.
#'
#' @param filterByIsotopePattern `logical` When `TRUE`, filters molecular
#'        formulas by comparing their theoretical isotope patterns to the
#'        measured ones, excluding those that don't match. Default is `TRUE`.
#'
#' @param formulaSearchDBs `list(character)` A list of structure databases
#'        (e.g., `"CHEBI"`, `"HMDB"`) from which molecular formulas are
#'        extracted to reduce the search space. Use only if necessary, as de
#'        novo formula annotation is usually more effective. Default is
#'        `character(0)`.
#'
#' @param applyFormulaConstraintsToDBAndBottomUpSearch `logical` If `TRUE`,
#'        applies formula (element) constraints to both database search and
#'        bottom-up search, in addition to de novo generation. Default is
#'        `FALSE`.
#'
#' @param enforcedFormulaConstraints `character` Specifies the elements
#'        that are always considered when auto-detecting the formula. Enforced
#'        elements are always included in the formula, even if the compound is
#'        already assigned to a specific molecular formula. Default is
#'        `H,C,N,O,P`.
#'
#' @param fallbackFormulaConstraints `character` Specifies the elements that
#'        are used as fallback when auto-detection fails (e.g., no isotope
#'        pattern). Default is `S`.
#'
#' @param ilpTimeout `logical` The timeout settings for the integer linear
#'        programming (ILP) solver. If `TRUE`, it should include timeout
#'        parameters such as `numberOfSecondsPerDecomposition` and
#'        `numberOfSecondsPerInstance`.
#'
#' @param numberOfSecondsPerDecomposition `numeric`
#'
#' @param numberOfSecondsPerInstance `numeric`
#'
#' @param useHeuristic `logical` If `TRUE`, enables the use of heuristics in
#'        molecular formula annotation. When enabled, additional thresholds
#'        like `useHeuristicAboveMz` and `useOnlyHeuristicAboveMz` can be set.
#'
#' @param useHeuristicAboveMz `numeric` The m/z threshold above which heuristic
#'        is used. Default is `300`.
#'
#' @param useOnlyHeuristicAboveMz `numeric(1)` The m/z threshold above which
#'        only heuristic is used. Default is `650`.
#'
#' @param injectSpecLibMatchFormulas `logical` If `TRUE`, formula candidates
#'        matching spectral library entries above a certain similarity
#'        threshold will be preserved for further analysis, regardless of score
#'        or filter settings. Default is `TRUE`.
#'
#' @param minScoreToInjectSpecLibMatch `numeric(1)` The similarity threshold
#'        for injecting spectral library match formulas. If the score is above
#'        this threshold, the formula will be preserved. Default is `0.7`.
#'
#' @param minPeaksToInjectSpecLibMatch `integer` The minimum number of matching
#'        peaks required to inject spectral library match formulas into further
#'        analysis.
#'
#'
#' @return An object of class `formulaIdParam` with the specified parameters.
#'
#' @importFrom methods setClass new
#' @importClassesFrom ProtGenerics Param
#'
#' @note
#' For more information, see the Sirius
#' [documentation](https://v6.docs.sirius-ms.io/methods-background).
#'
#' @references reference
#'
#' @examples
#' # Example of creating a formulaIdParam object
#' param <- formulaIdParam(instrument = "QTOF",
#'                         numberOfCandidates = 5,
#'                         enforceElGordoFormula = TRUE)
NULL

setClassUnion("listOrLogical", c("list", "logical"))

#' @noRd
setClass("formulaIdParam",
         slots = c(
             instrument = "character",
             numberOfCandidates = "numeric",
             numberOfCandidatesPerIonization = "numeric",
             massAccuracyMS2ppm = "numeric",
             isotopeMs2Settings = "character",
             filterByIsotopePattern = "logical",
             enforceElGordoFormula = "logical",
             performBottomUpSearch = "logical",
             performDeNovoBelowMz = "numeric",
             formulaSearchDBs = "character",
             applyFormulaConstraintsToDBAndBottomUpSearch = "logical",
             enforcedFormulaConstraints = "character",
             fallbackFormulaConstraints = "character",
             detectableElements = "character",
             ilpTimeout = "listOrLogical",
             numberOfSecondsPerDecomposition = "numeric",
             numberOfSecondsPerInstance = "numeric",
             useHeuristic = "listOrLogical",
             useHeuristicAboveMz = "numeric",
             useOnlyHeuristicAboveMz = "numeric",
             injectSpecLibMatchFormulas = "logical",
             minScoreToInjectSpecLibMatch = "numeric",
             minPeaksToInjectSpecLibMatch = "numeric"
         ),
         contains = "Param",
         prototype = prototype(
             instrument = "QTOF",
             numberOfCandidates = 10,
             numberOfCandidatesPerIonization = 0,
             massAccuracyMS2ppm = 10,
             isotopeMs2Settings = "IGNORE",
             filterByIsotopePattern = TRUE,
             enforceElGordoFormula = TRUE,
             performBottomUpSearch = TRUE,
             performDeNovoBelowMz = 400,
             formulaSearchDBs = c("CHEBI", "HMDB", "LIPIDMAPS",
                                  "PUBCHEM", "KEGG"),
             applyFormulaConstraintsToDBAndBottomUpSearch = FALSE,
             enforcedFormulaConstraints = c("H", "C", "N", "O", "P"),
             fallbackFormulaConstraints = c("B", "S", "Cl", "Se", "Br"),
             detectableElements = c("B", "S", "Cl", "Se", "Br"),
             ilpTimeout = FALSE,
             numberOfSecondsPerDecomposition = 0,
             numberOfSecondsPerInstance = 0,
             useHeuristic = TRUE,
             useHeuristicAboveMz = 300,
             useOnlyHeuristicAboveMz = 650,
             injectSpecLibMatchFormulas = TRUE,
             minScoreToInjectSpecLibMatch = 0.7,
             minPeaksToInjectSpecLibMatch = 6),
         validity = function(object) {
             if (!is.character(object@enforcedFormulaConstraints)) {
                 return("enforcedFormulaConstraints must be a single string.")
             }
             if (!is.character(object@fallbackFormulaConstraints)) {
                 return("fallbackFormulaConstraints must be a single string.")
             }
             TRUE
         }
)


#' @rdname formulaIdParam
#' @export
formulaIdParam <- function(
        instrument = c("QTOF", "ORBITRAP", "FTICR"),
        numberOfCandidates = 10,
        numberOfCandidatesPerIonization = 1,
        massAccuracyMS2ppm = 10.,
        isotopeMs2Settings = c("IGNORE", "FILTER", "SCORE"),
        filterByIsotopePattern = TRUE,
        enforceElGordoFormula = TRUE,
        performBottomUpSearch = TRUE,
        performDeNovoBelowMz = 400,
        formulaSearchDBs = character(0),
        applyFormulaConstraintsToDBAndBottomUpSearch = FALSE,
        enforcedFormulaConstraints = c("H", "C", "N", "O", "P"),
        fallbackFormulaConstraints = c("S"),
        detectableElements = c("B", "S", "Cl", "Se", "Br"),
        ilpTimeout = FALSE,
        numberOfSecondsPerDecomposition = 0,
        numberOfSecondsPerInstance = 0,
        useHeuristic = TRUE,
        useHeuristicAboveMz = 300,
        useOnlyHeuristicAboveMz = 650,
        injectSpecLibMatchFormulas = TRUE,
        minScoreToInjectSpecLibMatch = 0.7,
        minPeaksToInjectSpecLibMatch = 6
) {

    # Match argument values
    instrument <- match.arg(instrument)
    isotopeMs2Settings <- match.arg(isotopeMs2Settings)

    enforcedFormulaConstraints <- paste(enforcedFormulaConstraints,
                                        collapse = "")
    fallbackFormulaConstraints <- paste(fallbackFormulaConstraints,
                                        collapse = "")

    if (useHeuristic == TRUE) {
        useHeuristic <- list(
            useHeuristicAboveMz = useHeuristicAboveMz,
            useOnlyHeuristicAboveMz = useOnlyHeuristicAboveMz
        )
    } else {
        useHeuristic <- NA
    }

    if (ilpTimeout == TRUE) {
        ilpTimeout <- list(
            numberOfSecondsPerDecomposition = numberOfSecondsPerDecomposition,
            numberOfSecondsPerInstance = numberOfSecondsPerInstance
        )
    } else {
        ilpTimeout <- NA
    }

    # Create a new object
    param <- new("formulaIdParam",
                 instrument = instrument,
                 numberOfCandidates = numberOfCandidates,
                 numberOfCandidatesPerIonization = numberOfCandidatesPerIonization,
                 massAccuracyMS2ppm = massAccuracyMS2ppm,
                 isotopeMs2Settings = isotopeMs2Settings,
                 filterByIsotopePattern = filterByIsotopePattern,
                 enforceElGordoFormula = enforceElGordoFormula,
                 performBottomUpSearch = performBottomUpSearch,
                 performDeNovoBelowMz = performDeNovoBelowMz,
                 formulaSearchDBs = formulaSearchDBs,
                 applyFormulaConstraintsToDBAndBottomUpSearch = applyFormulaConstraintsToDBAndBottomUpSearch,
                 enforcedFormulaConstraints = enforcedFormulaConstraints,
                 fallbackFormulaConstraints = fallbackFormulaConstraints,
                 detectableElements = detectableElements,
                 ilpTimeout = ilpTimeout,
                 useHeuristic = useHeuristic,
                 injectSpecLibMatchFormulas = injectSpecLibMatchFormulas,
                 minScoreToInjectSpecLibMatch = minScoreToInjectSpecLibMatch,
                 minPeaksToInjectSpecLibMatch = minPeaksToInjectSpecLibMatch)
    param
}

#' @title Predicting FingerPrint and compounds Identifications
#'
#' @description
#' This function creates an object of class `predictParam` that can be used to
#' predict molecular fingerprints and compound identifications using
#' *CSI:FingerID* and *CANOPUS*.
#'
#' CSI:FIngerID identifies the structure of a molecule by predicting its
#' molecular fingerprint and using this fingerprint to search in a molecular
#' structure database.
#'
#' CANOPUS (Dührkop et al.) predicts the presense/absense of more than 2500
#' compound classes. CANOPUS predicts these classes based solely on MS/MS data
#' and without requiring database information. This means it can identify a
#' class even if no molecular structure of that class exists in the molecular
#' structure database.
#'
#' @name predictParam
#'
#'
#' @param useScoreThreshold `logical` whether to use a soft threshold to be
#'        applied to only compute FingerPrints for promising formula candidates.
#'        Enabling is highly recommended. Default is `TRUE`.
#' @param alwaysPredictHighRefMatches `logical` wether to predict
#'       FingerPrint/Classes&Structure for formulas candidates with reference
#'       spectrum similarity > `Sirius.minReferenceMatchScoreToInject` no
#'       matter which score threshold rules apply. Default is `FALSE`.
#'
#' @importFrom methods setClass new
#' @importClassesFrom ProtGenerics Param
#'
#' @note
#' For more information, see the Sirius
#' [documentation](https://v6.docs.sirius-ms.io/methods-background).
#'
#' @references reference
#'
#' @examples
#' # Example of setting up the parameters for the prediction of molecular
#' # fingerprints and compound class
#' param <- predictParam()
#'
NULL

#' @noRd
setClass("predictParam",
         slots =
             c(useScoreThreshold = "logical",
               alwaysPredictHighRefMatches = "logical"),
         contains = "Param",
         prototype = prototype(useScoreThreshold = TRUE,
                               alwaysPredictHighRefMatches = FALSE)
)

#' @rdname predictParam
#' @export
predictParam <- function(useScoreThreshold = TRUE,
                         alwaysPredictHighRefMatches = FALSE) {
    new("predictParam",
        useScoreThreshold = useScoreThreshold,
        alwaysPredictHighRefMatches = alwaysPredictHighRefMatches)
}

#' @title Spectra database matching
#'
#' @name spectraMatchingParam
#'
#' @description
#' This function is to set up the parameter for matching to spectra
#' databases. This needs to be run first. Spectral library matching is
#' performed using the cosine score with squared peak intensities, ignoring the
#' precursor peak.
#'
#' Note that spectral library matches are added as annotations to CSI:FingerID
#' results and do not influence the ranking of structure candidates.
#'
#' @param spectraSearchDBs `character` vector of the databases to search. The
#'        default is `c("BIO", "massbank")`. Other values can be found by
#'        running `listDatabases()`.
#' @param peakDeviationPpm `numeric` Maximum allowed mass deviation in ppm for
#'        matching peaks.
#' @param precursorDeviationPpm `numeric` Maximum allowed mass deviation in ppm
#'        for matching the precursor. If not specified, the same value as for
#'        the peaks is used.
#'
#' @param scoring `character` The scoring function to use. Possible values are
#'       `MODIFIED_COSINE`, `GAUSSIAN`, `INTENSITY`. The default is
#'       `MODIFIED_COSINE`.
#'
#' @importFrom methods setClass new
#' @importClassesFrom ProtGenerics Param
#'
#' @note
#' For more information, see the Sirius
#'  [documentation](https://v6.docs.sirius-ms.io/methods-background).
#'
#' @return An object of class `spectraMatchingParam`
#'
#' @examples
#' # Example of setting up the parameters for spectra matching
#' param <- spectraMatchingParam(spectraSearchDBs = c("BIO", "massbank"),
#'                               peakDeviationPpm = 10,
#'                               precursorDeviationPpm = 10,
#'                               scoring = "MODIFIED_COSINE")
#'
NULL

#' @noRd
spectraMatchingParam <- setClass("spectraMatchingParam",
                                 slots = c(
                                     spectraSearchDBs = "character",
                                     peakDeviationPpm = "numeric",
                                     precursorDeviationPpm = "numeric",
                                     scoring = "character"),
                                 contains = "Param",
                                 prototype = prototype(
                                     spectraSearchDBs = c("BIO", "massbank"),
                                     peakDeviationPpm = 10,
                                     precursorDeviationPpm = 10,
                                     scoring = c("MODIFIED_COSINE", "GAUSSIAN", "INTENSITY")
                                 ),
                                 validity = function(object) {
                                     if (object@peakDeviationPpm < 0) {
                                         return("peakDeviationPpm must be a positive number")
                                     }
                                     if (object@precursorDeviationPpm < 0) {
                                         return("precursorDeviationPpm must be a positive number")
                                     }
                                     if (!object@scoring %in% c("MODIFIED_COSINE", "INTENSITY", "GAUSSIAN")) {
                                         return("scoring must be one of 'MODIFIED_COSINE', 'INTENSITY', ",
                                                "'GAUSSIAN'")
                                     }
                                     return(TRUE)
                                 }
)

#' @rdname spectraMatchingParam
#' @export
spectraMatchingParam <- function(
        spectraSearchDBs = c("BIO", "massbank"),
        peakDeviationPpm = 10,
        precursorDeviationPpm = 10,
        scoring = c("MODIFIED_COSINE", "INTENSITY", "GAUSSIAN")) {
    scoring <- match.arg(scoring)
    new("spectraMatchingParam",
        spectraSearchDBs = spectraSearchDBs,
        peakDeviationPpm = peakDeviationPpm,
        precursorDeviationPpm = precursorDeviationPpm,
        scoring = scoring)
}

#' @title Structure Database Search
#'
#' @name structureDbSearchParam
#'
#' @description
#' This function creates an object of class `structureDbSearchParam` that can
#' be used to configure the structure database search in Sirius. The object can
#' be passed to the `runSirius` function to perform the structure database
#' search.
#'
#' By default, *SIRIUS* searches for molecular structures in a biomolecule
#' structure database. It can also search in the (extremely large) PubChem
#' database or in custom “suspect databases” provided by the user.
#'
#' @param structureSearchDbs `character` vector, specifying the id of the
#'        databases to search for structures. Default is `c("BIO", "massbank")`.
#'        If expansive search is enabled this database selection will be
#'        expanded to PubChem if not high confidence hit was found in the
#'        selected databases. The available databases can be found by running
#'        `listDatabases()`.
#'
#' @param tagStructuresWithLipidClass `logical`, specifying whether to tag
#'        structures with lipid class estimated by El Gordo. The lipid class
#'        will only be available if El Gordo predicts that the MS/MS  is a
#'        lipid spectrum. If this parameter is set to `FALSE` El Gordo will
#'        still be executed and e.g. improve the fragmentation tree, but the
#'        matching structure candidates will not be tagged if they match lipid
#'        class.
#'
#' @param expansiveSearchConfidenceMode `character(1)` , specifying the
#'        confidence mode for expansive search. Possible values are
#'        `"APPROXIMATE"`,`"EXACT"`, `"OFF"`. Defaults to `"APPROXIMATE"`.
#'
#' @importFrom methods setClass new
#' @importClassesFrom ProtGenerics Param
#'
#' @return An object of class `structureDbSearchParam`.
#'
#' @note
#' For more information, see the Sirius
#' [documentation](https://v6.docs.sirius-ms.io/methods-background).
#'
#' @references reference
#'
#' @examples
#' # Example of setting up the parameters for structure database search
#' param <- structureDbSearchParam(
#'                  structureSearchDbs = c("BIO", "massbank"),
#'                  tagStructuresWithLipidClass = TRUE,
#'                  expansiveSearchConfidenceMode = "APPROXIMATE")
#'
#'
NULL

#' @noRd
setClass("structureDbSearchParam", slots = c(
    structureSearchDbs = "character",
    tagStructuresWithLipidClass = "logical",
    expansiveSearchConfidenceMode = "character"),
    contains = "Param",
    prototype = prototype(
        structureSearchDbs = c("BIO", "massbank"),
        tagStructuresWithLipidClass = FALSE,
        expansiveSearchConfidenceMode = c("APPROXIMATE","EXACT")),
    validity = function(object) {

    })

#' @rdname structureDbSearchParam
#' @export
structureDbSearchParam <- function(
        structureSearchDbs = c("BIO", "massbank"),
        tagStructuresWithLipidClass = TRUE,
        expansiveSearchConfidenceMode = c("APPROXIMATE", "EXACT", "OFF")
) {
    expansiveSearchConfidenceMode <- match.arg(expansiveSearchConfidenceMode)
    new("structureDbSearchParam",
        structureSearchDbs = structureSearchDbs,
        tagStructuresWithLipidClass = tagStructuresWithLipidClass,
        expansiveSearchConfidenceMode = expansiveSearchConfidenceMode)
}

#' @title de novo structure annotation
#'
#' @name deNovoStructureParam
#'
#' @description
#' This function to set up the parameter for de novo structure annotation using
#' the *MSNovelist* tool.
#'
#' *MSNovelist* generates molecular structures de novo from MS/MS data -
#' without relying on any database. This makes it particularly useful for
#' analyzing poorly represented analyte classes and novel compounds, where
#' traditional database searches may fall short. However, it is not intended
#' to replace database searches altogether, as structural elucidation of small
#' molecules from MS/MS data remains a challenging task, and identifying a
#' structure without database candidates is even more difficult.
#'
#' @param numberOfCandidateToPredict `numeric`, number of structure candidates
#'        to be predicted by MsNovelist. Max Value `128`. Actual number of
#'        returned candidate might be lower du to duplicates being created.
#'        Default is `10`
#'
#'
#' @importFrom methods setClass new
#' @importClassesFrom ProtGenerics Param
#'
#' @return An object of class `deNovoStructureParam`.
#'
#' @note
#' For more information, see the Sirius
#' [documentation](https://v6.docs.sirius-ms.io/methods-background).
#'
#' @references reference
#'
#' @examples
#' # Example of setting up the parameters for de novo structure annotation
#' param <- deNovoStructureParam(numberOfCandidateToPredict = 10)
#'
NULL

#' @noRd
setClass("deNovoStructureParam",
         slots = c(numberOfCandidateToPredict = "numeric"),
         contains = "Param",
         prototype = prototype(c(numberOfCandidateToPredict = 10)),
         validity = function(object) {
             if (object@numberOfCandidateToPredict < 1 ||
                 object@numberOfCandidateToPredict > 128)
                 stop("numberOfCandidateToPredict must be between 1 and 128.")
             TRUE
         }
)

#' @rdname deNovoStructureParam
#' @export
deNovoStructureParam <- function(numberOfCandidateToPredict = 10) {
    new("deNovoStructureParam",
        numberOfCandidateToPredict = numberOfCandidateToPredict)
}
#' @title Configuration fr re-ranking of Molecular Formula Annotation
#'
#' @name zodiacParam
#'
#' @description
#' This function configures the parameters for the re-ranking of the previously
#' computed molecular formula annotation in Sirius. This step is quite
#' computationally and memory demanding, it is advised to perform it only if
#' de novo structure annotation is used later on
#'
#' ZODIAC uses the top X molecular formula candidates for each molecule from
#' SIRIUS to build a similarity network, and uses Bayesian statistics to
#' re-rank those candidates.
#'
#' @param consideredCandidatesAt300Mz An `integer(1)` specifying the maximum
#'        number of candidate molecular formulas considered by ZODIAC for
#'        compounds below 300 m/z. Default is `10`.
#' @param consideredCandidatesAt800Mz An `integer(1)` specifying the maximum
#'        number of candidate molecular formulas considered by ZODIAC for \
#'        compounds above 800 m/z. Default is `50`.
#' @param runInTwoSteps `logical` specifying whether ZODIAC uses a
#'        2-step approach. First running 'good quality compounds' only, and
#'        afterwards including the remaining compounds. Default is `TRUE`.
#' @param edgeFilterThreshold `logical` value specifying whether ZODIAC uses
#'        an edge filter threshold. Default is `TRUE`.
#' @param thresholdFilter `numeric(1)` value specifying the threshold filter.
#'       Default is `0.95`.
#' @param minLocalCandidates `numeric(1)` value specifying the minimum number of
#'        local candidates. Default is `1`.
#' @param minLocalConnections `numeric(1)` value specifying the minimum number of
#'        local connections. Default is `10`.
#' @param gibbsSamplerParameters `logical` value specifying whether ZODIAC
#'        uses Gibbs sampler parameters. Default is `TRUE`.
#' @param iterations `numeric(1)` specifying the number of iterations. Default is
#'       `20000`.
#' @param burnInPeriod `numeric(1)` specifying the burn-in period. Default is
#'       `2000`.
#' @param numberOfMarkovChains `numeric(1)` specifying the number of Markov
#'        chains. Default is `10`.
#'
#' @importFrom methods setClass setClassUnion
#' @importClassesFrom ProtGenerics Param
#'
#' @note
#' For more information, see the Sirius
#' [documentation](https://v6.docs.sirius-ms.io/methods-background).
#'
#' @references reference
#'
#' @return An object of class `zodiacParam`.
#'
#' @examples
#' # Example of setting up the parameter for Zodiac re-ranking
#' param <- zodiacParam(consideredCandidatesAt300Mz = 10,
#'                      consideredCandidatesAt800Mz = 50,
#'                      runInTwoSteps = TRUE,
#'                      edgeFilterThreshold = TRUE,
#'                      thresholdFilter = 0.95,
#'                      minLocalCandidates = 1,
#'                      minLocalConnections = 10,
#'                      gibbsSamplerParameters = TRUE,
#'                      iterations = 20000,
#'                      burnInPeriod = 2000,
#'                      numberOfMarkovChains = 10)
#'
NULL

setClassUnion("listOrLogical", c("list", "logical"))

#' @noRd
setClass("zodiacParam",
         slots = c(consideredCandidatesAt300Mz = "numeric",
                   consideredCandidatesAt800Mz = "numeric",
                   runInTwoSteps = "logical",
                   edgeFilterThreshold = "listOrLogical",
                   thresholdFilter = "numeric",
                   minLocalCandidates = "numeric",
                   minLocalConnections = "numeric",
                   gibbsSamplerParameters = "listOrLogical",
                   iterations = "numeric",
                   burnInPeriod = "numeric",
                   numberOfMarkovChains = "numeric"
         ),
         contains = "Param",
         prototype = prototype(
             consideredCandidatesAt300Mz = 10,
             consideredCandidatesAt800Mz = 50,
             runInTwoSteps = TRUE,
             edgeFilterThreshold = TRUE,
             thresholdFilter = 0.1,
             minLocalCandidates = 1,
             minLocalConnections = 1,
             gibbsSamplerParameters = TRUE,
             iterations = 1000,
             burnInPeriod = 100,
             numberOfMarkovChains = 1),
         validity = function(object) {
             TRUE
         }
)

#' @rdname zodiacParam
#' @export
zodiacParam <- function(consideredCandidatesAt300Mz = 10,
                        consideredCandidatesAt800Mz = 50,
                        runInTwoSteps = TRUE,
                        edgeFilterThreshold = TRUE,
                        thresholdFilter = 0.95,
                        minLocalCandidates = 1,
                        minLocalConnections = 10,
                        gibbsSamplerParameters = TRUE,
                        iterations = 20000,
                        burnInPeriod = 2000,
                        numberOfMarkovChains = 10){
    if (edgeFilterThreshold == TRUE)
        edgeFilterThreshold <- list(
            thresholdFilter = thresholdFilter,
            minLocalCandidates = minLocalCandidates,
            minLocalConnections = minLocalConnections
        )
    else edgeFilterThreshold <- NA
    if (gibbsSamplerParameters == TRUE)
        gibbsSamplerParameters <- list(
            iterations = iterations,
            burnInPeriod = burnInPeriod,
            numberOfMarkovChains = numberOfMarkovChains
        )
    else gibbsSamplerParameters <- NA
    new("zodiacParam",
        consideredCandidatesAt300Mz = consideredCandidatesAt300Mz,
        consideredCandidatesAt800Mz = consideredCandidatesAt800Mz,
        runInTwoSteps = runInTwoSteps,
        edgeFilterThreshold = edgeFilterThreshold,
        gibbsSamplerParameters = gibbsSamplerParameters)
}


