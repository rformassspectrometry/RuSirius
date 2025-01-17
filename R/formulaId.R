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
#' documentation to learn how to adapt them to your dataset and annotation needs.
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
#'        - `"SCORE"`: Uses isotope patterns for scoring, useful for clear MS/MS
#'          isotope patterns.
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
#'        molecular formula annotation. When enabled, additional thresholds like
#'        `useHeuristicAboveMz` and `useOnlyHeuristicAboveMz` can be set.
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
#' For more information, see the Sirius [documentation](https://v6.docs.sirius-ms.io/methods-background).
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
             formulaSearchDBs = c("CHEBI", "HMDB", "LIPIDMAPS", "PUBCHEM", "KEGG"),
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

    enforcedFormulaConstraints <- paste(enforcedFormulaConstraints, collapse = "")
    fallbackFormulaConstraints <- paste(fallbackFormulaConstraints, collapse = "")

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
