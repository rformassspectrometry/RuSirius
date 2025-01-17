#'@include predictParam.R structureDbSearch.R deNovoStructure.R

#' @title Run job on Sirius.
#'
#' @description This function configures the job submission to Sirius.
#' It creates an object of class `config` that can be used to submit a job
#' Sirius, it can also be saved and reused later on through the
#' `saveJobConfig()`function. For example on how to use, see the vignette.
#'
#' Depending on what task you want to perform, you can specify the following
#' parameters:
#'
#' - [`spectraMatchingParam`]: Allows to perform matching betweeen spectra input
#'   to spectral libraries.
#' - [`formulaIdParam`]: Allows to generate molecular formula candidates for
#'   each features.
#' - [`zodiacParam`]: Allows to perform re-ranking of formula candidates
#'   using *Zodiac*. It is advised to only perform it if De Novo structure
#'   annotation is run later.
#' - [`predictParam`]: Allows to perform molecular fingeprint prediction using
#'   *CSI:FingerID* and compound classification using *CANOPUS*.
#' - [`structureDbSearchParam`]: Allows to perform structure annotation based
#'   on the fingerprint identifications.
#' - [`deNovoStructureParam`]: Allows to perform de novo structure generation using
#'   the *MSNovelist* tool.
#'
#' @section Only formula identification:
#'
#' If you only want to perform formula identification, you can by only inputing
#' the `formulaIdParam` object. In combination, you can also input the
#' `spectraMatchingParam` object to perform spectral matching and subsequently
#' compare the results.
#'
#' @section Structure annotation:
#'
#' To performe structure annotation, you need input the `formulaIdParam` object,
#' as well as the `predictParam` object to perform molecular fingerprint prediction
#' and compound classification. These results will then subsequently be used to
#' perform structure annotation using the `structureDbSearchParam` object.
#'
#' @section De Novo structure annotation:
#'
#' To perform de novo structure annotation, you need to input the `formulaIdParam`
#' object, it is also advised in this case to perform re-ranking using the
#' `zodiacParam` object. The molecular fingerprint prediction and compound
#' classification can be performed using the `predictParam` object.
#' The `deNovoStructureParam` object is then used to perform the de novo structure
#' annotation.
#'
#' @param compoundsIds `character` vector, the ids of the compounds to process.
#' @param alignedFeaturesIds `character` vector, the ids of the aligned features
#'        to process. By default, computes all.
#' @param fallbackAdducts `character` vector, fallback adducts are considered
#'        if the auto detection did not find any indication for an ion mode.
#' @param enforceAdducts `character` vector, the adducts to enforce. They are
#'        always considered.
#' @param detectableAdducts `character` vector, detectable adducts are only
#'        considered if there is an indication in the MS1 scan (e.g. correct
#'        mass delta).
#' @param spectraSearchParams object of class `spectraMatchingParam`, containing
#'       the parameters for the spectra matching.
#' @param formulaIdParams object of class `formulaIdParam`, containing the
#'        parameters for the molecular formula identification.
#' @param zodiacParams object of class `zodiacParam`, containing the parameters
#'        for the Zodiac re-ranking.
#' @param predictParams object of class `predictParam`, containing the parameters for
#'        the molecular fingerprint prediction and compound classification.
#' @param structureDbSearchParams object of class `structureDbParam`, containing
#'        the parameters for the structure annotation.
#' @param msNovelistParams object of class `deNovoStructureParam`, containing the
#'        parameters for the de novo structure generation.
#' @param recompute `logical`, whether to recompute the job , default is FALSE.
#'
#' @name run
#'
#' @returns The job ID of the submitted job, it can be inputted in the
#'  `jobInfo()` function to retrieve the job information. To retrieve results
#'  see [`results`] documentation.
#'
#' @importClassesFrom ProtGenerics Param
#' @importFrom methods setClass setClassUnion is
#' @importFrom ProtGenerics as.list
#'
#'
NULL

#' @rdname run
#' @importFrom Rsirius JobSubmission
#' @importFrom jsonlite toJSON
#'
#' @param sirius `Sirius` object, the connection to the Sirius server.
#' @param configFile `character`, the path to the configuration file to use for
#'        the job submission, optional.
#' @param recompute `logical`, whether to recompute the job , only necessary if
#'        the configfile comes from a saved file. Default is FALSE.
#' @param wait `logical`, whether to wait for the job to finish. Default is TRUE
#'
#' @export
run <- function(sirius,
                compoundsIds = character(),
                alignedFeaturesIds = featuresId(sirius),
                fallbackAdducts = c("[M + H]+", "[M - H]-",
                                    "[M + Na]+", "[M + K]+"),
                enforceAdducts = character(),
                detectableAdducts = c(
                    "[M + H3N + H]+", "[M - H4O2 + H]+",
                    "[M - H2O - H]-",
                    "[M - H3N - H]-", "[M + Cl]-", "[2M + K]+",
                    "[M + K]+",
                    "[2M + Cl]-", "[M + C2H4O2 - H]-", "[M + H]+",
                    "[2M + H]+",
                    "[M - CH3 - H]-", "[M - H]-", "[M + Na]+",
                    "[M - H2O + H]+" ),
                spectraSearchParams = NA,
                formulaIdParams = NA,
                zodiacParams = NA,
                predictParams = NA,
                structureDbSearchParams = NA,
                msNovelistParams = NA,
                recompute = FALSE,
                configFile = character(),
                wait = TRUE) {
    if (length(configFile)) {
        configfile <- sirius@api$jobs_api$GetJobConfig(configFile)
        job <- sirius@api$jobs_api$StartJobFromConfig(sirius@projectId,
                                                        configfile,
                                                        recompute)
    } else {
        config <- config(
            compoundsIds = compoundsIds,
            alignedFeaturesIds = alignedFeaturesIds,
            fallbackAdducts = fallbackAdducts,
            enforceAdducts = enforceAdducts,
            detectableAdducts = detectableAdducts,
            spectraSearchParams = spectraSearchParams,
            formulaIdParams = formulaIdParams,
            zodiacParams = zodiacParams,
            predictParams = predictParams,
            structureDbSearchParams = structureDbSearchParams,
            msNovelistParams = msNovelistParams,
            recompute = recompute
        )
        l_config <- as.list(config)
        configjson <- toJSON(l_config, auto_unbox = TRUE)
        js <- JobSubmission$new()
        js$fromJSON(configjson)
        job <- sirius@api$jobs_api$StartJob(sirius@projectId, js)
    }
    # Monitor job status
    if (wait) {
        while (sirius@api$jobs_api$GetJob(sirius@projectId, job$id)$progress$state != "DONE")
            Sys.sleep(1)
    }
    return(job$id)
}

setClassUnion("ListOrLogical", c("list", "logical"))

#' @noRd
setClass(
    "config",
    slots = c(
        compoundsIds = "character",
        alignedFeaturesIds = "character",
        fallbackAdducts = "character",
        enforceAdducts = "character",
        detectableAdducts = "character",
        formulaIdParams = "ListOrLogical",
        zodiacParams = "ListOrLogical",
        predictParams = "ListOrLogical",
        structureDbSearchParams = "ListOrLogical",
        msNovelistParams = "ListOrLogical",
        canopusParams = "ListOrLogical",
        fingerprintPredictionParams = "ListOrLogical",
        spectraSearchParams = "ListOrLogical",
        recompute = "logical"
    ),
    contains = "Param",
    prototype = prototype(
        compoundsIds = character(),
        alignedFeaturesIds = character(),
        fallbackAdducts = character(),
        enforceAdducts = character(),
        detectableAdducts = character(),
        formulaIdParams = list(),
        zodiacParams = NA,
        predictParams = NA,
        structureDbSearchParams = NA,
        msNovelistParams = NA,
        canopusParams = list(enabled = FALSE),
        fingerprintPredictionParams = list(enabled = FALSE),
        spectraSearchParams = NA,
        recompute = FALSE
    ),
    validity = function(object) {
        if (length(object@compoundsIds) == 0 && length(object@alignedFeaturesIds) == 0) {
            return("Either 'compoundsIds' or 'alignedFeaturesIds' must be provided.")
        }
        TRUE
    }
)

#' @rdname run
#' @export
config <- function(compoundsIds = character(),
                     alignedFeaturesIds = character(),
                     fallbackAdducts = c("[M + H]+", "[M - H]-",
                                         "[M + Na]+", "[M + K]+"),
                     enforceAdducts = character(),
                     detectableAdducts = c(
                         "[M + H3N + H]+", "[M - H4O2 + H]+",
                         "[M - H2O - H]-",
                         "[M - H3N - H]-", "[M + Cl]-", "[2M + K]+",
                         "[M + K]+",
                         "[2M + Cl]-", "[M + C2H4O2 - H]-", "[M + H]+",
                         "[2M + H]+",
                         "[M - CH3 - H]-", "[M - H]-", "[M + Na]+",
                         "[M - H2O + H]+"
                     ),
                     formulaIdParams = formulaIdParam(),
                     zodiacParams = NA,
                     predictParams = NA,
                     structureDbSearchParams = NA,
                     msNovelistParams = NA,
                     spectraSearchParams = NA,
                     recompute = FALSE) {
    if (is(spectraSearchParams, "spectraMatchingParam")) {
        spectraSearchParams <- as.list(spectraSearchParams)
        spectraSearchParams$enabled <- TRUE
    }
    if (is(formulaIdParams, "formulaIdParam")) {
        formulaIdParams <- as.list(formulaIdParams)
        formulaIdParams$enabled <- TRUE
    } # here need to input an error in case no input
    if (is(zodiacParams, "zodiacParam")) {
        zodiacParams <- as.list(zodiacParams)
        zodiacParams$enabled <- TRUE
    }
    if (is(predictParams, "predictParam")) {
        fingerprintPredictionParams <- as.list(predictParams)
        fingerprintPredictionParams$enabled <- TRUE
        canopusParams <- list(enabled = TRUE)
    } else {
        canopusParams <- NA
        fingerprintPredictionParams <- NA
    }

    if (is(structureDbSearchParams, "structureDbSearchParam")) {
        structureDbSearchParams <- as.list(structureDbSearchParams)
        structureDbSearchParams$enabled <- TRUE
    }
    if (is(msNovelistParams, "deNovoStructureParam")) {
        msNovelistParams <- as.list(msNovelistParams)
        msNovelistParams$enabled <- TRUE
    }
    new(
        "config",
        compoundsIds = compoundsIds,
        alignedFeaturesIds = alignedFeaturesIds,
        fallbackAdducts = fallbackAdducts,
        enforceAdducts = enforceAdducts,
        detectableAdducts = detectableAdducts,
        formulaIdParams = formulaIdParams,
        zodiacParams = zodiacParams,
        structureDbSearchParams = structureDbSearchParams,
        msNovelistParams = msNovelistParams,
        canopusParams = canopusParams,
        fingerprintPredictionParams = fingerprintPredictionParams,
        spectraSearchParams = spectraSearchParams,
        recompute = recompute
    )
}
