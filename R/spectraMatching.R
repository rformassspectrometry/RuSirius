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
#' @param peakDeviationPpm `numeric` 	Maximum allowed mass deviation in ppm for
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
#' For more information, see the Sirius [documentation](https://v6.docs.sirius-ms.io/methods-background).
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
      return("scoring must be one of 'MODIFIED_COSINE', 'INTENSITY', 'GAUSSIAN'")
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

