#' @title Import Data into Sirius
#'
#' @name import
#' @export
#'
#' @param sirius `Sirius`. The connection to the Sirius instance with a
#'        loaded project.
#'
#' @param spectra `Spectra` object containing MS data to import. Can contain
#'        MS1 and/or MS2 spectra. If multiple MS levels are present, the
#'        `ms_column_name` parameter must be provided to group spectra into
#'        features. See the `fragmentGroupIndex()` function from the Spectra
#'        package for generating appropriate grouping indices.
#'
#' @param ms_column_name `character(1)`. This is the name of a column expected
#'        in the `spectraData` of the input spectra object. MS1 and MS2 spectra
#'        with the same index in this column will be grouped into a single
#'        feature. This parameter must be provided if the object has multiple
#'        `msLevel`s. If no column name is provided and the object contains
#'        only one MS level, then each MS1 spectrum will be exported as a
#'        separate feature in Sirius. Lastly, we support multiple MS2 spectra
#'        per feature, but only one MS1 spectrum per index. Please merge MS1
#'        spectra beforehand if necessary.
#'
#' @param adducts `character` vector of the adduct(s) associated with the
#'        features being imported. Must be either length 1 or the same length
#'        as the number of features. If of length 1 and fewer than the number
#'        of features, the same adduct will be used for all features.
#'
#' @param chunkSize `numeric(1)`. Number of features to process and import at
#'        once. Importing can be slow, so this is useful when working with a
#'        very large number of spectra.
#'
#' @param deleteExistingFeatures `logical(1)`. If `TRUE`, all existing features
#'        will be deleted before importing the new ones.

#'
#'
#' @importFrom methods setClass new
#' @importClassesFrom ProtGenerics Param
#'
#' @importFrom RSirius FeatureImport BasicSpectrum SimplePeak
#' @importFrom Spectra spectraData peaksData
#'
NULL



#' @rdname import
#' @export
import <- function(sirius, spectra, ms_column_name = character(),
                   adducts = character(),
                   chunkSize = 500, deleteExistingFeatures = TRUE) {

    has_ms1 <- 1L %in% unique(spectra$msLevel)
    if (length(unique(spectra$msLevel)) > 1) { ## need to improve these checks
        if (!length(ms_column_name)) {
            if (!has_ms1) {
                ## Group MSn-only spectra by acquisition order, inspired
                ## by fragmentGroupIndex(). Within each dataOrigin, a new
                ## group starts whenever a new MS2 precursorMz is
                ## encountered. Subsequent higher-level scans (MS3+) are
                ## assigned to the same group as their preceding MS2.
                message("No MS1 data found. Auto-grouping MSn spectra by ",
                        "acquisition order and precursorMz.")
                spectra$ms_column_name <- .groupMSnIndex(spectra)
                ms_column_name <- "ms_column_name"
            } else {
                stop("If spectra have more than one MS level, a column to ",
                     "group the spectra must be provided in ",
                     "'ms_column_name'.")
            }
        }

        if (anyNA(spectra[[ms_column_name]])) {
            stop("The column used to group the spectra cannot contain ",
                 "NA values.")
        }
    } else {
        if (!length(ms_column_name)) {
            ms_column_name <- "ms_column_name"
            if (!has_ms1) {
                ## Single-level MSn (e.g. only MS2): group by
                ## precursorMz within each dataOrigin so that each
                ## unique precursor becomes a separate feature.
                spectra$ms_column_name <- .groupMSnIndex(spectra)
            } else {
                ## MS1-only: each spectrum is its own feature.
                spectra[[ms_column_name]] <- lapply(
                    split(spectra, spectra$dataOrigin),
                    function(x) seq_len(length(x))) |>
                    unlist(use.names = FALSE)
            }
        }
    }
    ## import data
    idxs <- spectra[[ms_column_name]]
    l <- length(unique(idxs))
    if (!length(adducts)) adducts <- rep("[M+?]+", l)
    la <- length(adducts)
    if (la != l) {
        if (la == 1) adducts <- rep(adducts, l)
        else
            stop("The number of adducts must be either 1 or the same as the ",
                 "number of spectra being imported.")
    }
    chunks <- split(unique(idxs), ceiling(seq_along(unique(idxs))/chunkSize))
    if (deleteExistingFeatures) sirius <- deleteFeatures(sirius)
    lapply(chunks, .importSpectraChunk, sirius = sirius, data = spectra,
                 ms_column_name = ms_column_name,
                 adducts = adducts)
    sirius@featureMap <- mapFeatures(sirius = sirius)
    sirius
}
