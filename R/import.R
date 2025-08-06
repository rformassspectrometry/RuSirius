#' @title Import Data into Sirius
#'
#' @name import
#' @export
#'
#' @param ms_column_name character(1). This is the name of a column expected
#' in the `spectraData` of the input spectra object. MS1 and MS2 spectra with
#' the same index in this column will be grouped into a single feature. This
#' parameter must be provided if the object has multiple `msLevel`s.
#' If no column name is provided and the object contains only one MS level,
#' then each MS1 spectrum will be exported as a separate feature in Sirius.
#' Lastly, we support multiple MS2 spectra per feature, but only one MS1
#' spectrum per index. Please merge MS1 spectra beforehand if necessary.
#'
#' @param chunkSize numeric(). Number of features to process and import at
#' once. Importing can be slow, so this is useful when working with a very
#' large number of spectra.
#'
#' @param sirius `Sirius`. The connection to the Sirius instance with a
#'        loaded project.
#'
#' @param adducts `character` vector of the adduct(s) associated with the
#'        features being imported. Must be either length 1 or the same length
#'        as the number of features. If of length 1 and fewer than the number
#'        of features, the same adduct will be used for all features.
#'
#' @param deleteExistingFeatures `logical(1)`. If `TRUE`, all existing features
#'        will be deleted before importing the new ones.

#'
#'
#' @importFrom methods setClass new
#' @importClassesFrom ProtGenerics Param
#'
#' @importFrom Rsirius FeatureImport BasicSpectrum SimplePeak
#' @importFrom Spectra spectraData peaksData
#'
NULL



#' @rdname import
#' @export
import <- function(sirius, spectra, ms_column_name = character(),
                   adducts = character(),
                   chunkSize = 500, deleteExistingFeatures = TRUE) {

    if (length(unique(spectra$msLevel)) > 1) { ## need to improve these checks
        if (!length(ms_column_name)) {
            stop("If spectra have more than one MS level, a column to group ",
                 "the spectra must be provided in 'ms_column_name'.")
        }

        if (anyNA(spectra[[ms_column_name]])) {
            stop("The column used to group the spectra cannot contain NA values.")
        }
    } else {
        if (!length(ms_column_name)) {
            spectra[[ms_column_name]] <- lapply(split(spectra, spectra$dataOrigin),
                                                function(x) seq_len(length(x))) |>
                unlist(use.names = FALSE)
            ms_column_name <- "ms_column_name"
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
