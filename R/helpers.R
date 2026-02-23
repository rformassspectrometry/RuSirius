#' Here are the helper functions used in the package.
#' Please add a description of the function and the methods in which it is
#' used.

### data import

#' @description
#' Group MSn-only spectra by acquisition order, inspired by
#' `fragmentGroupIndex()`. Within each `dataOrigin`, a new group starts
#' whenever a new MS2 `precursorMz` is encountered. Subsequent
#' higher-level scans (MS3+) belong to the same group as the preceding
#' MS2 scan. Used in `import()` when no MS1 data is present.
#'
#' @param spectra A `Spectra` object containing only MSn (level >= 2) data.
#'
#' @return An integer vector of group indices, same length as `spectra`.
#'
#' @noRd
.groupMSnIndex <- function(spectra) {
    origins <- spectra$dataOrigin
    ms_levels <- spectra$msLevel
    precursor_mz <- spectra$precursorMz
    n <- length(spectra)
    idx <- integer(n)
    group_counter <- 0L
    for (origin in unique(origins)) {
        sel <- which(origins == origin)
        current_ms2_mz <- NA_real_
        for (j in sel) {
            if (ms_levels[j] == 2L) {
                if (!isTRUE(all.equal(precursor_mz[j], current_ms2_mz))) {
                    group_counter <- group_counter + 1L
                    current_ms2_mz <- precursor_mz[j]
                }
            }
            idx[j] <- group_counter
        }
    }
    idx
}

#' @description
#' Used in import
#'
#' @noRd
.createpeaks <- function(matrices){
    if (nrow(matrices) == 1)
        return(list(RSirius::SimplePeak$new(mz = matrices[1], intensity = matrices[2])))
    res <- apply(matrices, 1,  function(x) {
        RSirius::SimplePeak$new(mz = x[1],
                                intensity = x[2])
    })
    return(res)
}

.createspectraMS1 <- function(data = character()) {
    if (!length(data))  return(NULL)
    RSirius::BasicSpectrum$new(
        msLevel = 1L,
        scanNumber = data$scanIndex,
        peaks = .createpeaks(matrices = peaksData(data)[[1]])
    )
}

.createspectraMSn <- function(data = character()) {
    if (!length(data)) return(NULL)
    if (length(data) == 1) {
        return(list(RSirius::BasicSpectrum$new(
            msLevel = data$msLevel,
            scanNumber = data$scanIndex,
            precursorMz = data$precursorMz,
            peaks = .createpeaks(matrices = peaksData(data)[[1]]))))
    }
    pdata <- lapply(peaksData(data), .createpeaks)
    basic_spectra <- vector("list", length(pdata))
    for (i in seq_along(pdata)) {
        basic_spectra[[i]] <- RSirius::BasicSpectrum$new(
            msLevel = data$msLevel[i],
            scanNumber = data$scanIndex[i],
            precursorMz = data$precursorMz[i],
            # collisionEnergy = if (!is.na(meta$collisionEnergy) && meta$collisionEnergy != 0)
            #   as.character(meta$collisionEnergy) else NULL, ## needed ?
            peaks = pdata[[i]]
        )
    }
    return(basic_spectra)
}

.createfeatures <- function(data, idx, adduct) {
    pol <- data$polarity[1]
    ms1 <- data[data$msLevel == 1L]
    msn <- data[data$msLevel >= 2L]
    ## Use precursorMz from the first MSn spectrum as ionMass whenever
    ## MSn data is available.  For MS1-only features, let Sirius
    ## derive the ionMass from the MS1 spectrum (ionMass = 0).
    if (length(msn) > 0) {
        ion_mass <- msn$precursorMz[1]
    } else {
        ion_mass <- 0
    }
    RSirius::FeatureImport$new(
        externalFeatureId = as.character(idx),
        ionMass = ion_mass,
        charge = if (pol == 0) -1 else pol,
        detectedAdducts = list(adduct),
        mergedMs1 = .createspectraMS1(ms1),
        ms2Spectra = .createspectraMSn(msn)
    )
}


.importSpectraChunk <- function(chunk, sirius, ms_column_name, data, adducts){
    fdata <- vector("list", length(chunk))

    for (i in chunk) {
        adduct <- adducts[i]
        bd_subset <- data[data[[ms_column_name]] == i]
        fdata[[which(chunk == i)]] <- .createfeatures(data = bd_subset, idx = i, adduct = adduct)
    }
    sirius@api$features_api$AddAlignedFeatures(
        project_id = sirius@projectId,
        fdata
    )
}


# Function to format and clean settings
.format_command <- function(command) {
    l <- unlist(strsplit(command, " "))
    l <- l[l != "config"]
    return(paste(l, collapse = "\n"))
}

# Function to clean and format job output
.clean_output <- function(output) {
    paste(
        sprintf("Job ID: %s", output$id),
        "",
        sprintf("Command: \n%s", .format_command(output$command)),
        "",
        "Progress:",
        sprintf("   State: %s", output$progress$state),
        sprintf("   Current Progress: %d", output$progress$currentProgress),
        sprintf("   Max Progress: %d", output$progress$maxProgress),
        "",
        "Affected Compound IDs:",
        sprintf("   %s", paste(output$affectedCompoundIds, collapse = ", ")),
        "",
        "Affected Aligned Feature IDs:",
        paste(
            vapply(output$affectedAlignedFeatureIds, function(x) {
                paste(x, collapse = ", ")
            }, character(1)),
            collapse = "\n"
        ),
        "",
        sep = "\n"
    )
}

## Developpers help
#' @noRd
.openWebApi <- function(sirius) {
    browseURL(sirius@api$projects_api$api_client$base_path)
}
