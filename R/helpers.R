#' Here are the helper functions used in the package.
#' Please add a description of the function and the methods in which it is
#' used.

### data import
#' @description
#' Used in import
#'
#' @noRd
.createpeaks <- function(matrices){
    if (nrow(matrices) == 1)
        return(list(Rsirius::SimplePeak$new(mz = matrices[1], intensity = matrices[2])))
    res <- apply(matrices, 1,  function(x) {
        Rsirius::SimplePeak$new(mz = x[1],
                                intensity = x[2])
    })
    return(res)
}

.createspectraMS1 <- function(data = character()) {
    if (!length(data))  return(NULL)
    Rsirius::BasicSpectrum$new(
        msLevel = 1L,
        scanNumber = data$scanIndex,
        peaks = .createpeaks(matrices = peaksData(data)[[1]])
    )
}

.createspectraMSn <- function(data = character()) {
    if (!length(data)) return(NULL)
    if (length(data) == 1) {
        return(list(Rsirius::BasicSpectrum$new(
            msLevel = data$msLevel,
            scanNumber = data$scanIndex,
            precursorMz = data$precursorMz,
            peaks = .createpeaks(matrices = peaksData(data)[[1]]))))
    }
    pdata <- lapply(peaksData(data), .createpeaks)
    basic_spectra <- vector("list", length(pdata))
    for (i in seq_along(pdata)) {
        basic_spectra[[i]] <- Rsirius::BasicSpectrum$new(
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
    Rsirius::FeatureImport$new(
        externalFeatureId = as.character(idx), ## don't need it ?
        ionMass = 0,
        charge = if (pol == 0) -1 else pol,
        detectedAdducts = list(adduct),
        mergedMs1 = .createspectraMS1(data[data$msLevel == 1L]),
        ms2Spectra = .createspectraMSn(data[data$msLevel == 2L])
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
