#' Here are the helper functions used in the package.
#' Please add a description of the function and the methods in which it is
#' used.

### adduct normalization

#' @description
#' Normalize adduct strings to the canonical SIRIUS format with spaces
#' around `+` and `-` inside brackets, e.g. `"[M+H]+"` becomes
#' `"[M + H]+"`.  Strings already in the spaced format are returned
#' unchanged.
#'
#' @param x `character` vector of adduct strings.
#'
#' @return `character` vector with normalized adduct strings.
#'
#' @noRd
.normalize_adducts <- function(x) {
    ## Only modify the part *inside* the brackets.
    ## 1. Extract the bracket content and the trailing charge sign.
    ## 2. Add spaces around every `+` or `-` inside the brackets.
    ## 3. Collapse multiple spaces and trim.
    vapply(x, function(a) {
        m <- regmatches(a, regexec("^(\\[)(.+)(\\])(.*)$", a))[[1L]]
        if (length(m) < 5L) return(a)          # not a recognized adduct
        inner <- m[3L]
        suffix <- m[5L]
        ## Add spaces around + and - (but not the leading sign of a number)
        inner <- gsub("(?<=[A-Za-z0-9?\\]])([+-])(?=[A-Za-z0-9?\\[])",
                       " \\1 ", inner, perl = TRUE)
        ## Collapse any multiple spaces
        inner <- gsub("\\s+", " ", trimws(inner))
        paste0("[", inner, "]", suffix)
    }, character(1L), USE.NAMES = FALSE)
}

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
    if (anyNA(origins))
        stop("'dataOrigin' must not contain NA values. Ensure all ",
             "spectra have a valid dataOrigin (e.g. loaded from a file ",
             "or set manually).")
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
    if (is.na(pol))
        stop("'polarity' is NA for feature '", idx, "'. The Spectra ",
             "object must have polarity set (0 = negative, 1 = positive). ",
             "You can set it with: spectra$polarity <- 1L")
    charge <- if (pol == 0L) -1L else as.integer(pol)
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
        charge = charge,
        detectedAdducts = list(adduct),
        mergedMs1 = .createspectraMS1(ms1),
        ms2Spectra = .createspectraMSn(msn)
    )
}


.importSpectraChunk <- function(chunk, sirius, ms_column_name, data, adducts){
    fdata <- vector("list", length(chunk))

    for (pos in seq_along(chunk)) {
        i <- chunk[pos]
        adduct <- adducts[[as.character(i)]]
        bd_subset <- data[data[[ms_column_name]] == i]
        fdata[[pos]] <- .createfeatures(data = bd_subset, idx = i,
                                        adduct = adduct)
    }
    result <- sirius@api$features_api$AddAlignedFeatures(
        project_id = sirius@projectId,
        fdata
    )
    if (is.null(result) || length(result) == 0)
        warning("Sirius API did not return any features for this chunk. ",
                "This may indicate a mismatch between adduct and polarity ",
                "(charge). Check that your adducts match the ionization ",
                "mode of your data.", call. = FALSE)
    result
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

## SDK connection helpers used by the Sirius() constructor
## to eliminate duplicated verbose / port branching.

#' @noRd
.sdk_connect <- function(sdk, port = integer(), verbose = FALSE) {
    call_fn <- function() {
        if (length(port)) sdk$attach_to_sirius(sirius_port = port)
        else sdk$attach_or_start_sirius()
    }
    if (verbose) call_fn() else suppressMessages(call_fn())
}

#' @noRd
.sdk_start <- function(sdk, port = integer(), verbose = FALSE) {
    call_fn <- function() {
        if (length(port)) sdk$start_sirius(port = port)
        else sdk$start_sirius()
    }
    if (verbose) call_fn() else suppressMessages(call_fn())
}

#' Check whether the user is logged in.
#'
#' Calls the login API and returns \code{TRUE} / \code{FALSE}. Used internally
#' to guard functions that require authentication.
#'
#' @param sirius A \code{Sirius} object.
#' @return \code{TRUE} if the user is logged in, \code{FALSE} otherwise.
#' @noRd
.is_logged_in <- function(sirius) {
    isTRUE(tryCatch(
        sirius@api$login_and_account_api$IsLoggedIn(),
        error = function(e) FALSE
    ))
}

## Developpers help
#' @noRd
.openWebApi <- function(sirius) {
    browseURL(sirius@api$projects_api$api_client$base_path)
}
