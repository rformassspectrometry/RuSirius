# Shared Sirius connection -------------------------------------------------
# A single Sirius instance is lazily created and reused across all tests.
# Shutdown is handled in teardown.R.

.shared_sirius_env <- new.env(parent = emptyenv())
.shared_sirius_env$srs <- NULL
.shared_sirius_env$available <- NA

#' Return (or lazily create) the shared Sirius connection.
#' @noRd
get_shared_sirius <- function(projectId = "test_ruSirius") {
    if (isFALSE(.shared_sirius_env$available)) return(NULL)
    if (!is.null(.shared_sirius_env$srs) &&
        checkConnection(.shared_sirius_env$srs)) {
        srs <- .shared_sirius_env$srs
        if (!identical(srs@projectId, projectId)) {
            srs <- tryCatch(
                suppressWarnings(suppressMessages(
                    openProject(srs, projectId, path = tempdir())
                )),
                error = function(e) srs
            )
            .shared_sirius_env$srs <- srs
        }
        return(srs)
    }

    ## Connection died or never existed — clean up before retrying.
    if (!is.null(.shared_sirius_env$srs)) {
        tryCatch(
            .shared_sirius_env$srs@sdk$shutdown_sirius(),
            error = function(e) NULL
        )
        .shared_sirius_env$srs <- NULL
        Sys.sleep(3)
    }

    srs <- tryCatch(
        suppressMessages(
            suppressWarnings(Sirius(projectId = projectId, path = tempdir()))
        ),
        error = function(e) NULL
    )
    if (is.null(srs) || !checkConnection(srs)) {
        .shared_sirius_env$available <- FALSE
        return(NULL)
    }
    ## If projectId wasn't set during construction (e.g. API error during
    ## project creation), try to open the project explicitly now.
    if (!length(srs@projectId)) {
        srs <- tryCatch(
            suppressWarnings(suppressMessages(
                openProject(srs, projectId, path = tempdir())
            )),
            error = function(e) srs
        )
    }
    .shared_sirius_env$available <- TRUE
    .shared_sirius_env$srs <- srs
    srs
}

#' @noRd
skip_if_no_sirius <- function() {
    srs <- get_shared_sirius()
    if (is.null(srs)) testthat::skip("Sirius API not available")
}

#' @noRd
skip_if_not_logged_in <- function(sirius) {
    logged_in <- tryCatch(
        sirius@api$login_and_account_api$IsLoggedIn(),
        error = function(e) FALSE
    )
    if (!isTRUE(logged_in))
        testthat::skip("Not logged in to Sirius")
}

#' @noRd
skip_if_no_project <- function(sirius) {
    if (!length(sirius@projectId))
        testthat::skip("No project loaded")
}

#' Returns the shared connection. Used by individual tests.
#' @noRd
test_sirius_connection <- function(projectId = "test_ruSirius") {
    get_shared_sirius(projectId)
}

#' No-op — shutdown is handled in teardown.R.
#' Kept so existing on.exit(safe_shutdown(srs)) calls don't error.
#' @noRd
safe_shutdown <- function(srs) invisible(NULL)
