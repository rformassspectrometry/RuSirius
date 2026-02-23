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
                suppressMessages(openProject(srs, projectId, path = tempdir())),
                error = function(e) srs
            )
            .shared_sirius_env$srs <- srs
        }
        return(srs)
    }
    srs <- tryCatch(
        suppressMessages(Sirius(projectId = projectId, path = tempdir())),
        error = function(e) NULL
    )
    if (is.null(srs) || !checkConnection(srs)) {
        .shared_sirius_env$available <- FALSE
        return(NULL)
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
    if (!sirius@api$login_and_account_api$IsLoggedIn())
        testthat::skip("Not logged in to Sirius")
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
