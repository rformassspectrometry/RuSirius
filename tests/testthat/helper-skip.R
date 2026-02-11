#' Helper functions for skipping tests when Sirius API is unavailable
#'
#' @noRd

#' Skip test if Sirius API is not available
#'
#' Use this at the start of any test that requires a running Sirius instance.
#' @noRd
skip_if_no_sirius <- function() {
    tryCatch({
        sdk <- RSirius::SiriusSDK$new()
        suppressMessages(api <- sdk$attach_or_start_sirius())
        api$info_api$GetInfo()
    }, error = function(e) {
        testthat::skip("Sirius API not available")
    })
}

#' Skip test if not logged in to Sirius
#'
#' Use this for tests that require authentication.
#' @noRd
skip_if_not_logged_in <- function(sirius) {
    if (!sirius@api$login_and_account_api$IsLoggedIn()) {
        testthat::skip("Not logged in to Sirius")
    }
}

#' Create a temporary Sirius connection for testing
#'
#' Returns NULL if Sirius is not available, allowing tests to skip gracefully.
#' @noRd
test_sirius_connection <- function(projectId = "test_ruSirius") {
    tryCatch({
        suppressMessages(
            srs <- Sirius(projectId = projectId, path = tempdir())
        )
        srs
    }, error = function(e) {
        NULL
    })
}

#' Safely shutdown a Sirius connection
#'
#' Ensures proper cleanup even if errors occur.
#' @noRd
safe_shutdown <- function(srs) {
    if (is.null(srs)) return(invisible(NULL))
    tryCatch({
        if (checkConnection(srs)) {
            shutdown(srs, closeProject = TRUE)
        }
    }, error = function(e) NULL)
    invisible(NULL)
}
