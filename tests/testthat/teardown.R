# Shut down the shared Sirius instance after all tests complete
srs <- .shared_sirius_env$srs
if (!is.null(srs)) {
  tryCatch(
    if (checkConnection(srs)) shutdown(srs, closeProject = TRUE),
    error = function(e) NULL
  )
}
.shared_sirius_env$srs <- NULL
