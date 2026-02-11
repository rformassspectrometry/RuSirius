#' @title Utility function for RuSirius
#' @name utils
#' @importFrom utils browseURL
NULL

# Internal helper for GUI API calls
.callGuiApi <- function(sirius, method, query_params = list()) {
    url <- paste0(sirius@api$api_client$base_path, 
                  "/api/projects/", sirius@projectId, "/gui")
    sirius@api$api_client$CallApi(
        url = url,
        method = method,
        query_params = query_params,
        header_params = c(),
        form_params = list(),
        file_params = list(),
        accepts = list("application/json"),
        content_types = list(),
        body = NULL,
        is_oauth = FALSE,
        oauth_scopes = NULL
    )
}

#' @rdname utils
#' @param username `character(1)`, the username to use for the connection
#' @param password `character(1)`, the password to use for the connection
#' @param sirius a valid `Sirius` object
#'
#' @return A `message` whether the user is successfully logged in or not.
#'
#' @export
logIn <- function(sirius, username, password) {
    if (!length(username) && !length(password))
        stop("Please provide a username and a password.")
    if (!checkConnection(sirius))
        stop("The connection to the Sirius instance is not valid.")
    cred <- AccountCredentials$new(username = username,
                                   password = password)
    tryCatch(sirius@api$login_and_account_api$Login(accept_terms = TRUE,
                                                    account_credentials = cred),
             error = function(e) {
                 stop("We could not log you in. Please check the provided ",
                         "credential")
             })
    message("You are now logged in.")
    sirius
}

#' @rdname utils
#' @description returns `TRUE` if the connection to the Sirius is valid,
#' `FALSE` otherwise.
#' @param sirius a `Sirius` object
#'
#' @return `logical`, `TRUE` if the connection is valid, `FALSE` otherwise.
#'
#' @export
checkConnection <- function(sirius) {
    tryCatch({
        sirius@api$info_api$GetInfo()
        TRUE
    }, error = function(e) {
        FALSE
    })
}


#' @rdname utils
#'
#' @param sirius object of class `Sirius` with a valid connection
#' to Sirius.
#' @param closeProject `logical`, whether to close the project before
#' shutting down Sirius. Default is `TRUE`.
#'
#' @return Invisible `NULL`. Messages indicate shutdown status.
#'
#' @export
shutdown <- function(sirius, closeProject = TRUE) {
    if (closeProject) {
        projects <- tryCatch(listOpenProjects(sirius), error = function(e) NULL)
        for (p in projects) {
            tryCatch(
                sirius@api$projects_api$CloseProject(project_id = p),
                error = function(e) NULL
            )
        }
    }
    tryCatch(
        sirius@sdk$shutdown_sirius(),
        error = function(e) 
            message("Could not shutdown Sirius. You may need to close it manually.")
    )
    invisible(NULL)
}

#' @rdname utils
#' @param sirius a `Sirius` object
#' @return Invisible `TRUE` if successful.
#' @export
openGUI <- function(sirius) {
    if (!length(sirius@projectId))
        stop("No project is currently open. Use openProject() first.")
    resp <- .callGuiApi(sirius, method = "POST")
    if (resp$status_code >= 200 && resp$status_code <= 299) {
        message("GUI opened for project: ", sirius@projectId)
        invisible(TRUE)
    } else {
        stop("Failed to open GUI. Status code: ", resp$status_code)
    }
}

#' @rdname utils
#' @param sirius a `Sirius` object.
#' @param closeProject `logical`, whether to also close the project when
#'        closing the GUI. Default is `FALSE`.
#' @return Invisible `TRUE` if successful.
#' @export
closeGUI <- function(sirius, closeProject = FALSE) {
    if (!length(sirius@projectId))
        stop("No project is currently open.")
    query <- if (closeProject) list(closeProject = "true") else list()
    resp <- .callGuiApi(sirius, method = "DELETE", query_params = query)
    if (resp$status_code >= 200 && resp$status_code <= 299) {
        message("GUI closed for project: ", sirius@projectId)
        invisible(TRUE)
    } else {
        stop("Failed to close GUI. Status code: ", resp$status_code)
    }
}

#' @rdname utils
#' @param sirius a `Sirius` object
#' @param infoType `character` vector of length 1 or 2, specifying the type of
#' information to retrieve. Possible values are `"compatibilityInfo"` and
#'`"sizeInformation"`.
#' @return a `list` with the information requested.
#' @export
projectInfo <- function(sirius,
                        infoType = c("compatibilityInfo", "sizeInformation")) {
    info <- sirius@api$projects_api$GetProject(project_id = sirius@projectId,
                                                 opt_fields = infoType)
    info$toSimpleType()
}

#' @rdname utils
#' @param sirius a `Sirius` object
#' @return a `character` vector with the open projects.
#' @export
listOpenProjects <- function(sirius) {
    tst <- tryCatch(
        sirius@api$projects_api$GetProjects(),
        error = function(e) {
            warning("Could not retrieve open projects: ", conditionMessage(e))
            return(list())
        }
    )
    if (length(tst) == 0) return(character(0))
    unlist(lapply(tst, function(x) x$projectId))
}

#' @rdname utils
#' @param sirius a `Sirius` object
#' @param projectId `character(1)` specifying the id of the project to open.
#'        can be an already existing project or a new one.
#' @param path `character(1)` path where to find the existing project or where
#'        to create a new one.By default, the porject will be opened in the
#'        current `"."` directory.
#' @return a `Sirius` object with the project opened.
#' @export
openProject <- function(sirius, projectId, path = character()) {
    if (!checkConnection(sirius))
        stop("The connection to the Sirius instance is not valid.")
    l <- listOpenProjects(sirius)
    if (!is.null(l) && length(l) > 0) {
        for (project in l) {
            sirius@api$projects_api$CloseProject(project_id = project)
            message("Closed project ", project)
        }
    }
    if (length(path) > 0) {
        if (!file.exists(path))
            stop("The provided 'path' does not exist.")
    } else path <- getwd()
    f <- file.path(path, paste0(projectId, ".sirius"))
    if (file.exists(f))
       sirius@api$projects_api$OpenProject(project_id = projectId,
                                             path_to_project = f)
    else sirius@api$projects_api$CreateProject(project_id = projectId,
                                            path_to_project = f)
    sirius@projectId <- projectId
    sirius
}

#' @rdname utils
#' @param sirius a `Sirius` object
#' @param type `character` vector of length 1, specifying the type of features
#'        ID to list. Possible values are `"sirius"` and `"xcms"`. Default
#'        is `"sirius"`.
#' @return a `character` vector with the features ID (empty if no features).
#' @export
featuresId <- function(sirius, type = c("sirius", "xcms")) {
    type <- match.arg(type)
    fts <- sirius@api$features_api$GetAlignedFeatures(sirius@projectId)
    if (length(fts) == 0) return(character(0))
    field <- if (type == "sirius") "alignedFeatureId" else "externalFeatureId"
    as.character(vapply(fts, function(x) x[[field]], character(1)))
}

#' @rdname utils
#' @param sirius a `Sirius` object
#' @return a `data.frame` with the features information.
#' @export
featuresInfo <- function(sirius) {
    fts <- sirius@api$features_api$GetAlignedFeatures(sirius@projectId)
    l <- lapply(fts, function(x) x$toSimpleType())
    do.call(rbind, l)
}

#' @rdname utils
#' @param sirius a `Sirius` object
#' @param featureId `character()` vector specifying the id of the feature to
#'        remove. By default remove all.
#' @return A `Sirius` object with the features removed.
#' @export
deleteFeatures <- function(sirius, featureId = featuresId(sirius)) {
    if (!all(featureId %in% featuresId(sirius)))
        stop("Some of the featureId provided are not in the project.")
    for (ft in featureId) {
        sirius@api$features_api$DeleteAlignedFeature(sirius@projectId, ft)
        sirius@featureMap <- sirius@featureMap[!sirius@featureMap$fts_sirius
                                               %in% ft, ]
    }
    sirius
}

#' @rdname utils
#' @param sirius a `Sirius` object
#' @return a `data.frame` with the features mapping.
#' @export
mapFeatures <- function(sirius) {
    fts_sirius <- featuresId(sirius, type = "sirius")
    fts_xcms <- featuresId(sirius, type = "xcms")
    data.frame(
        fts_sirius = fts_sirius,
        fts_xcms = fts_xcms)
}

#' @rdname utils
#' @param sirius a `Sirius` object
#' @param config a `configSirius` object
#' @param name `character` vector of length 1, specifying the name of the
#' configuration to load.
#' @return nothing, will save the configuration locally.
#' @export
saveConfig <- function(sirius, config, name) {
    if (!length(name))
        stop("Please provide a name for the configuration.")
    sirius@api@jobs_api$SaveJobConfig(name, config) # see where it saves it.
}

#' @rdname utils
#' @param sirius a `Sirius` object
#' @param jobId `character(1)` specifying the id of the job to retrieve.
#' @return a `character` vector with the job information.
#' @export
jobInfo <- function(sirius, jobId = character()) {
    if (!length(jobId))
        stop("Please provide a jobId that you wish to retrieve")
    j <- sirius@api$jobs_api$GetJob(sirius@projectId, jobId,
                                    c("command", "progress", "affectedIds"))
    return(.clean_output(j))
}

#' @rdname utils
#' @param sirius a `Sirius` object
#' @param jobId `character(1)` specifying the id of the job to delete.
#' @param all `logical`, whether to delete all jobs. Default is `FALSE`.
#' @return nothing, will delete the job from Sirius.
#' @export
deleteJob <- function(sirius, jobId = character(), all = FALSE) {
    if (!all) {
        if (!length(jobId))
            stop("Please provide a jobId that you wish to delete or set",
                 "all = TRUE to delete all jobs.")
        sirius@api$jobs_api$DeleteJob(sirius@projectId, jobId)
    } else {
        sirius@api$jobs_api$DeleteJob(sirius@projectId)
    }
}
