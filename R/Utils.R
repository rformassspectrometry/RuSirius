#' @title Utility function for RuSirius
#' @name Utils
#' @importFrom utils browseURL
NULL

#' @rdname Utils
#' @param username `character(1)`, the username to use for the connection
#' @param password `character(1)`, the password to use for the connection
#' @param sirius a valid `Sirius` object
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

#' @rdname Utils
#' @description returns `TRUE` if the connection to the Sirius is valid,
#' `FALSE` otherwise.
#' @param sirius a `Sirius` object
#' @export
checkConnection <- function(sirius) {
    tryCatch({
        sirius@api$info_api$GetInfo()
        TRUE
    }, error = function(e) {
        FALSE
    })
}

#' @rdname Utils
#'
#' @param sirius object of class `Sirius` with a valid connection
#' to Sirius.
#' @param closeProject `logical`, whether to close the project before
#' shutting down Sirius. Default is `TRUE`.
#'
#' @export
shutdown <- function(sirius, closeProject = TRUE) {
    if (is.null(sirius@sdk$process))
        stop("There is no Sirius process running.")
    if (closeProject) {
        l <- listOpenProjects(sirius)
        if (length(l) > 0) {
            for (i in l) {
                sirius@api$projects_api$CloseProject(project_id = i)
                message("Closed project ", i)
            }
        }
    }
    sirius@sdk$shutdown_sirius()
}

#' @rdname Utils
#' @param sirius a `Sirius` object
#' @export
openWebApi <- function(sirius) {
    browseURL(sirius@api$projects_api$api_client$base_path)
}

#' @rdname Utils
#' @param sirius a `Sirius` object
#' @export
openGUI <- function(sirius) {
    #check that the project is open (open it if necessary)
    sirius@api$gui_api$OpenGui(project_id = sirius@projectId)
}

#' @rdname Utils
#' @param sirius a `Sirius` object.
#' @export
closeGUI <- function(sirius) {
    #check that the GUI is open
    sirius@api$gui_api$CloseGui(project_id = sirius@projectId)
}

#' @rdname Utils
#' @param sirius a `Sirius` object
#' @param infoType `character` vector of length 1 or 2, specifying the type of
#' information to retrieve. Possible values are `"compatibilityInfo"` and
#'`"sizeInformation"`.
#'
#' @export
projectInfo <- function(sirius,
                        infoType = c("compatibilityInfo", "sizeInformation")) {
    info <- sirius@api$projects_api$GetProject(project_id = sirius@projectId,
                                                 opt_fields = infoType)
    info$toSimpleType()
}

#' @rdname Utils
#' @param sirius a `Sirius` object
#' @export
listOpenProjects <- function(sirius) {
    tst <- sirius@api$projects_api$GetProjects()
    unlist(lapply(tst, function(x) x$projectId))
}

#' @rdname Utils
#' @param sirius a `Sirius` object
#' @param projectId `character(1)` specifying the id of the project to open.
#'        can be an already existing project or a new one.
#' @param path `character(1)` path where to find the existing project or where
#'        to create a new one.By default Sirius will open or create a project
#'        in the folder `"C:/Users/<username>/sirius_projects"` fro windows and
#'        `"Sys.getenv("HOME")/sirius-projects"` in other OS.
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
            stop("The 'path' provided does not exist.")
    } else {
        if (.Platform$OS.type == "windows") {
            path <- file.path("C:/Users", Sys.getenv("USERNAME"), "sirius-projects")
        } else {
            path <- file.path(Sys.getenv("HOME"), "sirius-projects")
        }
        if (!file.exists(path)) {
            stop("The default path that Sirius uses does not exist. ",
                 "Either specify a valid path or create the default directory at '",
                 path, "'.")
        }
    }
    f <- file.path(path, paste0(projectId, ".sirius"))
    if (file.exists(f))
       sirius@api$projects_api$OpenProject(project_id = projectId,
                                             path_to_project = f)
    else sirius@api$projects_api$CreateProject(project_id = projectId,
                                            path_to_project = f)
    sirius@projectId <- projectId
    sirius
}

#' @rdname Utils
#' @param sirius a `Sirius` object
#' @param type `character` vector of length 1, specifying the type of features
#'        ID to list. Possible values are "sirius" and "xcms". Default
#'        is `"sirius"`.
#' @export
featuresId <- function(sirius, type = c("sirius", "xcms")) {
    type <- match.arg(type)
    fts <- sirius@api$features_api$GetAlignedFeatures(sirius@projectId)
    if (type == "sirius")
        return(unlist(lapply(fts, function(x) x$alignedFeatureId)))
    else
        return(unlist(lapply(fts, function(x) x$externalFeatureId)))
}

#' @rdname Utils
#' @param sirius a `Sirius` object
#' @export
featuresInfo <- function(sirius) {
    fts <- sirius@api$features_api$GetAlignedFeatures(sirius@projectId)
    l <- lapply(fts, function(x) x$toSimpleType())
    do.call(rbind, l)
}

#' @rdname Utils
#' @param sirius a `Sirius` object
#' @param featureId `character()` vector specifying the id of the feature to remove.
#'            By default remove all.
#' @export
deleteFeatures <- function(sirius, featureId = featuresId(sirius)) {
    if (!all(featureId %in% featuresId(sirius)))
        stop("Some of the featureId provided are not in the project.")
    for (ft in featureId) {
        sirius@api$features_api$DeleteAlignedFeature(sirius@projectId, ft)
        sirius@featureMap <- sirius@featureMap[!sirius@featureMap$fts_sirius %in% ft, ]
    }
    sirius
}

#' @rdname Utils
#' @param sirius a `Sirius` object
#' @export
mapFeatures <- function(sirius) {
    fts_sirius <- featuresId(sirius, type = "sirius")
    fts_xcms <- featuresId(sirius, type = "xcms")
    data.frame(
        fts_sirius = fts_sirius,
        fts_xcms = fts_xcms)
}

#' @rdname Utils
#' @description
#' List the databases that are searchable by Sirius for spectral matching.
#' Can be inputted in??
#'
#' @param sirius a `Sirius` object
#' @export
listDatabases <- function(sirius) {
    l <- sirius@api$searchable_databases_api$GetDatabases()
    jsonlist <- lapply(l, function(x) x$toSimpleType())
    data.frame(do.call(bind_rows, jsonlist)) # need to be fixed, pb with number of columns
}

#' @rdname Utils
#' @param sirius a `Sirius` object
#' @param config a `configSirius` object
#' @param name `character` vector of length 1, specifying the name of the
#' configuration to load.
#' @export
saveConfig <- function(sirius, config, name) {
    if (!length(name))
        stop("Please provide a name for the configuration.")
    sirius@api@jobs_api$SaveJobConfig(name, config) # see where it saves it.
}

## jobs
#' @rdname Utils
#' @param sirius a `Sirius` object
#' @param jobId `character(1)` specifying the id of the job to retrieve.
#' @export
jobInfo <- function(sirius, jobId = character()) {
    if (!length(jobId))
        stop("Please provide a jobId that you wish to retrieve")
    j <- sirius@api$jobs_api$GetJob(sirius@projectId, jobId,
                            c("command", "progress", "affectedIds"))
    return(cat(.clean_output(j)))
}

#' @rdname Utils
#' @param sirius a `Sirius` object
#' @param jobId `character(1)` specifying the id of the job to delete.
#' @param all `logical`, whether to delete all jobs. Default is `FALSE`.
#' @export
deleteJob <- function(sirius, jobId = character(), all = FALSE) {
    if (!all) {
        if (!length(jobId))
            stop("Please provide a jobId that you wish to delete or set",
                 "all = FALSE.")
        sirius@api$jobs_api$DeleteJob(sirius@projectId, jobId)
    }
    else sirius@api$jobs_api$DeleteJob(sirius@projectId)
}


##cleaning jobInfo output - move to helper file
# Function to format and clean settings
.format_command <- function(command) {
    cleaned_command <- ""
    l <- unlist(strsplit(command, " "))
    l <- l[l != "config"]
    lp <- paste0(l, collapse = "\n")
    cleaned_command <- paste0(cleaned_command, lp)
    cleaned_command
}

# Function to clean and format job output
.clean_output <- function(output) {
    cleaned_output <- ""
    cleaned_output <- paste0(cleaned_output, "Job ID: ", output$id, "\n\n")

    cleaned_output <- paste0(cleaned_output, "Command: \n",
                             .format_command (output$command), "\n\n")

    cleaned_output <- paste0(cleaned_output, "Progress: \n")
    cleaned_output <- paste0(cleaned_output, "   State: ",
                             output$progress$state, "\n")
    cleaned_output <- paste0(cleaned_output, "   Current Progress: ",
                             output$progress$currentProgress, "\n")
    cleaned_output <- paste0(cleaned_output, "   Max Progress: ",
                             output$progress$maxProgress, "\n\n")

    cleaned_output <- paste0(cleaned_output, "Affected Compound IDs: \n")
    cleaned_output <- paste0(cleaned_output, "   ",
                             paste(output$affectedCompoundIds,
                                   collapse = ", "), "\n\n")

    cleaned_output <- paste0(cleaned_output, "Affected Aligned Feature IDs: \n")
    for (i in seq_along(output$affectedAlignedFeatureIds)) {
        cleaned_output <- paste0(cleaned_output,
                                 paste(output$affectedAlignedFeatureIds[[i]],
                                       collapse = ", "), "\n")
    }
    return(cleaned_output)
}

