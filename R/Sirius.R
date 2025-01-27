#' @title Connection to a Sirius instance
#'
#' @name Sirius
#'
#' @export
#'
#' @description
#' Creates a `Sirius` instance and checks that connection to the server
#' is valid. returns the Api and SDK within its slots.
#' Main object that the user will interact with to connect to the Sirius server
#' and perform operations.
#'
#' @slot api `ANY`, the api object to use for the connection
#' @slot sdk `ANY`, the sdk object to use for the connection
#' @slot projectId `character`, the project id to use for the connection
#' @slot featureMap `data.frame`, the feature map to use for the connection
#'
#' @importFrom methods new setClass
#' @importClassesFrom MetaboAnnotation CompAnnotationSource
#'
#' @author Philippine Louail (+people that worked on the API)
#'
#'
NULL

#' @noRd
setClass("Sirius",
         slots = c(api = "ANY",
                   sdk = "ANY",
                   projectId = "character",
                   featureMap = "data.frame",
                   toolUsed = "ANY"),
         contains = "CompAnnotationSource",
         prototype = prototype(username = character(),
                               password = character(), api = NULL, sdk = NULL,
                               projectId = character(),
                               featureMap = data.frame(),
                               toolUsed = NULL
                               ),
         validity = function(object) {
             if (length(object@username) && !length(object@password))
                 stop("A username was provided but no password.")
             if (!length(object@username) && length(object@password))
                 stop("A password was provided but no username.")
             if (length(object@projectId) > 1)
                 stop("projectId should be a character vector of length 1.")
         }
)

#' @rdname Sirius
#' @importFrom Rsirius SiriusSDK
#'
#' @description
#'  Creates a `Sirius` object and checks that the connection to the
#'  Sirius server is valid. If the Sirius server is not running, the function
#'  will attempt to start it using the provided path to the executable. If the
#'  connection is not valid, the function will attempt to log in using the
#'  provided credentials. If the connection is still not valid, the function
#'  will stop with an error message.
#'
#' @param username `character(1)`, the username to use for the connection
#' @param password `character(1)`, the password to use for the connection
#' @param projectId `character(1)`, the project id to use for the connection
#' @param path `character(1)` path where to find the existing project or where
#'        to create a new one.By default Sirius will open or create a project
#'        in the folder `"C:/Users/<username>/sirius_projects"` for windows and
#'        `"Sys.getenv("HOME")/sirius-projects"` in other OS. It will not be
#'        created automatically, if you want to use this default please create
#'        it beforehand.
#' @param verbose `logical(1)`, if `TRUE` the function will print all messages
#'       to the console. Use if need debug, default is `FALSE`.
#'
#' @return `Sirius` object with the Sirius api connected.
#'
#' @importFrom Rsirius AccountCredentials
#' @importFrom jsonlite fromJSON
#'
#' @export
Sirius <- function(username = character(), password = character(),
                   projectId = character(), path = character(),
                   verbose = FALSE) {
    srs <- new("Sirius")
    srs@sdk <- SiriusSDK$new()

    if (verbose) srs@api <- srs@sdk$attach_or_start_sirius()
    else
        suppressMessages(srs@api <- srs@sdk$attach_or_start_sirius())

    max_retries <- 5
    attempts <- 0
    while (!checkConnection(srs) && attempts < max_retries) {
        attempts <- attempts + 1
        message("Attempt: Sirius application is not starting ",
                        "properly, attempting to re-start.", attempts, "/n")
        shutdown(srs, closeProject = FALSE)
        srs@sdk <- SiriusSDK$new()
        if (verbose) srs@api <- srs@sdk$start_sirius()
        else
            suppressMessages(srs@api <- srs@sdk$start_sirius())
    }

    if (!checkConnection(srs)) {
        stop("Failed to establish a connection to Sirius after ", max_retries,
             " attempts.")
    }
    message("Connection established, Sirius version in use is ",
            srs@api$info_api$GetInfo()[["siriusVersion"]])
    if (!srs@api$login_and_account_api$IsLoggedIn()) {
        if (!length(username) || !length(password)) {
            message("You are not logged in and no credentials were provided.",
                    "Please use `logIn()` function ",
                    "and then `openProject()` to open the project wanted")
            return(srs)
        }
        srs <- logIn(srs, username, password)
        message("You are now logged in.")
    } else message("You are already logged in.")
    if (length(projectId)) {
        tryCatch({
            srs <- openProject(sirius = srs, projectId = projectId, path = path)
            if (projectInfo(srs)$numOfFeatures != 0)
                srs@featureMap <- mapFeatures(srs)
            }, error = function(e) {
                message("The project could not be loaded, please check the ",
                        "projectId or the path.")
                })
        } else message("No project loaded as none was given in parameters ",
                       "'projectId' run `openProject()` to load a project.")
    srs
}

#' @importFrom methods show
#' @param object `Sirius`, the object to show,
#' @export
#' @rdname Sirius
setMethod("show", signature(object = "Sirius"),
          definition = function(object) {
              cat("Sirius object\n")
              cat("Valid connection to Sirius: ", checkConnection(object), "\n")
              cat("Logged In: ", object@api$login_and_account_api$IsLoggedIn(),
                  "\n")
              cat("Sirius version: ",
                  object@api$info_api$GetInfo()[["siriusVersion"]], "\n")
              cat("Sirius update available: ",
                  object@api$info_api$GetInfo()[["updateAvailable"]], "\n")
              if (length(object@projectId)) {
                  cat("Project ID: ", object@projectId, "\n")
                  cat("Number of features in the project: ",
                      projectInfo(object)$numOfFeatures, "\n")
                  j <- object@api$jobs_api$GetJobs(object@projectId,
                                                   c("command", "progress", "affectedIds"))
                  if (!is.null(j)) {
                      ids <- as.numeric(sapply(j, `[[`, "id"))
                      cat("Job ids available: ", ids, "\n")
                      cat("State of latest job: ", j[[which.max(ids)]]$progress$state, "\n")
                  } else cat("No job was run/is running on this project. \n")
              } else cat("No project is currently loaded\n")
          }
)
