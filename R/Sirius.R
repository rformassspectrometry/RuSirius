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
setClass(
  "Sirius",
  slots = c(
    api = "ANY",
    sdk = "ANY",
    projectId = "character",
    featureMap = "data.frame",
    toolUsed = "ANY"
  ),
  contains = "CompAnnotationSource",
  prototype = prototype(
    username = character(),
    password = character(),
    api = NULL,
    sdk = NULL,
    projectId = character(),
    featureMap = data.frame(),
    toolUsed = NULL
  ),
  validity = function(object) {
    if (length(object@username) && !length(object@password)) {
      stop("A username was provided but no password.")
    }
    if (!length(object@username) && length(object@password)) {
      stop("A password was provided but no username.")
    }
    if (length(object@projectId) > 1) {
      stop("projectId should be a character vector of length 1.")
    }
  }
)

#' @rdname Sirius
#' @importFrom RSirius SiriusSDK
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
#'
#' @param password `character(1)`, the password to use for the connection
#'
#' @param projectId `character(1)`, the project id to use for the connection
#'
#' @param path `character(1)` path where to find the existing project or where
#'        to create a new one. By default, the project will be opened in the
#'        current `"."` directory.
#'
#' @param port `integer(1)` defining the port for Sirius. Allows to manually
#'        define the port to connect to. If not specified (the default) the
#'        port information will be read from a Sirius-config file.
#'
#' @param verbose `logical(1)`, if `TRUE` the function will print all messages
#'       to the console. Use if need debug, default is `FALSE`.
#'
#' @return `Sirius` object with the Sirius api connected.
#'
#' @importFrom RSirius AccountCredentials
#' @importFrom jsonlite fromJSON
#'
#' @export
Sirius <- function(
  username = character(),
  password = character(),
  projectId = character(),
  path = character(),
  port = integer(),
  verbose = FALSE
) {
  srs <- new("Sirius")
  srs@sdk <- SiriusSDK$new()
  if (length(port)) {
    srs@sdk$port <- port
  }
  srs@api <- .sdk_connect(srs@sdk, port, verbose)

  max_retries <- 5
  attempts <- 0
  while (!checkConnection(srs) && attempts < max_retries) {
    attempts <- attempts + 1
    message(
      "Attempt ",
      attempts,
      ": Sirius application is not starting ",
      "properly, attempting to re-start.\n"
    )
    shutdown(srs, closeProject = FALSE)
    Sys.sleep(2)
    srs@sdk <- SiriusSDK$new()
    srs@api <- .sdk_start(srs@sdk, port, verbose)
  }

  if (!checkConnection(srs)) {
    stop(
      "Failed to establish a connection to Sirius after ",
      max_retries,
      " attempts."
    )
  }
  info <- tryCatch(srs@api$info_api$GetInfo(), error = function(e) NULL)
  if (!is.null(info)) {
    message(
      "Connection established, Sirius version in use is ",
      info[["siriusVersion"]]
    )
  } else {
    message("Connection established.")
  }
  logged_in <- tryCatch(
    srs@api$login_and_account_api$IsLoggedIn(),
    error = function(e) FALSE
  )
  if (!isTRUE(logged_in)) {
    if (length(username) && length(password)) {
      srs <- logIn(srs, username, password)
      message("You are now logged in.")
    } else {
      message(
        "You are not logged in and no credentials were provided. ",
        "Some features may be limited. ",
        "Use `logIn()` to authenticate."
      )
    }
  } else {
    message("You are already logged in.")
  }
  if (length(projectId)) {
    tryCatch(
      {
        srs <- openProject(sirius = srs, projectId = projectId, path = path)
        if (isTRUE(projectInfo(srs)$numOfFeatures != 0)) {
          srs@featureMap <- mapFeatures(srs)
        }
      },
      error = function(e) {
        message("The project could not be loaded: ", conditionMessage(e))
      }
    )
  } else {
    message(
      "No project loaded as none was given in parameters ",
      "'projectId'. Run `openProject()` to load a project."
    )
  }
  srs
}

#' @importFrom methods show
#' @param object `Sirius`, the object to show,
#' @export
#' @rdname Sirius
setMethod("show", signature(object = "Sirius"), definition = function(object) {
  cat("Sirius object\n")
  valid <- checkConnection(object)
  cat("Valid connection to Sirius: ", valid, "\n")
  if (!valid) {
    return(cat("Try to restart Sirius\n"))
  }
  logged_in <- tryCatch(
    object@api$login_and_account_api$IsLoggedIn(),
    error = function(e) NA
  )
  cat("Logged In: ", logged_in, "\n")
  info <- tryCatch(
    object@api$info_api$GetInfo(),
    error = function(e) NULL
  )
  if (!is.null(info)) {
    cat("Sirius version: ", info[["siriusVersion"]], "\n")
    cat("Sirius update available: ", info[["updateAvailable"]], "\n")
  }
  if (length(object@projectId)) {
    cat("Project ID: ", object@projectId, "\n")
    pinfo <- tryCatch(projectInfo(object), error = function(e) NULL)
    if (!is.null(pinfo)) {
      cat("Number of features in the project: ", pinfo$numOfFeatures, "\n")
    }
    j <- tryCatch(
      object@api$jobs_api$GetJobs(
        object@projectId,
        c("command", "progress", "affectedIds")
      ),
      error = function(e) NULL
    )
    if (!is.null(j) && length(j) > 0) {
      ids <- as.numeric(vapply(
        j,
        function(x) {
          x[["id"]]
        },
        character(1)
      ))
      cat("Job ids available: ", ids, "\n")
      cat("State of latest job: ", j[[which.max(ids)]]$progress$state, "\n")
    } else {
      cat("No job was run/is running on this project.\n")
    }
  } else {
    cat("No project is currently loaded\n")
  }
})
