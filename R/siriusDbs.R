#' @title Functions for Handling Sirius Databases
#'
#' @aliases listDbs infoDb removeDb createDb
#'
#' @name siriusDbs
#'
#' @param sirius A `Sirius` object representing the Sirius connection.
#'
#' @description
#' This set of functions provides tools for managing databases in Sirius. These
#' include listing available databases, retrieving database information,
#' deleting databases, and creating new databases from files.
#'
NULL

#' @rdname siriusDbs
#' @description
#' List the databases that are searchable by Sirius for spectral matching.
#'
#' @return A `data.frame` containing details of searchable databases.
#' @export
listDbs <- function(sirius) {
    if (!checkConnection(sirius)) {
        stop("The connection to the Sirius instance is not valid.")
    }
    if (!.is_logged_in(sirius))
        stop("You must be logged in to list databases. Use logIn() first.")
    db_list <- sirius@api$searchable_databases_api$GetDatabases()
    json_list <- lapply(db_list, function(x) x$toSimpleType())
    data.frame(do.call(bind_rows, json_list))
}

#' @rdname siriusDbs
#' @description
#' Retrieve detailed information about a specific database.
#'
#' @param databaseId A `character(1)` string specifying the database ID.
#'
#' @return A `list` containing details of the specified database.
#' @export
infoDb <- function(sirius, databaseId = character()) {
    if (!checkConnection(sirius)) {
        stop("The connection to the Sirius instance is not valid.")
    }
    if (!.is_logged_in(sirius))
        stop("You must be logged in to retrieve database info. Use logIn() first.")
    if (!nzchar(databaseId)) {
        stop("Please provide a valid database ID.")
    }
    db <- sirius@api$searchable_databases_api$GetDatabase(databaseId)
    db$toSimpleType()
}

#' @rdname siriusDbs
#' @description
#' Delete a database from Sirius.
#' @param databaseId A `character(1)` string specifying the database ID to
#'        delete.
#' @param delete `logical(1)`, whether to also delete the database file from
#'        disk. If `FALSE` (default) the database is only removed from Sirius
#'        and its file is kept.
#'
#' @return A `logical(1)` indicating whether the database was successfully
#'         removed.
#'
#' @export
removeDb <- function(sirius, databaseId = character(), delete = FALSE) {
    if (!checkConnection(sirius))
        stop("The connection to the Sirius instance is not valid.")
    if (!.is_logged_in(sirius))
        stop("You must be logged in to remove a database. Use logIn() first.")
    if (!nzchar(databaseId)) {
        stop("Please provide a valid database ID.")
    }
    sirius@api$searchable_databases_api$RemoveDatabase(databaseId,
                                                       delete = delete)
    TRUE
}

#' @rdname siriusDbs
#' @description
#' Create a new database in Sirius from specified files.
#' @param databaseId A `character(1)` string specifying the new database ID.
#' @param files A `character` vector of file paths to add to the database.
#' @param location A `character(1)` string specifying the location for the
#'        database. Defaults to the current working directory.
#'
#' @return A `list` containing details of the created database.
#'
#' @export
createDb <- function(sirius, databaseId = character(), files = character(),
                     location = getwd()) {
    if (!checkConnection(sirius))
        stop("The connection to the Sirius instance is not valid.")
    if (!.is_logged_in(sirius))
        stop("You must be logged in to create a database. Use logIn() first.")
    if (!length(databaseId) || length(databaseId) > 1 ||
        !is.character(databaseId))
        stop("Please provide a single valid database ID.")
    if (length(files) == 0 || !all(file.exists(files)))
        stop("Please provide valid file(s) that exist.")
    if (length(location) != 1 || !dir.exists(location))
        stop("Please provide a valid directory path for the `location` ",
             "parameter.")

    db_path <- file.path(normalizePath(location), paste0(databaseId, ".siriusdb"))
    params <- RSirius::SearchableDatabaseParameters$new(location = db_path)
    api <- sirius@api$searchable_databases_api
    .check_db_response(api$CreateDatabase(database_id = databaseId,
                                          searchable_database_parameters = params),
                       databaseId)
    db <- .check_db_response(api$ImportIntoDatabase(database_id = databaseId,
                                                    input_file = files),
                             databaseId)
    db$toSimpleType()
}

## Stop with the server's message when the API returns an error response
.check_db_response <- function(res, databaseId) {
    if (inherits(res, "ApiResponse")) {
        detail <- tryCatch(jsonlite::fromJSON(res$response_as_text())$detail,
                           error = function(e) NULL)
        stop("Failed to create database '", databaseId, "' (HTTP ",
             res$status_code, ")",
             if (length(detail)) paste0(": ", detail) else ".", call. = FALSE)
    }
    res
}
