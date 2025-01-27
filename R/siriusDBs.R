#' @title Functions for Handling Sirius Databases
#'
#' @name siriusDBs
#' @description
#' This set of functions provides tools for managing databases in Sirius. These
#' include listing available databases, retrieving database information,
#' deleting databases, and creating new databases from files.
#'
NULL

#' @rdname siriusDBs
#' @description
#' List the databases that are searchable by Sirius for spectral matching.
#'
#' @param sirius A `Sirius` object representing the Sirius connection.
#'
#' @return A `data.frame` containing details of searchable databases.
#' @export
listDBs <- function(sirius) {
    if (!checkConnection(sirius)) {
        stop("The connection to the Sirius instance is not valid.")
    }
    db_list <- sirius@api$searchable_databases_api$GetDatabases()
    json_list <- lapply(db_list, function(x) x$toSimpleType())
    data.frame(do.call(bind_rows, json_list))
}

#' @rdname siriusDBs
#' @description
#' Retrieve detailed information about a specific database.
#'
#' @param sirius A `Sirius` object representing the Sirius connection.
#' @param databaseId A `character(1)` string specifying the database ID.
#'
#' @return A `list` containing details of the specified database.
#' @export
infoDb <- function(sirius, databaseId = character()) {
    if (!checkConnection(sirius)) {
        stop("The connection to the Sirius instance is not valid.")
    }
    if (!nzchar(databaseId)) {
        stop("Please provide a valid database ID.")
    }
    sirius@api$searchable_databases_api$GetDatabase(databaseId)$toSimpleType()
}

#' @rdname siriusDBs
#' @description
#' Delete a database from Sirius.
#'
#' @param sirius A `Sirius` object representing the Sirius connection.
#' @param databaseId A `character(1)` string specifying the database ID to delete.
#'
#' @export
removeDb <- function(sirius, databaseId = character()) {
    if (!checkConnection(sirius))
        stop("The connection to the Sirius instance is not valid.")
    if (!nzchar(databaseId)) {
        stop("Please provide a valid database ID.")
    }
    sirius@api$searchable_databases_api$RemoveDatabase(databaseId)
    TRUE
}

#' @rdname siriusDBs
#' @description
#' Create a new database in Sirius from specified files.
#'
#' @param sirius A `Sirius` object representing the Sirius connection.
#' @param databaseId A `character(1)` string specifying the new database ID.
#' @param files A `character` vector of file paths to add to the database.
#' @param location A `character(1)` string specifying the location for the
#'        database. Defaults to the current working directory.
#'
#' @return A `list` containing details of the created database.
#'
#' @export
createDB <- function(sirius, databaseId = character(), files = character(),
                     location = getwd()) {
    if (!checkConnection(sirius))
        stop("The connection to the Sirius instance is not valid.")
    if (!length(databaseId) || length(databaseId) > 1 || !is.character(databaseId))
        stop("Please provide a single valid database ID.")
    if (length(files) == 0 || !all(file.exists(files)))
        stop("Please provide valid file(s) that exist.")
    if (length(location) != 1 || !dir.exists(location))
        stop("Please provide a valid directory path for the `location` parameter.")

    db_path <- file.path(location, paste0(databaseId, ".siriusdb"))

    tryCatch({
        sirius@api$searchable_databases_api$CreateDatabase(database_id = databaseId, data_file = db_path)
        db <- sirius@api$searchable_databases_api$ImportIntoDatabase(database_id = databaseId, input_file = files)
    }, error = function(e) {
        stop("Failed to create the database: ", e$message)
    })

    db$toSimpleType()
}
