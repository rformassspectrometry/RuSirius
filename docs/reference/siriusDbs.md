# Functions for Handling Sirius Databases

This set of functions provides tools for managing databases in Sirius.
These include listing available databases, retrieving database
information, deleting databases, and creating new databases from files.

List the databases that are searchable by Sirius for spectral matching.

Retrieve detailed information about a specific database.

Delete a database from Sirius.

Create a new database in Sirius from specified files.

## Usage

``` r
listDbs(sirius)

infoDb(sirius, databaseId = character())

removeDb(sirius, databaseId = character())

createDb(
  sirius,
  databaseId = character(),
  files = character(),
  location = getwd()
)
```

## Arguments

- sirius:

  A `Sirius` object representing the Sirius connection.

- databaseId:

  A `character(1)` string specifying the new database ID.

- files:

  A `character` vector of file paths to add to the database.

- location:

  A `character(1)` string specifying the location for the database.
  Defaults to the current working directory.

## Value

A `data.frame` containing details of searchable databases.

A `list` containing details of the specified database.

A `logical(1)` indicating whether the database was successfully removed.

A `list` containing details of the created database.
