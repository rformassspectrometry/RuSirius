# Utility function for RuSirius

returns `TRUE` if the connection to the Sirius is valid, `FALSE`
otherwise.

## Usage

``` r
logIn(sirius, username, password)

checkConnection(sirius)

shutdown(sirius, closeProject = TRUE)

openGUI(sirius)

closeGUI(sirius, closeProject = FALSE)

projectInfo(sirius, infoType = c("compatibilityInfo", "sizeInformation"))

listOpenProjects(sirius)

openProject(sirius, projectId, path = character())

featuresId(sirius, type = c("sirius", "xcms"))

featuresInfo(sirius)

deleteFeatures(sirius, featureId = featuresId(sirius))

mapFeatures(sirius)

saveConfig(sirius, config, name)

jobInfo(sirius, jobId = character())

deleteJob(sirius, jobId = character(), all = FALSE)
```

## Arguments

- sirius:

  a `Sirius` object

- username:

  `character(1)`, the username to use for the connection

- password:

  `character(1)`, the password to use for the connection

- closeProject:

  `logical`, whether to also close the project when closing the GUI.
  Default is `FALSE`.

- infoType:

  `character` vector of length 1 or 2, specifying the type of
  information to retrieve. Possible values are `"compatibilityInfo"` and
  `"sizeInformation"`.

- projectId:

  `character(1)` specifying the id of the project to open. can be an
  already existing project or a new one.

- path:

  `character(1)` path where to find the existing project or where to
  create a new one.By default, the porject will be opened in the current
  `"."` directory.

- type:

  `character` vector of length 1, specifying the type of features ID to
  list. Possible values are `"sirius"` and `"xcms"`. Default is
  `"sirius"`.

- featureId:

  [`character()`](https://rdrr.io/r/base/character.html) vector
  specifying the id of the feature to remove. By default remove all.

- config:

  a `configSirius` object

- name:

  `character` vector of length 1, specifying the name of the
  configuration to load.

- jobId:

  `character(1)` specifying the id of the job to delete.

- all:

  `logical`, whether to delete all jobs. Default is `FALSE`.

## Value

A `message` whether the user is successfully logged in or not.

`logical`, `TRUE` if the connection is valid, `FALSE` otherwise.

Invisible `NULL`. Messages indicate shutdown status.

Invisible `TRUE` if successful.

Invisible `TRUE` if successful.

a `list` with the information requested.

a `character` vector with the open projects.

a `Sirius` object with the project opened.

a `character` vector with the features ID (empty if no features).

a `data.frame` with the features information.

A `Sirius` object with the features removed.

a `data.frame` with the features mapping.

nothing, will save the configuration locally.

a `character` vector with the job information.

nothing, will delete the job from Sirius.
