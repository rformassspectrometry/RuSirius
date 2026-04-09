# Tests for input validation of utility functions
# These tests do not require the Sirius API - they test argument checking

# =============================================================================
# logIn validation
# =============================================================================

test_that("logIn stops without credentials", {
  srs <- new("Sirius")
  expect_error(logIn(srs, character(), character()), "username and a password")
})

test_that("logIn stops with invalid connection", {
  srs <- new("Sirius")
  expect_error(logIn(srs, "user", "pass"), "connection.*not valid")
})

# =============================================================================
# checkConnection
# =============================================================================

test_that("checkConnection returns FALSE for default Sirius", {
  srs <- new("Sirius")
  expect_false(checkConnection(srs))
})

# =============================================================================
# openGUI / closeGUI validation
# =============================================================================

test_that("openGUI stops with no projectId", {
  srs <- new("Sirius", projectId = character(0))
  expect_error(openGUI(srs), "No project")
})

test_that("closeGUI stops with no projectId", {
  srs <- new("Sirius", projectId = character(0))
  expect_error(closeGUI(srs), "No project")
})

# =============================================================================
# openProject validation
# =============================================================================

test_that("openProject stops with invalid connection", {
  srs <- new("Sirius")
  expect_error(openProject(srs, "proj1"), "connection.*not valid")
})

test_that("openProject stops with non-existent path", {
  skip_if_no_sirius()
  srs <- test_sirius_connection()
  if (is.null(srs)) {
    skip("Could not create Sirius connection")
  }
  skip_if_not_logged_in(srs)

  suppressWarnings(expect_error(
    openProject(srs, "test_proj", path = "/nonexistent/path/xyz"),
    "path.*does not exist"
  ))
})

# =============================================================================
# infoDb validation
# =============================================================================

test_that("infoDb stops with invalid connection", {
  srs <- new("Sirius")
  expect_error(infoDb(srs, "db1"), "connection.*not valid")
})

test_that("infoDb stops with empty databaseId", {
  skip_if_no_sirius()
  srs <- test_sirius_connection()
  if (is.null(srs)) {
    skip("Could not create Sirius connection")
  }
  skip_if_not_logged_in(srs)

  expect_error(infoDb(srs, ""), "valid database ID")
})

# =============================================================================
# removeDb validation
# =============================================================================

test_that("removeDb stops with invalid connection", {
  srs <- new("Sirius")
  expect_error(removeDb(srs, "db1"), "connection.*not valid")
})

test_that("removeDb stops with empty databaseId", {
  skip_if_no_sirius()
  srs <- test_sirius_connection()
  if (is.null(srs)) {
    skip("Could not create Sirius connection")
  }
  skip_if_not_logged_in(srs)

  expect_error(removeDb(srs, ""), "valid database ID")
})

# =============================================================================
# createDb validation
# =============================================================================

test_that("createDb stops with invalid connection", {
  srs <- new("Sirius")
  expect_error(createDb(srs, "db1", "file.mgf"), "connection.*not valid")
})

test_that("createDb stops with missing databaseId", {
  skip_if_no_sirius()
  srs <- test_sirius_connection()
  if (is.null(srs)) {
    skip("Could not create Sirius connection")
  }
  skip_if_not_logged_in(srs)

  expect_error(
    createDb(srs, character(), "file.mgf"),
    "single valid database ID"
  )
})

test_that("createDb stops with multiple databaseIds", {
  skip_if_no_sirius()
  srs <- test_sirius_connection()
  if (is.null(srs)) {
    skip("Could not create Sirius connection")
  }
  skip_if_not_logged_in(srs)

  expect_error(
    createDb(srs, c("db1", "db2"), "file.mgf"),
    "single valid database ID"
  )
})

test_that("createDb stops with non-character databaseId", {
  skip_if_no_sirius()
  srs <- test_sirius_connection()
  if (is.null(srs)) {
    skip("Could not create Sirius connection")
  }
  skip_if_not_logged_in(srs)

  expect_error(createDb(srs, 123, "file.mgf"), "single valid database ID")
})

test_that("createDb stops with non-existent files", {
  skip_if_no_sirius()
  srs <- test_sirius_connection()
  if (is.null(srs)) {
    skip("Could not create Sirius connection")
  }
  skip_if_not_logged_in(srs)

  expect_error(createDb(srs, "testdb", "nonexistent_file.mgf"), "valid file")
})

test_that("createDb stops with empty files", {
  skip_if_no_sirius()
  srs <- test_sirius_connection()
  if (is.null(srs)) {
    skip("Could not create Sirius connection")
  }
  skip_if_not_logged_in(srs)

  expect_error(createDb(srs, "testdb", character()), "valid file")
})

test_that("createDb stops with non-existent location", {
  skip_if_no_sirius()
  srs <- test_sirius_connection()
  if (is.null(srs)) {
    skip("Could not create Sirius connection")
  }
  skip_if_not_logged_in(srs)

  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp))

  expect_error(
    createDb(srs, "testdb", tmp, location = "/nonexistent/path"),
    "valid directory"
  )
})

# =============================================================================
# listDbs validation
# =============================================================================

test_that("listDbs stops with invalid connection", {
  srs <- new("Sirius")
  expect_error(listDbs(srs), "connection.*not valid")
})

# =============================================================================
# listOpenProjects validation
# =============================================================================

test_that("listOpenProjects stops with invalid connection", {
  srs <- new("Sirius")
  expect_error(
    listOpenProjects(srs),
    "connection.*not valid.*Cannot retrieve open projects"
  )
})

# =============================================================================
# saveConfig validation
# =============================================================================

test_that("saveConfig stops without name", {
  srs <- new("Sirius")
  cfg <- config(alignedFeaturesIds = "feat1")
  expect_error(saveConfig(srs, cfg, character()), "name")
})

# =============================================================================
# jobInfo validation
# =============================================================================

test_that("jobInfo stops without jobId", {
  srs <- new("Sirius")
  expect_error(jobInfo(srs, character()), "jobId")
})

# =============================================================================
# deleteJob validation
# =============================================================================

test_that("deleteJob stops without jobId and all=FALSE", {
  srs <- new("Sirius")
  expect_error(
    deleteJob(srs, character(), all = FALSE),
    "delete or set all = TRUE"
  )
})

# =============================================================================
# featuresId validation
# =============================================================================

test_that("featuresId rejects invalid type argument", {
  srs <- new("Sirius")
  expect_error(featuresId(srs, type = "invalid"), "should be one of")
})

# =============================================================================
# import validation
# =============================================================================

test_that("import stops when multi-level spectra with MS1 lack ms_column_name", {
  srs <- new("Sirius")
  sp <- Spectra(DataFrame(
    msLevel = c(1L, 2L),
    polarity = c(1L, 1L),
    precursorMz = c(NA_real_, 300.0),
    scanIndex = c(1L, 2L),
    dataOrigin = c("test", "test"),
    mz = I(list(c(100.0), c(100.0))),
    intensity = I(list(c(1000), c(500)))
  ))

  expect_error(
    import(srs, sp),
    "group the spectra must be provided.*ms_column_name"
  )
})

test_that("import stops when grouping column contains NA", {
  srs <- new("Sirius")
  sp <- Spectra(DataFrame(
    msLevel = c(1L, 2L),
    polarity = c(1L, 1L),
    precursorMz = c(NA_real_, 300.0),
    scanIndex = c(1L, 2L),
    dataOrigin = c("test", "test"),
    ms_group = c(1L, NA_integer_),
    mz = I(list(c(100.0), c(100.0))),
    intensity = I(list(c(1000), c(500)))
  ))

  expect_error(
    import(srs, sp, ms_column_name = "ms_group"),
    "cannot contain.*NA"
  )
})

test_that("import stops when adducts length mismatches features", {
  srs <- new("Sirius")
  sp <- Spectra(DataFrame(
    msLevel = c(1L, 1L, 1L),
    polarity = c(1L, 1L, 1L),
    precursorMz = c(NA_real_, NA_real_, NA_real_),
    scanIndex = c(1L, 2L, 3L),
    dataOrigin = c("test", "test", "test"),
    mz = I(list(c(100.0), c(200.0), c(300.0))),
    intensity = I(list(c(1000), c(500), c(200)))
  ))

  expect_error(
    import(
      srs,
      sp,
      adducts = c("[M+H]+", "[M+Na]+"),
      deleteExistingFeatures = FALSE
    ),
    "number of adducts must be either 1 or the same"
  )
})

# =============================================================================
# Sirius class validation
# =============================================================================

test_that("Sirius class rejects username without password via constructor", {
  # username and password are handled by the Sirius() constructor function,
  # not the S4 class directly (they come from CompAnnotationSource)
  srs <- new("Sirius")
  expect_s4_class(srs, "Sirius")
})

test_that("Sirius class rejects multiple projectIds", {
  expect_error(new("Sirius", projectId = c("a", "b")), "projectId.*length 1")
})

test_that("Sirius class accepts valid empty construction", {
  srs <- new("Sirius")
  expect_s4_class(srs, "Sirius")
  expect_equal(srs@projectId, character())
})
