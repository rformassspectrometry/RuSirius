# Integration tests that require a running Sirius instance
# These tests verify the core API functionality works correctly
# Skip when Sirius is not available

# =============================================================================
# Connection Tests
# =============================================================================

test_that("Sirius connection can be established", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")

    expect_s4_class(srs, "Sirius")
    expect_true(checkConnection(srs))
})

test_that("checkConnection returns FALSE for invalid object", {
    invalid_srs <- new("Sirius")
    expect_false(checkConnection(invalid_srs))
})

# =============================================================================
# show() method
# =============================================================================

test_that("show method prints Sirius object info", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")

    output <- capture.output(show(srs))
    expect_true(any(grepl("Sirius object", output)))
    expect_true(any(grepl("Valid connection", output)))
})

test_that("show method handles disconnected object", {
    srs <- new("Sirius")
    output <- capture.output(show(srs))
    expect_true(any(grepl("Sirius object", output)))
    expect_true(any(grepl("FALSE", output)))
})

# =============================================================================
# Project Management Tests
# =============================================================================

test_that("listOpenProjects returns character vector", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")

    projects <- suppressMessages(listOpenProjects(srs))
    expect_type(projects, "character")
})

test_that("projectInfo returns expected structure", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_no_project(srs)

    info <- projectInfo(srs)

    expect_type(info, "list")
    expect_true(any(c("projectId", "location", "numOfFeatures") %in% names(info)))
})

# =============================================================================
# Features API Tests
# =============================================================================

test_that("featuresId returns character vector (empty or populated)", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_no_project(srs)

    ids <- featuresId(srs)
    expect_type(ids, "character")
})

test_that("featuresId type argument works", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_no_project(srs)

    ids_sirius <- featuresId(srs, type = "sirius")
    ids_xcms <- featuresId(srs, type = "xcms")

    expect_type(ids_sirius, "character")
    expect_type(ids_xcms, "character")
    expect_equal(length(ids_sirius), length(ids_xcms))
})

test_that("featuresInfo returns data structure", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_no_project(srs)

    info <- featuresInfo(srs)

    # Should be NULL or matrix/data.frame
    expect_true(is.null(info) || is.matrix(info) || is.data.frame(info))
})

test_that("mapFeatures returns data.frame with expected columns", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_no_project(srs)

    fm <- mapFeatures(srs)

    expect_s3_class(fm, "data.frame")
    expect_true("fts_sirius" %in% colnames(fm))
    expect_true("fts_xcms" %in% colnames(fm))
})

# =============================================================================
# GUI Tests
# =============================================================================

test_that("openGUI fails without project", {
    skip_if_no_sirius()

    # Create minimal Sirius object without projectId
    minimal_srs <- new("Sirius", projectId = character(0))

    expect_error(openGUI(minimal_srs), "No project")
})

# =============================================================================
# Database Tests
# =============================================================================

test_that("listDbs returns data.frame with database info", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_not_logged_in(srs)

    dbs <- listDbs(srs)

    expect_s3_class(dbs, "data.frame")
    expect_true(nrow(dbs) > 0)
})

test_that("infoDb returns info for BIO database", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_not_logged_in(srs)

    info <- infoDb(srs, "BIO")

    expect_type(info, "list")
    expect_true("databaseId" %in% names(info))
})

# =============================================================================
# Import + Features Workflow
# =============================================================================

test_that("import MS1+MS2 with ms_column_name works", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_no_project(srs)
    skip_if_not_logged_in(srs)

    sp <- Spectra(DataFrame(
        msLevel = c(1L, 2L),
        polarity = c(1L, 1L),
        precursorMz = c(NA_real_, 300.0),
        scanIndex = c(1L, 2L),
        dataOrigin = c("test_origin", "test_origin"),
        ms_group = c(1L, 1L),
        mz = I(list(c(300.0, 301.0), c(100, 150))),
        intensity = I(list(c(9999, 500), c(999, 500)))
    ))

    srs <- import(srs, sp, ms_column_name = "ms_group",
                  adducts = "[M+H]+", deleteExistingFeatures = TRUE)

    # Update shared object so subsequent tests have the fresh featureMap
    .shared_sirius_env$srs <- srs

    ids <- featuresId(srs)
    expect_type(ids, "character")
    expect_true(length(ids) >= 1)

    info <- featuresInfo(srs)
    expect_true(is.matrix(info) || is.data.frame(info))
})

# =============================================================================
# Config/Parameter Tests (don't require running Sirius)
# =============================================================================

test_that("config creates valid config object", {
    cfg <- config(
        alignedFeaturesIds = c("feature1", "feature2"),
        formulaIdParams = formulaIdParam(),
        predictParams = predictParam()
    )

    expect_s4_class(cfg, "config")
    expect_equal(cfg@alignedFeaturesIds, c("feature1", "feature2"))
})

test_that("formulaIdParam creates valid S4 parameters", {
    params <- formulaIdParam(
        numberOfCandidates = 5,
        instrument = "QTOF",
        massAccuracyMS2ppm = 15
    )

    expect_s4_class(params, "formulaIdParam")
    expect_equal(params@numberOfCandidates, 5)
    expect_equal(params@instrument, "QTOF")
    expect_equal(params@massAccuracyMS2ppm, 15)
})

test_that("predictParam creates valid S4 parameters", {
    params <- predictParam()

    expect_s4_class(params, "predictParam")
})

test_that("structureDbSearchParam creates valid S4 parameters", {
    params <- structureDbSearchParam(
        structureSearchDbs = c("BIO", "PUBCHEM")
    )

    expect_s4_class(params, "structureDbSearchParam")
    expect_equal(params@structureSearchDbs, c("BIO", "PUBCHEM"))
})

test_that("zodiacParam creates valid S4 parameters", {
    params <- zodiacParam(
        consideredCandidatesAt300Mz = 15,
        consideredCandidatesAt800Mz = 60
    )

    expect_s4_class(params, "zodiacParam")
    expect_equal(params@consideredCandidatesAt300Mz, 15)
    expect_equal(params@consideredCandidatesAt800Mz, 60)
})

test_that("deNovoStructureParam creates valid S4 parameters", {
    params <- deNovoStructureParam(
        numberOfCandidateToPredict = 10
    )

    expect_s4_class(params, "deNovoStructureParam")
    expect_equal(params@numberOfCandidateToPredict, 10)
})

test_that("spectraMatchingParam creates valid S4 parameters", {
    params <- spectraMatchingParam(
        peakDeviationPpm = 15,
        precursorDeviationPpm = 20
    )

    expect_s4_class(params, "spectraMatchingParam")
    expect_equal(params@peakDeviationPpm, 15)
    expect_equal(params@precursorDeviationPpm, 20)
})

# =============================================================================
# results() function
# =============================================================================

test_that("run computes formula identification", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_no_project(srs)
    skip_if_not_logged_in(srs)

    ids <- featuresId(srs)
    if (length(ids) == 0) skip("No features to analyze")

    jobId <- run(srs,
                 alignedFeaturesIds = ids[1],
                 formulaIdParams = formulaIdParam())

    expect_type(jobId, "character")
})

test_that("results returns list by default", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_no_project(srs)
    skip_if_not_logged_in(srs)

    ids <- featuresId(srs)
    if (length(ids) == 0) skip("No features available for results")

    res <- results(srs, features = ids[1], result.type = "formulaId",
                   return.type = "list")

    expect_type(res, "list")
    expect_true(length(res) >= 1)
})

test_that("results returns data.frame when requested", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_no_project(srs)
    skip_if_not_logged_in(srs)

    ids <- featuresId(srs)
    if (length(ids) == 0) skip("No features available for results")

    res <- results(srs, features = ids[1], result.type = "formulaId",
                   return.type = "data.frame")

    expect_true(is.data.frame(res))
})

test_that("results spectralDbMatch works", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_no_project(srs)
    skip_if_not_logged_in(srs)

    ids <- featuresId(srs)
    if (length(ids) == 0) skip("No features available for results")

    res <- results(srs, features = ids[1], result.type = "spectralDbMatch",
                   return.type = "list")

    expect_type(res, "list")
})

# =============================================================================
# summary() method
# =============================================================================

test_that("summary returns empty data.frame when no features exist", {
    skip_if_no_sirius()

    srs <- test_sirius_connection("test_summary_empty")
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_not_logged_in(srs)

    ## Ensure empty project
    srs <- deleteFeatures(srs)

    smry <- getMethod("summary", "Sirius")
    res <- smry(srs, result.type = "formulaId")
    expect_true(is.data.frame(res))
    expect_equal(nrow(res), 0)

    res <- smry(srs, result.type = "structure")
    expect_true(is.data.frame(res))
    expect_equal(nrow(res), 0)
})

test_that("summary returns data.frame with results after computation", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_no_project(srs)
    skip_if_not_logged_in(srs)

    ids <- featuresId(srs)
    if (length(ids) == 0) skip("No features available")

    smry <- getMethod("summary", "Sirius")
    res <- smry(srs, result.type = "formulaId")
    expect_true(is.data.frame(res))
    expect_true(nrow(res) >= 1)

    res <- smry(srs, result.type = "structure")
    expect_true(is.data.frame(res))
    expect_true(nrow(res) >= 1)
})

# =============================================================================
# API Response Structure Tests
# =============================================================================

test_that("Info API returns expected fields", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")

    info <- srs@api$info_api$GetInfo()

    # Check essential fields exist
    expect_true(!is.null(info$siriusVersion))
})

test_that("Account API returns login status", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")

    logged_in <- srs@api$login_and_account_api$IsLoggedIn()

    expect_type(logged_in, "logical")
})

test_that("IsLoggedIn returns TRUE when user is logged in", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_not_logged_in(srs)

    expect_true(srs@api$login_and_account_api$IsLoggedIn())
})

# =============================================================================
# Shutdown
# =============================================================================

test_that("shutdown handles disconnected object gracefully", {
    srs <- new("Sirius")
    expect_message(
        shutdown(srs),
        "Could not shutdown"
    )
})
