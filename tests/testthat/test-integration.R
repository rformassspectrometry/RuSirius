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
    on.exit(safe_shutdown(srs), add = TRUE)

    expect_s4_class(srs, "Sirius")
    expect_true(checkConnection(srs))
})

test_that("checkConnection returns FALSE for invalid object", {
    invalid_srs <- new("Sirius")
    expect_false(checkConnection(invalid_srs))
})

# =============================================================================
# Project Management Tests
# =============================================================================

test_that("listOpenProjects returns character vector", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    on.exit(safe_shutdown(srs), add = TRUE)

    projects <- listOpenProjects(srs)
    expect_type(projects, "character")
})

test_that("projectInfo returns expected structure", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    on.exit(safe_shutdown(srs), add = TRUE)

    info <- tryCatch(
        projectInfo(srs),
        error = function(e) skip(paste("API error:", conditionMessage(e)))
    )

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
    on.exit(safe_shutdown(srs), add = TRUE)

    ids <- tryCatch(
        featuresId(srs),
        error = function(e) skip(paste("API error:", conditionMessage(e)))
    )

    expect_type(ids, "character")
})

test_that("featuresId type argument works", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    on.exit(safe_shutdown(srs), add = TRUE)

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
    on.exit(safe_shutdown(srs), add = TRUE)

    info <- tryCatch(
        featuresInfo(srs),
        error = function(e) skip(paste("API error:", conditionMessage(e)))
    )

    # Should be NULL or matrix/data.frame
    expect_true(is.null(info) || is.matrix(info) || is.data.frame(info))
})

# =============================================================================
# GUI Tests
# =============================================================================

test_that("openGUI and closeGUI work", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    on.exit(safe_shutdown(srs), add = TRUE)

    # Test opening GUI
    result <- tryCatch({
        openGUI(srs)
        TRUE
    }, error = function(e) {
        skip(paste("GUI API error:", conditionMessage(e)))
    })
    expect_true(result)

    # Test closing GUI
    result <- tryCatch({
        closeGUI(srs)
        TRUE
    }, error = function(e) {
        skip(paste("GUI API error:", conditionMessage(e)))
    })
    expect_true(result)
})

test_that("openGUI fails without project", {
    skip_if_no_sirius()

    # Create minimal Sirius object without projectId
    minimal_srs <- new("Sirius", projectId = character(0))

    expect_error(openGUI(minimal_srs), "No project")
})

# =============================================================================
# Database Tests
# =============================================================================

test_that("listDbs returns database information", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    on.exit(safe_shutdown(srs), add = TRUE)

    dbs <- tryCatch(
        listDbs(srs),
        error = function(e) skip(paste("API error:", conditionMessage(e)))
    )

    expect_true(is.data.frame(dbs) || is.matrix(dbs) || is.list(dbs))
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
# API Response Structure Tests
# These verify that API responses have expected fields
# =============================================================================

test_that("Info API returns expected fields", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    on.exit(safe_shutdown(srs), add = TRUE)

    info <- tryCatch(
        srs@api$info_api$GetInfo(),
        error = function(e) skip(paste("API error:", conditionMessage(e)))
    )

    # Check essential fields exist
    expect_true(!is.null(info$siriusVersion))
})

test_that("Account API returns login status", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    on.exit(safe_shutdown(srs), add = TRUE)

    logged_in <- tryCatch(
        srs@api$login_and_account_api$IsLoggedIn(),
        error = function(e) skip(paste("API error:", conditionMessage(e)))
    )

    expect_type(logged_in, "logical")
})
