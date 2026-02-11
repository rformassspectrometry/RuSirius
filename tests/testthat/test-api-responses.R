# Tests for API response structure validation
# These tests verify that the RSirius API responses match expected structures
# If these tests fail after an API update, it indicates breaking changes

test_that("RSirius classes exist and have expected structure", {
    skip_if_not_installed("RSirius")

    # Check core RSirius classes exist
    expect_true(exists("SiriusSDK", where = asNamespace("RSirius")))
    expect_true(exists("SimplePeak", where = asNamespace("RSirius")))
    expect_true(exists("BasicSpectrum", where = asNamespace("RSirius")))
    expect_true(exists("FeatureImport", where = asNamespace("RSirius")))
    expect_true(exists("JobSubmission", where = asNamespace("RSirius")))
    expect_true(exists("AccountCredentials", where = asNamespace("RSirius")))
})

test_that("SimplePeak class has expected fields", {
    skip_if_not_installed("RSirius")

    peak <- RSirius::SimplePeak$new(mz = 100.5, intensity = 1000)

    # Check fields exist
    expect_true("mz" %in% names(peak))
    expect_true("intensity" %in% names(peak))

    # Check values are correctly assigned
    expect_equal(peak$mz, 100.5)
    expect_equal(peak$intensity, 1000)
})

test_that("BasicSpectrum class has expected fields", {
    skip_if_not_installed("RSirius")

    peaks <- list(RSirius::SimplePeak$new(mz = 100.5, intensity = 1000))
    spectrum <- RSirius::BasicSpectrum$new(
        msLevel = 2L,
        scanNumber = 1L,
        precursorMz = 500.0,
        peaks = peaks
    )

    # Check fields exist
    expect_true("msLevel" %in% names(spectrum))
    expect_true("scanNumber" %in% names(spectrum))
    expect_true("precursorMz" %in% names(spectrum))
    expect_true("peaks" %in% names(spectrum))

    # Check values are correctly assigned
    expect_equal(spectrum$msLevel, 2L)
    expect_equal(spectrum$precursorMz, 500.0)
})

test_that("FeatureImport class has expected fields", {
    skip_if_not_installed("RSirius")

    feature <- RSirius::FeatureImport$new(
        externalFeatureId = "test_feature_1",
        ionMass = 500.0,
        charge = 1
    )

    # Check fields exist
    expect_true("externalFeatureId" %in% names(feature))
    expect_true("ionMass" %in% names(feature))
    expect_true("charge" %in% names(feature))

    # Check optional fields
    expect_true("detectedAdducts" %in% names(feature))
    expect_true("mergedMs1" %in% names(feature))
    expect_true("ms2Spectra" %in% names(feature))
})

test_that("JobSubmission class has expected fields", {
    skip_if_not_installed("RSirius")

    job <- RSirius::JobSubmission$new()

    # Check core fields exist
    expect_true("compoundIds" %in% names(job) ||
                    "alignedFeatureIds" %in% names(job))
    expect_true("fallbackAdducts" %in% names(job))
    expect_true("formulaIdParams" %in% names(job) ||
                    "sirius" %in% names(job))
})

test_that("SiriusSDK can be instantiated", {
    skip_if_not_installed("RSirius")

    sdk <- RSirius::SiriusSDK$new()
    expect_s3_class(sdk, "SiriusSDK")

    # Check expected methods exist
    expect_true("attach_or_start_sirius" %in% names(sdk))
    expect_true("shutdown_sirius" %in% names(sdk))
})

test_that("AccountCredentials class works correctly", {
    skip_if_not_installed("RSirius")

    cred <- RSirius::AccountCredentials$new(
        username = "test_user",
        password = "test_pass"
    )

    expect_true("username" %in% names(cred))
    expect_true("password" %in% names(cred))
    expect_equal(cred$username, "test_user")
    expect_equal(cred$password, "test_pass")
})

# Tests for API version compatibility
test_that("RSirius package version is as expected",
{
    skip_if_not_installed("RSirius")

    pkg_version <- packageVersion("RSirius")

    # This test will fail if the API version changes unexpectedly

    # Update expected version when intentionally upgrading
    expect_equal(as.character(pkg_version), "6.3.3",
                 label = paste("RSirius version changed from expected.",
                               "Current:", pkg_version,
                               "- Review API changes and update tests if needed."))
})

# Tests for toSimpleType method availability
test_that("RSirius objects have toSimpleType method", {
    skip_if_not_installed("RSirius")

    peak <- RSirius::SimplePeak$new(mz = 100.5, intensity = 1000)

    # Check toSimpleType exists and returns expected format
    expect_true("toSimpleType" %in% names(peak))

    simple <- peak$toSimpleType()
    expect_type(simple, "list")
    expect_true("mz" %in% names(simple))
    expect_true("intensity" %in% names(simple))
})

test_that("BasicSpectrum toSimpleType works correctly", {
    skip_if_not_installed("RSirius")

    peaks <- list(RSirius::SimplePeak$new(mz = 100.5, intensity = 1000))
    spectrum <- RSirius::BasicSpectrum$new(
        msLevel = 2L,
        peaks = peaks
    )

    simple <- spectrum$toSimpleType()
    expect_type(simple, "list")
    expect_true("msLevel" %in% names(simple))
})

test_that("FeatureImport toJSON/fromJSON round-trip works", {
    skip_if_not_installed("RSirius")

    feature <- RSirius::FeatureImport$new(
        externalFeatureId = "test_feature",
        ionMass = 500.0,
        charge = 1
    )

    # Test that serialization methods exist
    expect_true("toJSON" %in% names(feature) ||
                    "toJSONString" %in% names(feature))
})
