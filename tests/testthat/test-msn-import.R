# Tests for MS2-only and MSn-only import support
# Tests .groupMSnIndex, .createfeatures with msLevel >= 2, and import()
# auto-grouping for MSn-only data

# =============================================================================
# Unit tests for .groupMSnIndex
# =============================================================================

test_that(".groupMSnIndex groups MS2 scans by precursorMz in order", {
    skip_if_not_installed("Spectra")
    library(Spectra)

    # Two MS2 scans with different precursorMz = 2 groups
    sp <- Spectra(DataFrame(
        msLevel = c(2L, 2L, 2L),
        precursorMz = c(300.0, 400.0, 300.0),
        dataOrigin = c("file1", "file1", "file1"),
        mz = I(list(c(100), c(200), c(100))),
        intensity = I(list(c(999), c(500), c(999)))
    ))

    idx <- RuSirius:::.groupMSnIndex(sp)
    # 300, 400, 300 again = 3 groups (new group each time precursorMz changes)
    expect_equal(idx, c(1L, 2L, 3L))
})

test_that(".groupMSnIndex keeps MS3 with preceding MS2", {
    skip_if_not_installed("Spectra")
    library(Spectra)

    # MS2 at 300, then MS3 at 150, then new MS2 at 400
    sp <- Spectra(DataFrame(
        msLevel = c(2L, 3L, 2L),
        precursorMz = c(300.0, 150.0, 400.0),
        dataOrigin = c("file1", "file1", "file1"),
        mz = I(list(c(100), c(50), c(200))),
        intensity = I(list(c(999), c(100), c(500)))
    ))

    idx <- RuSirius:::.groupMSnIndex(sp)
    # MS3 should belong to same group as preceding MS2
    expect_equal(idx, c(1L, 1L, 2L))
})

test_that(".groupMSnIndex handles multiple dataOrigins independently", {
    skip_if_not_installed("Spectra")
    library(Spectra)

    sp <- Spectra(DataFrame(
        msLevel = c(2L, 2L, 2L, 2L),
        precursorMz = c(300.0, 400.0, 300.0, 400.0),
        dataOrigin = c("file1", "file1", "file2", "file2"),
        mz = I(list(c(100), c(200), c(100), c(200))),
        intensity = I(list(c(999), c(500), c(999), c(500)))
    ))

    idx <- RuSirius:::.groupMSnIndex(sp)
    # file1: group 1, 2; file2: group 3, 4
    expect_equal(idx, c(1L, 2L, 3L, 4L))
})

test_that(".groupMSnIndex groups consecutive same precursorMz MS2 together", {
    skip_if_not_installed("Spectra")
    library(Spectra)

    # Two consecutive MS2 with same precursorMz = same group
    sp <- Spectra(DataFrame(
        msLevel = c(2L, 2L, 2L),
        precursorMz = c(300.0, 300.0, 400.0),
        dataOrigin = c("file1", "file1", "file1"),
        mz = I(list(c(100), c(120), c(200))),
        intensity = I(list(c(999), c(800), c(500)))
    ))

    idx <- RuSirius:::.groupMSnIndex(sp)
    expect_equal(idx, c(1L, 1L, 2L))
})

# =============================================================================
# Unit tests for .createfeatures msLevel >= 2 support
# =============================================================================

test_that(".createfeatures includes MS2 spectra when no MS1 present", {
    skip_if_not_installed("RSirius")
    skip_if_not_installed("Spectra")
    library(Spectra)

    sp <- Spectra(DataFrame(
        msLevel = c(2L, 2L),
        polarity = c(1L, 1L),
        precursorMz = c(300.0, 300.0),
        scanIndex = c(1L, 2L),
        dataOrigin = c("file1", "file1"),
        mz = I(list(c(100, 150), c(120, 180))),
        intensity = I(list(c(999, 500), c(800, 200)))
    ))

    feat <- RuSirius:::.createfeatures(sp, idx = 1, adduct = "[M+H]+")

    expect_true(is.null(feat$mergedMs1))
    expect_length(feat$ms2Spectra, 2)
    expect_equal(feat$ms2Spectra[[1]]$msLevel, 2L)
    expect_equal(feat$ms2Spectra[[2]]$msLevel, 2L)
    ## ionMass must be derived from precursorMz when no MS1 is present
    expect_equal(feat$ionMass, 300.0)
})

test_that(".createfeatures sets ionMass to precursorMz without MS1", {
    skip_if_not_installed("RSirius")
    skip_if_not_installed("Spectra")
    library(Spectra)

    sp <- Spectra(DataFrame(
        msLevel = 2L,
        polarity = 1L,
        precursorMz = 456.123,
        scanIndex = 1L,
        dataOrigin = "file1",
        mz = I(list(c(100, 200))),
        intensity = I(list(c(999, 500)))
    ))

    feat <- RuSirius:::.createfeatures(sp, idx = 1, adduct = "[M+H]+")
    expect_equal(feat$ionMass, 456.123)
    expect_true(is.null(feat$mergedMs1))
})

test_that(".createfeatures sets ionMass from precursorMz with MS1+MS2", {
    skip_if_not_installed("RSirius")
    skip_if_not_installed("Spectra")
    library(Spectra)

    sp <- Spectra(DataFrame(
        msLevel = c(1L, 2L),
        polarity = c(1L, 1L),
        precursorMz = c(NA_real_, 300.0),
        scanIndex = c(1L, 2L),
        dataOrigin = c("file1", "file1"),
        mz = I(list(c(300), c(100, 150))),
        intensity = I(list(c(9999), c(999, 500)))
    ))

    feat <- RuSirius:::.createfeatures(sp, idx = 1, adduct = "[M+H]+")
    ## ionMass should always come from precursorMz when MSn data exists
    expect_equal(feat$ionMass, 300.0)
    expect_false(is.null(feat$mergedMs1))
})

test_that(".createfeatures sets ionMass to 0 for MS1-only data", {
    skip_if_not_installed("RSirius")
    skip_if_not_installed("Spectra")
    library(Spectra)

    sp <- Spectra(DataFrame(
        msLevel = 1L,
        polarity = 1L,
        precursorMz = NA_real_,
        scanIndex = 1L,
        dataOrigin = "file1",
        mz = I(list(c(300, 301))),
        intensity = I(list(c(9999, 500)))
    ))

    feat <- RuSirius:::.createfeatures(sp, idx = 1, adduct = "[M+H]+")
    ## No MSn data: ionMass = 0, Sirius derives it from the MS1 spectrum
    expect_equal(feat$ionMass, 0)
    expect_false(is.null(feat$mergedMs1))
    expect_null(feat$ms2Spectra)
})

test_that(".createfeatures includes MS3 spectra alongside MS2", {
    skip_if_not_installed("RSirius")
    skip_if_not_installed("Spectra")
    library(Spectra)

    sp <- Spectra(DataFrame(
        msLevel = c(2L, 3L),
        polarity = c(1L, 1L),
        precursorMz = c(300.0, 150.0),
        scanIndex = c(1L, 2L),
        dataOrigin = c("file1", "file1"),
        mz = I(list(c(100, 150), c(50, 75))),
        intensity = I(list(c(999, 500), c(100, 80)))
    ))

    feat <- RuSirius:::.createfeatures(sp, idx = 1, adduct = "[M+H]+")

    expect_true(is.null(feat$mergedMs1))
    expect_length(feat$ms2Spectra, 2)
    expect_equal(feat$ms2Spectra[[1]]$msLevel, 2L)
    expect_equal(feat$ms2Spectra[[2]]$msLevel, 3L)
})

# =============================================================================
# Unit tests for import() auto-grouping of MSn-only spectra
# =============================================================================

test_that("import auto-groups MSn-only spectra by precursorMz", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_not_logged_in(srs)

    library(Spectra)

    # Create MS2 + MS3 spectra without MS1
    sp <- Spectra(DataFrame(
        msLevel = c(2L, 3L),
        polarity = c(1L, 1L),
        precursorMz = c(300.0, 150.0),
        scanIndex = c(1L, 2L),
        dataOrigin = c("test_origin", "test_origin"),
        mz = I(list(c(100, 150), c(50, 75))),
        intensity = I(list(c(999, 500), c(100, 80)))
    ))

    srs <- expect_message(
        import(srs, sp, adducts = "[M+H]+", deleteExistingFeatures = TRUE),
        "No MS1 data found"
    )

    info <- featuresInfo(srs)
    expect_true(is.matrix(info) || is.data.frame(info))
    expect_true(nrow(info) >= 1)
    # All features should have hasMs1 = FALSE
    expect_true(all(info[, "hasMs1"] == FALSE))
    expect_true(all(info[, "hasMsMs"] == TRUE))
    ## ionMass must match precursorMz of the MS2 spectrum
    expect_equal(as.numeric(info[1, "ionMass"]), 300.0)
})

test_that("import works with MS2-only spectra (single MS level)", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_not_logged_in(srs)

    library(Spectra)

    # Create MS2-only spectra (single MS level, no ms_column_name needed)
    sp <- Spectra(DataFrame(
        msLevel = c(2L, 2L),
        polarity = c(1L, 1L),
        precursorMz = c(300.0, 400.0),
        scanIndex = c(1L, 2L),
        dataOrigin = c("test_origin", "test_origin"),
        mz = I(list(c(100, 150), c(200, 250))),
        intensity = I(list(c(999, 500), c(300, 100)))
    ))

    srs <- import(srs, sp, adducts = "[M+H]+", deleteExistingFeatures = TRUE)

    info <- featuresInfo(srs)
    expect_true(is.matrix(info) || is.data.frame(info))
    expect_equal(nrow(info), 2)
    expect_true(all(info[, "hasMs1"] == FALSE))
    expect_true(all(info[, "hasMsMs"] == TRUE))
    ## ionMass must match the precursorMz of each feature
    ion_masses <- sort(as.numeric(info[, "ionMass"]))
    expect_equal(ion_masses, c(300.0, 400.0))
})

test_that("import single MS2 spectrum sets correct ionMass", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_not_logged_in(srs)

    library(Spectra)

    ## Minimal repro of the reported bug: a single MS2 spectrum, no MS1
    sp <- Spectra(DataFrame(
        msLevel     = 2L,
        polarity    = 1L,
        precursorMz = 207.24,
        scanIndex   = 45L,
        rtime       = 59.71,
        dataOrigin  = "user_file",
        mz          = I(list(c(51.81, 52.73, 200.51, 207.24))),
        intensity   = I(list(c(3991, 3641, 7690, 6591)))
    ))

    srs <- import(srs, sp, adducts = "[M+H]+", deleteExistingFeatures = TRUE)

    info <- featuresInfo(srs)
    expect_true(is.matrix(info) || is.data.frame(info))
    expect_equal(nrow(info), 1)
    expect_true(info[1, "hasMsMs"] == TRUE)
    expect_true(info[1, "hasMs1"] == FALSE)
    expect_equal(as.numeric(info[1, "ionMass"]), 207.24)
})

test_that("import still requires ms_column_name with MS1+MS2 data", {
    skip_if_not_installed("Spectra")
    library(Spectra)

    # Create MS1 + MS2 spectra
    sp <- Spectra(DataFrame(
        msLevel = c(1L, 2L),
        polarity = c(1L, 1L),
        precursorMz = c(NA_real_, 300.0),
        scanIndex = c(1L, 2L),
        dataOrigin = c("test_origin", "test_origin"),
        mz = I(list(c(300), c(100, 150))),
        intensity = I(list(c(9999), c(999, 500)))
    ))

    # Should still error when MS1 is present and no ms_column_name
    expect_error(
        import(new("Sirius"), sp, deleteExistingFeatures = FALSE),
        "column to group"
    )
})

# =============================================================================
# API-level tests for MS2-only and MS2+MS3 import
# =============================================================================

test_that("API accepts MS2-only feature (no MS1)", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_not_logged_in(srs)

    ms2_spectrum <- RSirius::BasicSpectrum$new(
        msLevel = 2L,
        scanNumber = 1L,
        precursorMz = 300.0,
        peaks = list(
            RSirius::SimplePeak$new(mz = 100.0, intensity = 999),
            RSirius::SimplePeak$new(mz = 150.0, intensity = 500)
        )
    )

    feature <- RSirius::FeatureImport$new(
        externalFeatureId = "api_test_ms2_only",
        ionMass = 300.0,
        charge = 1L,
        detectedAdducts = list("[M+H]+"),
        mergedMs1 = NULL,
        ms2Spectra = list(ms2_spectrum)
    )

    result <- tryCatch(
        srs@api$features_api$AddAlignedFeatures(
            project_id = srs@projectId,
            list(feature)
        ),
        error = function(e) e
    )

    expect_false(inherits(result, "error"))
    expect_true(length(result) == 1)
    expect_false(result[[1]]$hasMs1)
    expect_true(result[[1]]$hasMsMs)
})

test_that("API accepts MS2+MS3 feature (no MS1)", {
    skip_if_no_sirius()

    srs <- test_sirius_connection()
    if (is.null(srs)) skip("Could not create Sirius connection")
    skip_if_not_logged_in(srs)

    ms2_spectrum <- RSirius::BasicSpectrum$new(
        msLevel = 2L,
        scanNumber = 1L,
        precursorMz = 300.0,
        peaks = list(
            RSirius::SimplePeak$new(mz = 100.0, intensity = 999),
            RSirius::SimplePeak$new(mz = 150.0, intensity = 500)
        )
    )

    ms3_spectrum <- RSirius::BasicSpectrum$new(
        msLevel = 3L,
        scanNumber = 2L,
        precursorMz = 150.0,
        peaks = list(
            RSirius::SimplePeak$new(mz = 50.0, intensity = 100),
            RSirius::SimplePeak$new(mz = 75.0, intensity = 80)
        )
    )

    feature <- RSirius::FeatureImport$new(
        externalFeatureId = "api_test_ms2_ms3",
        ionMass = 300.0,
        charge = 1L,
        detectedAdducts = list("[M+H]+"),
        mergedMs1 = NULL,
        ms2Spectra = list(ms2_spectrum, ms3_spectrum)
    )

    result <- tryCatch(
        srs@api$features_api$AddAlignedFeatures(
            project_id = srs@projectId,
            list(feature)
        ),
        error = function(e) e
    )

    expect_false(inherits(result, "error"))
    expect_true(length(result) == 1)
    expect_false(result[[1]]$hasMs1)
    expect_true(result[[1]]$hasMsMs)
})
