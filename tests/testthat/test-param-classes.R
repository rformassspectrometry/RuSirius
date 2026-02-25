# Tests for parameter classes
# These tests do not require the Sirius API

test_that("formulaIdParam creates valid object with defaults", {
    param <- formulaIdParam()
    expect_s4_class(param, "formulaIdParam")
    expect_equal(param@instrument, "QTOF")
    expect_equal(param@numberOfCandidates, 10)
    expect_equal(param@massAccuracyMS2ppm, 10)
    expect_true(param@filterByIsotopePattern)
    expect_true(param@enforceElGordoFormula)
    expect_equal(param@candidateFormulas, character(0))
})

test_that("formulaIdParam accepts custom values", {
    param <- formulaIdParam(
        instrument = "ORBITRAP",
        numberOfCandidates = 5,
        massAccuracyMS2ppm = 5,
        filterByIsotopePattern = FALSE
    )
    expect_equal(param@instrument, "ORBITRAP")
    expect_equal(param@numberOfCandidates, 5)
    expect_equal(param@massAccuracyMS2ppm, 5)
    expect_false(param@filterByIsotopePattern)
})

test_that("formulaIdParam validates instrument argument", {
    expect_error(formulaIdParam(instrument = "INVALID"))
})

test_that("formulaIdParam validates isotopeMs2Settings argument", {
    expect_no_error(formulaIdParam(isotopeMs2Settings = "IGNORE"))
    expect_no_error(formulaIdParam(isotopeMs2Settings = "FILTER"))
    expect_no_error(formulaIdParam(isotopeMs2Settings = "SCORE"))
    expect_error(formulaIdParam(isotopeMs2Settings = "INVALID"))
})

test_that("formulaIdParam accepts candidateFormulas", {
    param <- formulaIdParam(candidateFormulas = "C10H12N2O")
    expect_equal(param@candidateFormulas, "C10H12N2O")

    param2 <- formulaIdParam(
        candidateFormulas = c("C6H12O6", "C10H12N2O")
    )
    expect_equal(param2@candidateFormulas, c("C6H12O6", "C10H12N2O"))
})

test_that("candidateFormulas is excluded from as.list serialisation", {
    param <- formulaIdParam(candidateFormulas = "C10H12N2O")
    l <- as.list(param)
    ## candidateFormulas is present in the list (slot extraction)
    expect_true("candidateFormulas" %in% names(l))
    ## but run()/config() will strip it before sending to the API
})

test_that("predictParam creates valid object with defaults", {
    param <- predictParam()
    expect_s4_class(param, "predictParam")
    expect_true(param@useScoreThreshold)
    expect_false(param@alwaysPredictHighRefMatches)
})

test_that("predictParam accepts custom values", {
    param <- predictParam(
        useScoreThreshold = FALSE,
        alwaysPredictHighRefMatches = TRUE
    )
    expect_false(param@useScoreThreshold)
    expect_true(param@alwaysPredictHighRefMatches)
})

test_that("spectraMatchingParam creates valid object with defaults", {
    param <- spectraMatchingParam()
    expect_s4_class(param, "spectraMatchingParam")
    expect_equal(param@peakDeviationPpm, 10)
    expect_equal(param@precursorDeviationPpm, 10)
    expect_equal(param@scoring, "MODIFIED_COSINE")
})

test_that("spectraMatchingParam validates scoring argument", {
    expect_no_error(spectraMatchingParam(scoring = "MODIFIED_COSINE"))
    expect_no_error(spectraMatchingParam(scoring = "INTENSITY"))
    expect_no_error(spectraMatchingParam(scoring = "GAUSSIAN"))
    expect_error(spectraMatchingParam(scoring = "INVALID"))
})

test_that("structureDbSearchParam creates valid object with defaults", {
    param <- structureDbSearchParam()
    expect_s4_class(param, "structureDbSearchParam")
    expect_true(param@tagStructuresWithLipidClass)
    expect_equal(param@expansiveSearchConfidenceMode, "APPROXIMATE")
})

test_that("structureDbSearchParam validates expansiveSearchConfidenceMode", {
    expect_no_error(structureDbSearchParam(expansiveSearchConfidenceMode = "APPROXIMATE"))
    expect_no_error(structureDbSearchParam(expansiveSearchConfidenceMode = "EXACT"))
    expect_no_error(structureDbSearchParam(expansiveSearchConfidenceMode = "OFF"))
    expect_error(structureDbSearchParam(expansiveSearchConfidenceMode = "INVALID"))
})

test_that("deNovoStructureParam creates valid object with defaults", {
    param <- deNovoStructureParam()
    expect_s4_class(param, "deNovoStructureParam")
    expect_equal(param@numberOfCandidateToPredict, 10)
})
test_that("deNovoStructureParam validates numberOfCandidateToPredict range", {
    expect_error(deNovoStructureParam(numberOfCandidateToPredict = 0))
    expect_error(deNovoStructureParam(numberOfCandidateToPredict = 129))
    expect_no_error(deNovoStructureParam(numberOfCandidateToPredict = 1))
    expect_no_error(deNovoStructureParam(numberOfCandidateToPredict = 128))
})

test_that("zodiacParam creates valid object with defaults", {
    param <- zodiacParam()
    expect_s4_class(param, "zodiacParam")
    expect_equal(param@consideredCandidatesAt300Mz, 10)
    expect_equal(param@consideredCandidatesAt800Mz, 50)
    expect_true(param@runInTwoSteps)
})

test_that("zodiacParam edgeFilterThreshold creates list when TRUE", {
    param <- zodiacParam(
        edgeFilterThreshold = TRUE,
        thresholdFilter = 0.9,
        minLocalCandidates = 2,
        minLocalConnections = 5
    )
    expect_type(param@edgeFilterThreshold, "list")
    expect_equal(param@edgeFilterThreshold$thresholdFilter, 0.9)
    expect_equal(param@edgeFilterThreshold$minLocalCandidates, 2)
    expect_equal(param@edgeFilterThreshold$minLocalConnections, 5)
})

test_that("zodiacParam gibbsSamplerParameters creates list when TRUE", {
    param <- zodiacParam(
        gibbsSamplerParameters = TRUE,
        iterations = 10000,
        burnInPeriod = 1000,
        numberOfMarkovChains = 5
    )
    expect_type(param@gibbsSamplerParameters, "list")
    expect_equal(param@gibbsSamplerParameters$iterations, 10000)
    expect_equal(param@gibbsSamplerParameters$burnInPeriod, 1000)
    expect_equal(param@gibbsSamplerParameters$numberOfMarkovChains, 5)
})
