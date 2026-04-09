# Tests for parameter classes and config()
# These tests do not require the Sirius API

# =============================================================================
# formulaIdParam
# =============================================================================

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
  expect_true("candidateFormulas" %in% names(l))
})

test_that("formulaIdParam FTICR instrument works", {
  param <- formulaIdParam(instrument = "FTICR")
  expect_equal(param@instrument, "FTICR")
})

test_that("formulaIdParam collapses formula constraints to string", {
  param <- formulaIdParam(
    enforcedFormulaConstraints = c("H", "C", "N"),
    fallbackFormulaConstraints = c("S", "Cl")
  )
  expect_equal(param@enforcedFormulaConstraints, "HCN")
  expect_equal(param@fallbackFormulaConstraints, "SCl")
})

test_that("formulaIdParam disabling heuristic sets NA", {
  param <- formulaIdParam(useHeuristic = FALSE)
  expect_identical(param@useHeuristic, NA)
})

test_that("formulaIdParam enabling heuristic creates list", {
  param <- formulaIdParam(
    useHeuristic = TRUE,
    useHeuristicAboveMz = 200,
    useOnlyHeuristicAboveMz = 500
  )
  expect_type(param@useHeuristic, "list")
  expect_equal(param@useHeuristic$useHeuristicAboveMz, 200)
  expect_equal(param@useHeuristic$useOnlyHeuristicAboveMz, 500)
})

test_that("formulaIdParam disabling ilpTimeout sets NA", {
  param <- formulaIdParam(ilpTimeout = FALSE)
  expect_identical(param@ilpTimeout, NA)
})

test_that("formulaIdParam enabling ilpTimeout creates list", {
  param <- formulaIdParam(
    ilpTimeout = TRUE,
    numberOfSecondsPerDecomposition = 10,
    numberOfSecondsPerInstance = 60
  )
  expect_type(param@ilpTimeout, "list")
  expect_equal(param@ilpTimeout$numberOfSecondsPerDecomposition, 10)
  expect_equal(param@ilpTimeout$numberOfSecondsPerInstance, 60)
})

test_that("formulaIdParam accepts empty formulaSearchDBs", {
  param <- formulaIdParam(formulaSearchDBs = character(0))
  expect_length(param@formulaSearchDBs, 0)
})

test_that("formulaIdParam accepts custom database list", {
  param <- formulaIdParam(formulaSearchDBs = c("HMDB", "CHEBI"))
  expect_equal(param@formulaSearchDBs, c("HMDB", "CHEBI"))
})

# =============================================================================
# predictParam
# =============================================================================

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

test_that("predictParam all options enabled", {
  param <- predictParam(
    useScoreThreshold = TRUE,
    alwaysPredictHighRefMatches = TRUE
  )
  expect_true(param@useScoreThreshold)
  expect_true(param@alwaysPredictHighRefMatches)
})

test_that("predictParam all options disabled", {
  param <- predictParam(
    useScoreThreshold = FALSE,
    alwaysPredictHighRefMatches = FALSE
  )
  expect_false(param@useScoreThreshold)
  expect_false(param@alwaysPredictHighRefMatches)
})

# =============================================================================
# spectraMatchingParam
# =============================================================================

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

test_that("spectraMatchingParam INTENSITY scoring works", {
  param <- spectraMatchingParam(scoring = "INTENSITY")
  expect_equal(param@scoring, "INTENSITY")
})

test_that("spectraMatchingParam GAUSSIAN scoring works", {
  param <- spectraMatchingParam(scoring = "GAUSSIAN")
  expect_equal(param@scoring, "GAUSSIAN")
})

test_that("spectraMatchingParam custom databases work", {
  param <- spectraMatchingParam(spectraSearchDBs = c("HMDB", "GNPS"))
  expect_equal(param@spectraSearchDBs, c("HMDB", "GNPS"))
})

test_that("spectraMatchingParam custom ppm values work", {
  param <- spectraMatchingParam(
    peakDeviationPpm = 5,
    precursorDeviationPpm = 20
  )
  expect_equal(param@peakDeviationPpm, 5)
  expect_equal(param@precursorDeviationPpm, 20)
})

# =============================================================================
# structureDbSearchParam
# =============================================================================

test_that("structureDbSearchParam creates valid object with defaults", {
  param <- structureDbSearchParam()
  expect_s4_class(param, "structureDbSearchParam")
  expect_true(param@tagStructuresWithLipidClass)
  expect_equal(param@expansiveSearchConfidenceMode, "APPROXIMATE")
})

test_that("structureDbSearchParam validates expansiveSearchConfidenceMode", {
  expect_no_error(structureDbSearchParam(
    expansiveSearchConfidenceMode = "APPROXIMATE"
  ))
  expect_no_error(structureDbSearchParam(
    expansiveSearchConfidenceMode = "EXACT"
  ))
  expect_no_error(structureDbSearchParam(expansiveSearchConfidenceMode = "OFF"))
  expect_error(structureDbSearchParam(
    expansiveSearchConfidenceMode = "INVALID"
  ))
})

test_that("structureDbSearchParam EXACT confidence mode", {
  param <- structureDbSearchParam(expansiveSearchConfidenceMode = "EXACT")
  expect_equal(param@expansiveSearchConfidenceMode, "EXACT")
})

test_that("structureDbSearchParam OFF confidence mode", {
  param <- structureDbSearchParam(expansiveSearchConfidenceMode = "OFF")
  expect_equal(param@expansiveSearchConfidenceMode, "OFF")
})

test_that("structureDbSearchParam lipid tagging disabled", {
  param <- structureDbSearchParam(tagStructuresWithLipidClass = FALSE)
  expect_false(param@tagStructuresWithLipidClass)
})

test_that("structureDbSearchParam custom databases", {
  param <- structureDbSearchParam(
    structureSearchDbs = c("PUBCHEM", "HMDB", "CHEBI")
  )
  expect_equal(param@structureSearchDbs, c("PUBCHEM", "HMDB", "CHEBI"))
})

# =============================================================================
# deNovoStructureParam
# =============================================================================

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

test_that("deNovoStructureParam boundary values work", {
  param_min <- deNovoStructureParam(numberOfCandidateToPredict = 1)
  expect_equal(param_min@numberOfCandidateToPredict, 1)

  param_max <- deNovoStructureParam(numberOfCandidateToPredict = 128)
  expect_equal(param_max@numberOfCandidateToPredict, 128)
})

test_that("deNovoStructureParam mid-range value", {
  param <- deNovoStructureParam(numberOfCandidateToPredict = 64)
  expect_equal(param@numberOfCandidateToPredict, 64)
})

# =============================================================================
# zodiacParam
# =============================================================================

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

test_that("zodiacParam disabling edgeFilterThreshold sets NA", {
  param <- zodiacParam(edgeFilterThreshold = FALSE)
  expect_identical(param@edgeFilterThreshold, NA)
})

test_that("zodiacParam disabling gibbsSamplerParameters sets NA", {
  param <- zodiacParam(gibbsSamplerParameters = FALSE)
  expect_identical(param@gibbsSamplerParameters, NA)
})

test_that("zodiacParam custom Gibbs sampler values", {
  param <- zodiacParam(
    gibbsSamplerParameters = TRUE,
    iterations = 50000,
    burnInPeriod = 5000,
    numberOfMarkovChains = 20
  )
  expect_equal(param@gibbsSamplerParameters$iterations, 50000)
  expect_equal(param@gibbsSamplerParameters$burnInPeriod, 5000)
  expect_equal(param@gibbsSamplerParameters$numberOfMarkovChains, 20)
})

# =============================================================================
# config()
# =============================================================================

test_that("config auto-enables formulaIdParams and predictParams for msNovelistParams", {
  cfg <- config(
    alignedFeaturesIds = "feat1",
    msNovelistParams = deNovoStructureParam(numberOfCandidateToPredict = 5)
  )
  expect_type(cfg@formulaIdParams, "list")
  expect_true(cfg@formulaIdParams$enabled)
  expect_type(cfg@fingerprintPredictionParams, "list")
  expect_true(cfg@fingerprintPredictionParams$enabled)
  expect_type(cfg@msNovelistParams, "list")
  expect_true(cfg@msNovelistParams$enabled)
})

test_that("config auto-enables formulaIdParams and predictParams for structureDbSearchParams", {
  cfg <- config(
    alignedFeaturesIds = "feat1",
    structureDbSearchParams = structureDbSearchParam()
  )
  expect_type(cfg@formulaIdParams, "list")
  expect_true(cfg@formulaIdParams$enabled)
  expect_type(cfg@fingerprintPredictionParams, "list")
  expect_true(cfg@fingerprintPredictionParams$enabled)
})

test_that("config auto-enables formulaIdParams for predictParams", {
  cfg <- config(
    alignedFeaturesIds = "feat1",
    predictParams = predictParam()
  )
  expect_type(cfg@formulaIdParams, "list")
  expect_true(cfg@formulaIdParams$enabled)
})

test_that("config does not override explicit formulaIdParams", {
  custom <- formulaIdParam(instrument = "ORBITRAP", numberOfCandidates = 3)
  cfg <- config(
    alignedFeaturesIds = "feat1",
    formulaIdParams = custom,
    msNovelistParams = deNovoStructureParam()
  )
  expect_type(cfg@formulaIdParams, "list")
  expect_true(cfg@formulaIdParams$enabled)
  expect_equal(cfg@formulaIdParams$instrument, "ORBITRAP")
  expect_equal(cfg@formulaIdParams$numberOfCandidates, 3)
})

test_that("config validation requires features", {
  expect_error(
    config(compoundsIds = character(), alignedFeaturesIds = character()),
    "compoundsIds.*alignedFeaturesIds"
  )
})

test_that("config with compoundsIds only is valid", {
  cfg <- config(compoundsIds = "compound1")
  expect_s4_class(cfg, "config")
  expect_equal(cfg@compoundsIds, "compound1")
})

test_that("config normalizes adducts", {
  cfg <- config(
    alignedFeaturesIds = "feat1",
    fallbackAdducts = c("[M+H]+", "[M-H]-"),
    enforceAdducts = c("[M+Na]+"),
    detectableAdducts = c("[M+K]+")
  )
  expect_equal(cfg@fallbackAdducts, c("[M + H]+", "[M - H]-"))
  expect_equal(cfg@enforceAdducts, "[M + Na]+")
  expect_equal(cfg@detectableAdducts, "[M + K]+")
})

test_that("config sets recompute flag", {
  cfg <- config(
    alignedFeaturesIds = "feat1",
    recompute = TRUE
  )
  expect_true(cfg@recompute)
})

test_that("config spectraSearchParams enabled flag", {
  cfg <- config(
    alignedFeaturesIds = "feat1",
    spectraSearchParams = spectraMatchingParam()
  )
  expect_type(cfg@spectraSearchParams, "list")
  expect_true(cfg@spectraSearchParams$enabled)
})

test_that("config zodiacParams enabled flag", {
  cfg <- config(
    alignedFeaturesIds = "feat1",
    zodiacParams = zodiacParam()
  )
  expect_type(cfg@zodiacParams, "list")
  expect_true(cfg@zodiacParams$enabled)
})

test_that("config canopusParams enabled when predict is used", {
  cfg <- config(
    alignedFeaturesIds = "feat1",
    predictParams = predictParam()
  )
  expect_type(cfg@canopusParams, "list")
  expect_true(cfg@canopusParams$enabled)
})

test_that("config canopusParams NA when predict not used", {
  cfg <- config(
    alignedFeaturesIds = "feat1",
    formulaIdParams = formulaIdParam()
  )
  expect_identical(cfg@canopusParams, NA)
})

test_that("config preserves formula + predict when deNovo is used", {
  cfg <- config(
    alignedFeaturesIds = "feat1",
    msNovelistParams = deNovoStructureParam(),
    formulaIdParams = formulaIdParam(instrument = "FTICR")
  )
  expect_equal(cfg@formulaIdParams$instrument, "FTICR")
  expect_true(cfg@fingerprintPredictionParams$enabled)
  expect_true(cfg@msNovelistParams$enabled)
})

# =============================================================================
# as.list() serialization of param objects
# =============================================================================

test_that("as.list works for formulaIdParam", {
  param <- formulaIdParam(instrument = "ORBITRAP", numberOfCandidates = 3)
  l <- as.list(param)
  expect_type(l, "list")
  expect_equal(l$instrument, "ORBITRAP")
  expect_equal(l$numberOfCandidates, 3)
})

test_that("as.list works for predictParam", {
  param <- predictParam(useScoreThreshold = FALSE)
  l <- as.list(param)
  expect_type(l, "list")
  expect_false(l$useScoreThreshold)
})

test_that("as.list works for spectraMatchingParam", {
  param <- spectraMatchingParam(peakDeviationPpm = 5)
  l <- as.list(param)
  expect_type(l, "list")
  expect_equal(l$peakDeviationPpm, 5)
})

test_that("as.list works for structureDbSearchParam", {
  param <- structureDbSearchParam(tagStructuresWithLipidClass = FALSE)
  l <- as.list(param)
  expect_type(l, "list")
  expect_false(l$tagStructuresWithLipidClass)
})

test_that("as.list works for zodiacParam", {
  param <- zodiacParam(consideredCandidatesAt300Mz = 20)
  l <- as.list(param)
  expect_type(l, "list")
  expect_equal(l$consideredCandidatesAt300Mz, 20)
})

test_that("as.list works for deNovoStructureParam", {
  param <- deNovoStructureParam(numberOfCandidateToPredict = 50)
  l <- as.list(param)
  expect_type(l, "list")
  expect_equal(l$numberOfCandidateToPredict, 50)
})

test_that("as.list for config includes all slots", {
  cfg <- config(
    alignedFeaturesIds = c("f1", "f2"),
    formulaIdParams = formulaIdParam(),
    spectraSearchParams = spectraMatchingParam()
  )
  l <- as.list(cfg)
  expect_type(l, "list")
  expect_true("alignedFeaturesIds" %in% names(l))
  expect_true("fallbackAdducts" %in% names(l))
  expect_true("formulaIdParams" %in% names(l))
  expect_true("spectraSearchParams" %in% names(l))
})
