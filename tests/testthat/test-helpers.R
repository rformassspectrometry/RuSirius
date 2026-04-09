# Tests for helper functions
# These tests do not require the Sirius API

# =============================================================================
# .flatten_list
# =============================================================================

test_that(".flatten_list flattens nested lists correctly", {
  lst <- list(a = 1, b = list(c = 2, d = 3))
  flat <- RuSirius:::.flatten_list(lst)

  expect_type(flat, "list")
  expect_equal(flat$a, 1)
  expect_equal(flat$c, 2)
  expect_equal(flat$d, 3)
})

test_that(".flatten_list handles empty list", {
  flat <- RuSirius:::.flatten_list(list())
  expect_length(flat, 0)
})

test_that(".flatten_list handles deeply nested lists", {
  lst <- list(a = 1, b = list(c = list(d = list(e = 5))))
  flat <- RuSirius:::.flatten_list(lst)

  expect_equal(flat$a, 1)
  expect_equal(flat$e, 5)
})

test_that(".flatten_list handles single-level named list", {
  lst <- list(a = 1, b = "two", c = TRUE)
  result <- RuSirius:::.flatten_list(lst)

  expect_equal(result$a, 1)
  expect_equal(result$b, "two")
  expect_equal(result$c, TRUE)
})

test_that(".flatten_list keeps first occurrence of duplicate keys", {
  lst <- list(a = 1, nested = list(a = 2))
  result <- RuSirius:::.flatten_list(lst)

  expect_equal(result$a, 1)
})

test_that(".flatten_list handles NULL values in list", {
  lst <- list(a = 1, b = NULL, c = 3)
  result <- RuSirius:::.flatten_list(lst)

  expect_equal(result$a, 1)
  expect_equal(result$c, 3)
})

test_that(".flatten_list handles list with vector values", {
  lst <- list(a = c(1, 2, 3), b = list(c = c(4, 5)))
  result <- RuSirius:::.flatten_list(lst)

  expect_equal(result$a, c(1, 2, 3))
  expect_equal(result$c, c(4, 5))
})

# =============================================================================
# .standardize_columns
# =============================================================================

test_that(".standardize_columns converts known columns", {
  df <- data.frame(
    inchiKey = 123,
    smiles = 456,
    structureName = 789,
    xlogP = "1.5",
    mcesDistToTopHit = "0.3",
    csiScore = "99.5",
    other = "unchanged",
    stringsAsFactors = FALSE
  )

  result <- RuSirius:::.standardize_columns(df)

  expect_type(result$inchiKey, "character")
  expect_type(result$smiles, "character")
  expect_type(result$structureName, "character")
  expect_type(result$xlogP, "double")
  expect_type(result$mcesDistToTopHit, "double")
  expect_type(result$csiScore, "double")
  expect_equal(result$other, "unchanged")
})

test_that(".standardize_columns handles missing columns gracefully", {
  df <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  result <- RuSirius:::.standardize_columns(df)

  expect_equal(result$a, 1)
  expect_equal(result$b, 2)
})

test_that(".standardize_columns handles empty data.frame", {
  df <- data.frame()
  result <- RuSirius:::.standardize_columns(df)
  expect_equal(nrow(result), 0)
})

test_that(".standardize_columns handles NA values in converted columns", {
  df <- data.frame(
    inchiKey = NA,
    xlogP = NA_character_,
    stringsAsFactors = FALSE
  )
  result <- RuSirius:::.standardize_columns(df)

  expect_type(result$inchiKey, "character")
  expect_true(is.na(result$inchiKey))
  expect_type(result$xlogP, "double")
  expect_true(is.na(result$xlogP))
})

# =============================================================================
# .convert_to_dataframe
# =============================================================================

test_that(".convert_to_dataframe returns NULL for NULL input", {
  result <- RuSirius:::.convert_to_dataframe(NULL, "test")
  expect_null(result)
})

test_that(".convert_to_dataframe adds section column", {
  input <- list(
    list(name = "kingdom", value = "Organic"),
    list(name = "class", value = "Lipids")
  )
  result <- RuSirius:::.convert_to_dataframe(input, "classyFire")

  expect_s3_class(result, "data.frame")
  expect_true("section" %in% colnames(result))
  expect_true(all(result$section == "classyFire"))
  expect_equal(nrow(result), 2)
})

test_that(".convert_to_dataframe handles single-item list", {
  input <- list(list(name = "test", score = 0.9))
  result <- RuSirius:::.convert_to_dataframe(input, "single")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$section, "single")
})

# =============================================================================
# .force_classes_structure
# =============================================================================

test_that(".force_classes_structure has expected entries", {
  fcs <- RuSirius:::.force_classes_structure

  expect_type(fcs, "character")
  expect_true("inchiKey" %in% names(fcs))
  expect_true("smiles" %in% names(fcs))
  expect_true("xlogP" %in% names(fcs))
  expect_true("csiScore" %in% names(fcs))

  expect_equal(fcs[["inchiKey"]], "character")
  expect_equal(fcs[["xlogP"]], "double")
})

# =============================================================================
# .process_compound_classes
# =============================================================================

test_that(".process_compound_classes handles minimal input", {
  mock_json <- list(
    npcPathway = list(name = "Alkaloids", probability = 0.9),
    npcSuperclass = list(name = "Amino acids", probability = 0.8),
    npcClass = list(name = "Tryptophan", probability = 0.7),
    classyFireLineage = NULL,
    classyFireAlternatives = NULL
  )

  result <- RuSirius:::.process_compound_classes(mock_json)

  expect_s3_class(result, "data.frame")
  expect_true("section" %in% colnames(result))
  expect_true(nrow(result) >= 3)
})

test_that(".process_compound_classes handles ClassyFire lineage", {
  mock_json <- list(
    npcPathway = list(name = "path", probability = 0.5),
    npcSuperclass = list(name = "super", probability = 0.5),
    npcClass = list(name = "cls", probability = 0.5),
    classyFireLineage = list(
      list(name = "Organic", level = "Kingdom", probability = 0.9),
      list(name = "Lipids", level = "Superclass", probability = 0.8)
    ),
    classyFireAlternatives = NULL
  )

  result <- RuSirius:::.process_compound_classes(mock_json)

  expect_s3_class(result, "data.frame")
  expect_true(any(result$section == "classyFireLineage"))
  if ("level" %in% colnames(result)) {
    expect_s3_class(result$level, "factor")
  }
})

# =============================================================================
# .map_sirius_to_xcms
# =============================================================================

test_that(".map_sirius_to_xcms returns matching xcms id", {
  srs <- new("Sirius")
  srs@featureMap <- data.frame(
    fts_sirius = c("s1", "s2", "s3"),
    fts_xcms = c("x1", "x2", "x3"),
    stringsAsFactors = FALSE
  )

  result <- RuSirius:::.map_sirius_to_xcms(srs, "s2")
  expect_equal(result, "x2")
})

test_that(".map_sirius_to_xcms warns for missing id", {
  srs <- new("Sirius")
  srs@featureMap <- data.frame(
    fts_sirius = c("s1", "s2"),
    fts_xcms = c("x1", "x2"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    RuSirius:::.map_sirius_to_xcms(srs, "s999"),
    "No matching XCMS ID"
  )
})

# =============================================================================
# .normalize_adducts
# =============================================================================

test_that(".normalize_adducts adds spaces around operators", {
  expect_equal(
    RuSirius:::.normalize_adducts("[M+H]+"),
    "[M + H]+"
  )
  expect_equal(
    RuSirius:::.normalize_adducts("[M-H]-"),
    "[M - H]-"
  )
  expect_equal(
    RuSirius:::.normalize_adducts("[M+Na]+"),
    "[M + Na]+"
  )
})

test_that(".normalize_adducts leaves already-spaced adducts unchanged", {
  expect_equal(
    RuSirius:::.normalize_adducts("[M + H]+"),
    "[M + H]+"
  )
  expect_equal(
    RuSirius:::.normalize_adducts("[M - H]-"),
    "[M - H]-"
  )
})

test_that(".normalize_adducts handles complex adducts", {
  expect_equal(
    RuSirius:::.normalize_adducts("[M+H3N+H]+"),
    "[M + H3N + H]+"
  )
  expect_equal(
    RuSirius:::.normalize_adducts("[M-H2O+H]+"),
    "[M - H2O + H]+"
  )
  expect_equal(
    RuSirius:::.normalize_adducts("[2M+K]+"),
    "[2M + K]+"
  )
  expect_equal(
    RuSirius:::.normalize_adducts("[M+C2H4O2-H]-"),
    "[M + C2H4O2 - H]-"
  )
})

test_that(".normalize_adducts works on vectors", {
  input <- c("[M+H]+", "[M-H]-", "[M + Na]+")
  expected <- c("[M + H]+", "[M - H]-", "[M + Na]+")
  expect_equal(RuSirius:::.normalize_adducts(input), expected)
})

test_that(".normalize_adducts handles unknown adduct placeholder", {
  expect_equal(
    RuSirius:::.normalize_adducts("[M+?]+"),
    "[M + ?]+"
  )
})

# =============================================================================
# .format_command
# =============================================================================

test_that(".format_command removes 'config' and formats correctly", {
  cmd <- "sirius config --param1 value1 --param2 value2"
  formatted <- RuSirius:::.format_command(cmd)

  expect_false(grepl("config", formatted))
  expect_true(grepl("sirius", formatted))
  expect_true(grepl("--param1", formatted))
})

test_that(".format_command handles command without config", {
  cmd <- "sirius --instrument QTOF"
  result <- RuSirius:::.format_command(cmd)

  expect_true(grepl("sirius", result))
  expect_true(grepl("--instrument", result))
})

test_that(".format_command removes config and formats lines", {
  cmd <- "sirius config --instrument QTOF --ppm 10"
  result <- RuSirius:::.format_command(cmd)

  expect_false(grepl("config", result))
  expect_true(grepl("sirius", result))
  expect_true(grepl("--instrument", result))
  expect_true(grepl("QTOF", result))
})

# =============================================================================
# .clean_output
# =============================================================================

test_that(".clean_output formats job output correctly", {
  mock_output <- list(
    id = "42",
    command = "sirius config --param1 value1",
    progress = list(
      state = "DONE",
      currentProgress = 100,
      maxProgress = 100
    ),
    affectedCompoundIds = c("comp1", "comp2"),
    affectedAlignedFeatureIds = list("feat1", "feat2")
  )

  result <- RuSirius:::.clean_output(mock_output)

  expect_type(result, "character")
  expect_true(grepl("Job ID: 42", result))
  expect_true(grepl("DONE", result))
  expect_true(grepl("100", result))
  expect_true(grepl("feat1", result))
  expect_true(grepl("comp1", result))
})

test_that(".clean_output handles empty affected IDs", {
  mock_output <- list(
    id = "1",
    command = "sirius config --test",
    progress = list(
      state = "RUNNING",
      currentProgress = 50,
      maxProgress = 100
    ),
    affectedCompoundIds = character(0),
    affectedAlignedFeatureIds = list()
  )

  result <- RuSirius:::.clean_output(mock_output)

  expect_type(result, "character")
  expect_true(grepl("Job ID: 1", result))
  expect_true(grepl("RUNNING", result))
})

# =============================================================================
# .createpeaks
# =============================================================================

test_that(".createpeaks creates SimplePeak objects from matrix", {
  mat <- matrix(c(100.5, 1000, 200.3, 500), ncol = 2, byrow = TRUE)
  peaks <- RuSirius:::.createpeaks(mat)

  expect_length(peaks, 2)
  expect_equal(peaks[[1]]$mz, 100.5)
  expect_equal(peaks[[1]]$intensity, 1000)
  expect_equal(peaks[[2]]$mz, 200.3)
  expect_equal(peaks[[2]]$intensity, 500)
})

test_that(".createpeaks handles single row matrix", {
  mat <- matrix(c(150.0, 2000), ncol = 2)
  peaks <- RuSirius:::.createpeaks(mat)

  expect_length(peaks, 1)
  expect_equal(peaks[[1]]$mz, 150.0)
  expect_equal(peaks[[1]]$intensity, 2000)
})

test_that(".createpeaks handles matrix with many rows", {
  mat <- matrix(
    c(100, 1000, 200, 500, 300, 250, 400, 100, 500, 50),
    ncol = 2,
    byrow = TRUE
  )
  peaks <- RuSirius:::.createpeaks(mat)

  expect_length(peaks, 5)
  expect_equal(peaks[[5]]$mz, 500)
  expect_equal(peaks[[5]]$intensity, 50)
})

# =============================================================================
# .createspectraMS1
# =============================================================================

test_that(".createspectraMS1 creates BasicSpectrum from MS1 Spectra", {
  sp <- Spectra(DataFrame(
    msLevel = 1L,
    polarity = 1L,
    scanIndex = 5L,
    precursorMz = NA_real_,
    dataOrigin = "file1",
    mz = I(list(c(100.1, 200.2, 300.3))),
    intensity = I(list(c(1000, 500, 200)))
  ))

  result <- RuSirius:::.createspectraMS1(sp)

  expect_false(is.null(result))
  expect_equal(result$msLevel, 1L)
  expect_equal(result$scanNumber, 5L)
  expect_length(result$peaks, 3)
})

test_that(".createspectraMS1 returns NULL for empty input", {
  result <- RuSirius:::.createspectraMS1(character())
  expect_null(result)
})

# =============================================================================
# .createspectraMSn
# =============================================================================

test_that(".createspectraMSn creates single MS2 spectrum", {
  sp <- Spectra(DataFrame(
    msLevel = 2L,
    polarity = 1L,
    scanIndex = 10L,
    precursorMz = 300.0,
    dataOrigin = "file1",
    mz = I(list(c(100, 150))),
    intensity = I(list(c(999, 500)))
  ))

  result <- RuSirius:::.createspectraMSn(sp)

  expect_type(result, "list")
  expect_length(result, 1)
  expect_equal(result[[1]]$msLevel, 2L)
  expect_equal(result[[1]]$precursorMz, 300.0)
  expect_equal(result[[1]]$scanNumber, 10L)
})

test_that(".createspectraMSn creates multiple MS2 spectra", {
  sp <- Spectra(DataFrame(
    msLevel = c(2L, 2L, 3L),
    polarity = c(1L, 1L, 1L),
    scanIndex = c(1L, 2L, 3L),
    precursorMz = c(300.0, 300.0, 150.0),
    dataOrigin = c("file1", "file1", "file1"),
    mz = I(list(c(100, 150), c(120, 180), c(50))),
    intensity = I(list(c(999, 500), c(800, 200), c(300)))
  ))

  result <- RuSirius:::.createspectraMSn(sp)

  expect_type(result, "list")
  expect_length(result, 3)
  expect_equal(result[[1]]$msLevel, 2L)
  expect_equal(result[[2]]$msLevel, 2L)
  expect_equal(result[[3]]$msLevel, 3L)
  expect_equal(result[[3]]$precursorMz, 150.0)
})

test_that(".createspectraMSn returns NULL for empty input", {
  result <- RuSirius:::.createspectraMSn(character())
  expect_null(result)
})
