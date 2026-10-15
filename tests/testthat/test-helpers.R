# Tests for helper functions
# These tests do not require the Sirius API

# =============================================================================
# .flatten_list
# =============================================================================

test_that(".flatten_list flattens nested lists correctly", {
    lst <- list(a = 1, b = list(c = 2, d = 3))
    flat <- .flatten_list(lst)

    expect_type(flat, "list")
    expect_equal(flat$a, 1)
    expect_equal(flat$c, 2)
    expect_equal(flat$d, 3)
})

test_that(".flatten_list handles empty list", {
    flat <- .flatten_list(list())
    expect_length(flat, 0)
})

test_that(".flatten_list handles deeply nested lists", {
    lst <- list(a = 1, b = list(c = list(d = list(e = 5))))
    flat <- .flatten_list(lst)

    expect_equal(flat$a, 1)
    expect_equal(flat$e, 5)
})

test_that(".flatten_list handles single-level named list", {
    lst <- list(a = 1, b = "two", c = TRUE)
    result <- .flatten_list(lst)

    expect_equal(result$a, 1)
    expect_equal(result$b, "two")
    expect_equal(result$c, TRUE)
})

test_that(".flatten_list keeps first occurrence of duplicate keys", {
    lst <- list(a = 1, nested = list(a = 2))
    result <- .flatten_list(lst)

    expect_equal(result$a, 1)
})

test_that(".flatten_list handles NULL values in list", {
    lst <- list(a = 1, b = NULL, c = 3)
    result <- .flatten_list(lst)

    expect_equal(result$a, 1)
    expect_equal(result$c, 3)
})

test_that(".flatten_list handles list with vector values", {
    lst <- list(a = c(1, 2, 3), b = list(c = c(4, 5)))
    result <- .flatten_list(lst)

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

    result <- .standardize_columns(df)

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
    result <- .standardize_columns(df)

    expect_equal(result$a, 1)
    expect_equal(result$b, 2)
})

test_that(".standardize_columns handles empty data.frame", {
    df <- data.frame()
    result <- .standardize_columns(df)
    expect_equal(nrow(result), 0)
})

test_that(".standardize_columns handles NA values in converted columns", {
    df <- data.frame(
        inchiKey = NA,
        xlogP = NA_character_,
        stringsAsFactors = FALSE
    )
    result <- .standardize_columns(df)

    expect_type(result$inchiKey, "character")
    expect_true(is.na(result$inchiKey))
    expect_type(result$xlogP, "double")
    expect_true(is.na(result$xlogP))
})

# =============================================================================
# .convert_to_dataframe
# =============================================================================

test_that(".convert_to_dataframe returns NULL for NULL input", {
    result <- .convert_to_dataframe(NULL, "test")
    expect_null(result)
})

test_that(".convert_to_dataframe adds section column", {
    input <- list(
        list(name = "kingdom", value = "Organic"),
        list(name = "class", value = "Lipids")
    )
    result <- .convert_to_dataframe(input, "classyFire")

    expect_s3_class(result, "data.frame")
    expect_true("section" %in% colnames(result))
    expect_true(all(result$section == "classyFire"))
    expect_equal(nrow(result), 2)
})

test_that(".convert_to_dataframe handles single-item list", {
    input <- list(list(name = "test", score = 0.9))
    result <- .convert_to_dataframe(input, "single")

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$section, "single")
})

# =============================================================================
# .force_classes_structure
# =============================================================================

test_that(".force_classes_structure has expected entries", {
    fcs <- .force_classes_structure

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

    result <- .process_compound_classes(mock_json)

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

    result <- .process_compound_classes(mock_json)

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

    result <- .map_sirius_to_xcms(srs, "s2")
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
        .map_sirius_to_xcms(srs, "s999"),
        "No matching XCMS ID"
    )
})

# =============================================================================
# .normalize_adducts
# =============================================================================

test_that(".normalize_adducts adds spaces around operators", {
    expect_equal(
        .normalize_adducts("[M+H]+"),
        "[M + H]+"
    )
    expect_equal(
        .normalize_adducts("[M-H]-"),
        "[M - H]-"
    )
    expect_equal(
        .normalize_adducts("[M+Na]+"),
        "[M + Na]+"
    )
})

test_that(".normalize_adducts leaves already-spaced adducts unchanged", {
    expect_equal(
        .normalize_adducts("[M + H]+"),
        "[M + H]+"
    )
    expect_equal(
        .normalize_adducts("[M - H]-"),
        "[M - H]-"
    )
})

test_that(".normalize_adducts handles complex adducts", {
    expect_equal(
        .normalize_adducts("[M+H3N+H]+"),
        "[M + H3N + H]+"
    )
    expect_equal(
        .normalize_adducts("[M-H2O+H]+"),
        "[M - H2O + H]+"
    )
    expect_equal(
        .normalize_adducts("[2M+K]+"),
        "[2M + K]+"
    )
    expect_equal(
        .normalize_adducts("[M+C2H4O2-H]-"),
        "[M + C2H4O2 - H]-"
    )
})

test_that(".normalize_adducts works on vectors", {
    input <- c("[M+H]+", "[M-H]-", "[M + Na]+")
    expected <- c("[M + H]+", "[M - H]-", "[M + Na]+")
    expect_equal(.normalize_adducts(input), expected)
})

test_that(".normalize_adducts handles unknown adduct placeholder", {
    expect_equal(
        .normalize_adducts("[M+?]+"),
        "[M + ?]+"
    )
})

# =============================================================================
# .format_command
# =============================================================================

test_that(".format_command removes 'config' and formats correctly", {
    cmd <- "sirius config --param1 value1 --param2 value2"
    formatted <- .format_command(cmd)

    expect_false(grepl("config", formatted))
    expect_true(grepl("sirius", formatted))
    expect_true(grepl("--param1", formatted))
})

test_that(".format_command handles command without config", {
    cmd <- "sirius --instrument QTOF"
    result <- .format_command(cmd)

    expect_true(grepl("sirius", result))
    expect_true(grepl("--instrument", result))
})

test_that(".format_command removes config and formats lines", {
    cmd <- "sirius config --instrument QTOF --ppm 10"
    result <- .format_command(cmd)

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

    result <- .clean_output(mock_output)

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

    result <- .clean_output(mock_output)

    expect_type(result, "character")
    expect_true(grepl("Job ID: 1", result))
    expect_true(grepl("RUNNING", result))
})

# =============================================================================
# .createpeaks
# =============================================================================

test_that(".createpeaks creates SimplePeak objects from matrix", {
    mat <- matrix(c(100.5, 1000, 200.3, 500), ncol = 2, byrow = TRUE)
    peaks <- .createpeaks(mat)

    expect_length(peaks, 2)
    expect_equal(peaks[[1]]$mz, 100.5)
    expect_equal(peaks[[1]]$intensity, 1000)
    expect_equal(peaks[[2]]$mz, 200.3)
    expect_equal(peaks[[2]]$intensity, 500)
})

test_that(".createpeaks handles single row matrix", {
    mat <- matrix(c(150.0, 2000), ncol = 2)
    peaks <- .createpeaks(mat)

    expect_length(peaks, 1)
    expect_equal(peaks[[1]]$mz, 150.0)
    expect_equal(peaks[[1]]$intensity, 2000)
})

test_that(".createpeaks handles matrix with many rows", {
    mat <- matrix(c(100, 1000, 200, 500, 300, 250, 400, 100, 500, 50),
                  ncol = 2, byrow = TRUE)
    peaks <- .createpeaks(mat)

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

    result <- .createspectraMS1(sp)

    expect_false(is.null(result))
    expect_equal(result$msLevel, 1L)
    expect_equal(result$scanNumber, 5L)
    expect_length(result$peaks, 3)
})

test_that(".createspectraMS1 returns NULL for empty input", {
    result <- .createspectraMS1(character())
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

    result <- .createspectraMSn(sp)

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

    result <- .createspectraMSn(sp)

    expect_type(result, "list")
    expect_length(result, 3)
    expect_equal(result[[1]]$msLevel, 2L)
    expect_equal(result[[2]]$msLevel, 2L)
    expect_equal(result[[3]]$msLevel, 3L)
    expect_equal(result[[3]]$precursorMz, 150.0)
})

test_that(".createspectraMSn returns NULL for empty input", {
    result <- .createspectraMSn(character())
    expect_null(result)
})

# =============================================================================
# .job_state / .wait_for_job  (job-waiting helpers used by run(wait = TRUE))
# These use a mocked Sirius API and do not require a running server.
# =============================================================================

# Build a mock Sirius object whose GetJob returns the supplied progress
# objects on successive calls. A NULL entry simulates an HTTP/GetJob error.
.mock_sirius <- function(states) {
    i <- 0L
    getjob <- function(project_id, job_id, ...) {
        i <<- i + 1L
        st <- states[[min(i, length(states))]]
        if (is.null(st)) stop("simulated GetJob HTTP error")
        list(id = job_id, progress = st)
    }
    api <- list(jobs_api = list(GetJob = getjob))
    new("Sirius", api = api, projectId = "p")
}

test_that(".job_state returns a single character state", {
    srs <- .mock_sirius(list(list(state = "RUNNING")))
    expect_equal(.job_state(srs, "1")$state, "RUNNING")
})

test_that(".job_state returns NA (not NULL) when progress is missing", {
    srs <- .mock_sirius(list(list(state = NULL)))
    s <- .job_state(srs, "1")
    expect_true(is.na(s$state))
    expect_length(s$state, 1L)
})

test_that(".job_state returns NA when GetJob errors", {
    srs <- .mock_sirius(list(NULL))
    expect_true(is.na(.job_state(srs, "1")$state))
})

test_that(".wait_for_job returns invisibly when the job is DONE", {
    srs <- .mock_sirius(list(
        list(state = "RUNNING", currentProgress = 1, maxProgress = 10),
        list(state = "DONE",    currentProgress = 10, maxProgress = 10)
    ))
    expect_invisible(.wait_for_job(srs, "1", pollInterval = 0))
    expect_identical(
        .wait_for_job(
            .mock_sirius(list(list(state = "DONE"))), "1", pollInterval = 0),
        "1"
    )
})

test_that(".wait_for_job errors (not loops) on FAILED with the error message", {
    srs <- .mock_sirius(list(
        list(state = "RUNNING"),
        list(state = "FAILED", errorMessage = "boom in tree computation")
    ))
    expect_error(
        .wait_for_job(srs, "7", pollInterval = 0),
        "FAILED.*boom in tree computation"
    )
})

test_that(".wait_for_job errors on CANCELED", {
    srs <- .mock_sirius(list(list(state = "CANCELED")))
    expect_error(
        .wait_for_job(srs, "9", pollInterval = 0),
        "CANCELED"
    )
})

test_that(".wait_for_job aborts (no length-0 crash) on repeated unreadable progress", {
    srs <- .mock_sirius(rep(list(NULL), 6))
    expect_error(
        .wait_for_job(srs, "3", pollInterval = 0),
        "Could not read the progress"
    )
})

test_that(".wait_for_job recovers from transient read failures", {
    srs <- .mock_sirius(list(
        NULL,
        list(state = "RUNNING", currentProgress = 2, maxProgress = 4),
        NULL,
        list(state = "DONE", currentProgress = 4, maxProgress = 4)
    ))
    expect_identical(.wait_for_job(srs, "5", pollInterval = 0), "5")
})

test_that(".wait_for_job times out on a stalled (stuck RUNNING) job", {
    srs <- .mock_sirius(
        rep(list(list(state = "RUNNING",
                      currentProgress = 1100, maxProgress = 1600)), 100)
    )
    expect_error(
        .wait_for_job(srs, "1", timeout = 1, pollInterval = 0.2),
        "Timed out"
    )
})

# =============================================================================
# Shared mock for the tests below
# =============================================================================

# Mock Sirius with the given API parts (e.g. `features_api = list(...)`); the
# connection and login checks always succeed.
.mock_api <- function(..., projectId = "p") {
    api <- c(list(info_api = list(GetInfo = function(...) list()),
                  login_and_account_api = list(IsLoggedIn = function(...) TRUE)),
             list(...))
    new("Sirius", api = api, projectId = projectId)
}

# =============================================================================
# .structures_for_one_feature
# =============================================================================

# Structure endpoints returning `payloads` (one list per candidate); counts calls.
.structure_api <- function(payloads, calls = new.env()) {
    calls$db <- 0L
    calls$denovo <- 0L
    models <- function() lapply(payloads, function(p)
        list(toSimpleType = function() p))
    .mock_api(features_api = list(
        GetStructureCandidates = function(...) {
            calls$db <- calls$db + 1L
            models()
        },
        GetDeNovoStructureCandidates = function(...) {
            calls$denovo <- calls$denovo + 1L
            models()
        }))
}

.cand <- function(formulaId, inchiKey, csiScore)
    list(formulaId = formulaId, adduct = "[M + H]+",
         molecularFormula = "C8H10N4O2", inchiKey = inchiKey,
         csiScore = csiScore)

.formula_res <- function(ids)
    data.frame(formulaId = ids, molecularFormula = "C8H10N4O2",
               adduct = "[M + H]+", siriusScore = seq_along(ids),
               stringsAsFactors = FALSE)

test_that(".structures_for_one_feature keeps top n per formula of formula_res", {
    srs <- .structure_api(list(
        .cand("F1", "A", -1), .cand("F1", "B", -2), .cand("F1", "C", -3),
        .cand("F2", "D", -4), .cand("F2", "E", -5),
        .cand("F3", "X", -0.5)                 # F3 not in formula_res
    ))
    res <- .structures_for_one_feature(srs, "fts1",
                                       .formula_res(c("F1", "F2")), top = 2)
    expect_equal(nrow(res), 4L)
    expect_false("F3" %in% res$formulaId)
    expect_equal(res$inchiKey[res$formulaId == "F1"], c("A", "B"))
    expect_equal(res$inchiKey[res$formulaId == "F2"], c("D", "E"))
    expect_type(res$csiScore, "double")
})

test_that(".structures_for_one_feature keeps formulas without structures", {
    srs <- .structure_api(list(.cand("F1", "A", -1)))
    res <- .structures_for_one_feature(srs, "fts1",
                                       .formula_res(c("F1", "F9")), top = 5)
    expect_equal(sort(res$formulaId), c("F1", "F9"))
    expect_true(is.na(res$inchiKey[res$formulaId == "F9"]))
})

test_that(".structures_for_one_feature returns formula_res when no structure is kept", {
    fr <- .formula_res(c("F1", "F2"))
    ## no candidates at all
    expect_identical(
        .structures_for_one_feature(.structure_api(list()), "fts1", fr,
                                    top = 5), fr)
    ## candidates only for formulas outside formula_res
    expect_identical(
        .structures_for_one_feature(.structure_api(list(.cand("F9", "X", -1))),
                                    "fts1", fr, top = 5), fr)
})

test_that(".structures_for_one_feature does not duplicate formula columns", {
    srs <- .structure_api(list(.cand("F1", "A", -1)))
    res <- .structures_for_one_feature(srs, "fts1", .formula_res("F1"),
                                       top = 5)
    expect_false(any(grepl("\\.(x|y)$", names(res))))
    expect_true(all(c("adduct", "molecularFormula", "inchiKey") %in% names(res)))
})

test_that(".structures_for_one_feature makes one call to the right endpoint", {
    calls <- new.env()
    srs <- .structure_api(list(.cand("F1", "A", -1)), calls)
    fr <- .formula_res(c("F1", "F2", "F3"))
    .structures_for_one_feature(srs, "fts1", fr, top = 5)
    expect_equal(calls$db, 1L)
    expect_equal(calls$denovo, 0L)
    .structures_for_one_feature(srs, "fts1", fr, top = 5, deNovo = TRUE)
    expect_equal(calls$db, 1L)
    expect_equal(calls$denovo, 1L)
})

test_that(".structures_for_one_feature output does not depend on API order", {
    fr <- .formula_res(c("F1", "F2"))
    grouped <- list(.cand("F1", "A", -1), .cand("F1", "B", -2),
                    .cand("F1", "C", -5), .cand("F2", "D", -3),
                    .cand("F2", "E", -4))
    interleaved <- grouped[c(1, 2, 4, 5, 3)]
    expect_identical(
        .structures_for_one_feature(.structure_api(grouped), "fts1", fr,
                                    top = 5),
        .structures_for_one_feature(.structure_api(interleaved), "fts1", fr,
                                    top = 5))
})

# =============================================================================
# run() feature selection
# =============================================================================

# Jobs API storing the submitted JobSubmission in `rec$js`.
.job_api <- function(rec)
    .mock_api(jobs_api = list(
        StartJob = function(project_id, job_submission, ...) {
            rec$js <- job_submission
            list(id = "1")
        }))

test_that("run() submits exactly the selected alignedFeaturesIds", {
    rec <- new.env()
    run(.job_api(rec), alignedFeaturesIds = c("f1", "f2"),
        formulaIdParams = formulaIdParam(), wait = FALSE)
    expect_equal(unlist(rec$js$alignedFeatureIds), c("f1", "f2"))
    expect_null(rec$js$compoundIds)
    ## a single id must stay a list (JSON array)
    run(.job_api(rec), alignedFeaturesIds = "f1",
        formulaIdParams = formulaIdParam(), wait = FALSE)
    expect_type(rec$js$alignedFeatureIds, "list")
    expect_equal(unlist(rec$js$alignedFeatureIds), "f1")
})

test_that("run() submits exactly the selected compoundsIds", {
    rec <- new.env()
    run(.job_api(rec), compoundsIds = "c1", alignedFeaturesIds = character(),
        formulaIdParams = formulaIdParam(), wait = FALSE)
    expect_equal(unlist(rec$js$compoundIds), "c1")
    expect_null(rec$js$alignedFeatureIds)
})

# =============================================================================
# openProject()
# =============================================================================

# Projects API with one open project "current"; closed ids go to `rec$closed`.
.project_api <- function(rec) {
    rec$closed <- character()
    .mock_api(projectId = "current", projects_api = list(
        GetProjects = function(...) list(list(projectId = "current")),
        CloseProject = function(project_id, ...)
            rec$closed <- c(rec$closed, project_id),
        OpenProject = function(...) NULL,
        CreateProject = function(...) NULL))
}

test_that("openProject() with a bad path keeps the open project open", {
    rec <- new.env()
    expect_error(
        suppressMessages(openProject(.project_api(rec), "other",
                                     path = "/nonexistent/xyz")),
        "does not exist")
    expect_length(rec$closed, 0L)
})

test_that("openProject() with a valid path closes the open project", {
    rec <- new.env()
    srs <- suppressMessages(openProject(.project_api(rec), "other",
                                        path = tempdir()))
    expect_equal(rec$closed, "current")
    expect_equal(srs@projectId, "other")
})

# =============================================================================
# createDb() / removeDb()
# =============================================================================

# Databases API: CreateDatabase returns `create_res` and records its
# parameters in `rec$params`; RemoveDatabase records `delete` in `rec$delete`.
.db_api <- function(rec, create_res = NULL)
    .mock_api(searchable_databases_api = list(
        CreateDatabase = function(database_id,
                                  searchable_database_parameters = NULL, ...) {
            rec$params <- searchable_database_parameters
            create_res
        },
        ImportIntoDatabase = function(...)
            list(toSimpleType = function() list(databaseId = "db1")),
        RemoveDatabase = function(database_id, delete = FALSE, ...)
            rec$delete <- c(rec$delete, delete)))

.mgf_file <- function() {
    f <- tempfile(fileext = ".mgf")
    writeLines("BEGIN IONS", f)
    f
}

test_that("createDb() sends the requested location to Sirius", {
    rec <- new.env()
    res <- createDb(.db_api(rec), databaseId = "db1", files = .mgf_file(),
                    location = tempdir())
    expect_equal(rec$params$location,
                 file.path(normalizePath(tempdir()), "db1.siriusdb"))
    expect_equal(res$databaseId, "db1")
})

test_that("createDb() reports the server error message", {
    err <- structure(list(status_code = 500,
                          response_as_text = function(...)
                              '{"detail":"Error when creating user database"}'),
                     class = "ApiResponse")
    expect_error(
        createDb(.db_api(new.env(), create_res = err), databaseId = "db1",
                 files = .mgf_file(), location = tempdir()),
        "HTTP 500.*Error when creating user database")
})

test_that("removeDb() passes delete to Sirius (default FALSE)", {
    rec <- new.env()
    srs <- .db_api(rec)
    expect_true(removeDb(srs, "db1"))
    expect_true(removeDb(srs, "db1", delete = TRUE))
    expect_equal(rec$delete, c(FALSE, TRUE))
})

# =============================================================================
# deleteFeatures()
# =============================================================================

# Features API with features f1-f3; each DeleteAlignedFeatures call goes to
# `rec$deleted`.
.delete_api <- function(rec) {
    rec$deleted <- list()
    fts <- lapply(c("f1", "f2", "f3"), function(id)
        list(alignedFeatureId = id, externalFeatureId = paste0("x", id)))
    srs <- .mock_api(features_api = list(
        GetAlignedFeatures = function(...) fts,
        DeleteAlignedFeatures = function(project_id, request_body, ...)
            rec$deleted <- c(rec$deleted, list(unlist(request_body)))))
    srs@featureMap <- data.frame(fts_sirius = c("f1", "f2", "f3"),
                                 fts_xcms = c("xf1", "xf2", "xf3"))
    srs
}

test_that("deleteFeatures() deletes the selected features in one call", {
    rec <- new.env()
    srs <- deleteFeatures(.delete_api(rec), featureId = c("f1", "f3"))
    expect_length(rec$deleted, 1L)
    expect_equal(rec$deleted[[1]], c("f1", "f3"))
    expect_equal(srs@featureMap$fts_sirius, "f2")
})

test_that("deleteFeatures() deletes all features by default", {
    rec <- new.env()
    srs <- deleteFeatures(.delete_api(rec))
    expect_equal(rec$deleted[[1]], c("f1", "f2", "f3"))
    expect_equal(nrow(srs@featureMap), 0L)
})

test_that("deleteFeatures() rejects ids that are not in the project", {
    expect_error(deleteFeatures(.delete_api(new.env()), featureId = "nope"),
                 "not in the project")
})
