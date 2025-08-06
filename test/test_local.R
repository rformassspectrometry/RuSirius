# These test need to be run locally as they need to run SIrius and a
# authentification token is needed.

library(Spectra)
library(testthat)

## test object
dda_file <- system.file("TripleTOF-SWATH", "PestMix1_DDA.mzML",
                        package = "msdata")
sp <- Spectra(dda_file)
sp <- setBackend(sp, MsBackendMemory())
sp <- filterEmptySpectra(sp)

## change the function below for the new one i'm implementing in Spectra.
idxs <- groupMsFragments(sp)
sp$Msn_idx <- idxs

empty_bd <- MsBackendSirius()

## MSbackendSIrius functions test

test_that("logIn works properly", {
    test_that("logIn works", {
        expect_error(logIn(empty_bd), "Please provide a username and a password.")
        expect_error(logIn(empty_bd, username = "test", password = "test"),
                     "Connection to Sirius not valid")
    })
})

## MSbackendSirius test
test_that("MsBackendSirius works properly", {
    expect_identical(new("MsBackendSirius"), empty_bd)
    expect_output(show(empty_bd), "MsBackendSirius with 0 spectra")

})

test_that("checkConnection works", {
              expect_false(checkConnection(empty_bd))
})

test_that("listOpenProjects works", {
    expect_error(listOpenProject(empty_bd), "Connection to Sirius not valid")
})

test_that("openProject works", {
    expect_error(openProject(empty_bd), "projectId needs to be provided.")
    expect_error(openProject(empty_bd, projectId = "test"),
                 "Connection to Sirius not valid")
})

test_that("projectInfo works", {
    expect_error(projectInfo(empty_bd), "Connection to Sirius not valid")
})

test_that("shutdown works", {
    expect_error(shutdown(empty_bd), "Connection to Sirius not valid")
})

test_that("supportsetbackend returns TRUE"{
    expect_true(supportsSetBackend(be_empty))
})
## next backendIni and show on ini backend but not populated.
