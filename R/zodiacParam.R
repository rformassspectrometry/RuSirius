#' @title Configuration fr re-ranking of Molecular Formula Annotation
#'
#' @name zodiacParam
#'
#' @description
#' This function configures the parameters for the re-ranking of the previously
#' computed molecular formula annotation in Sirius. This step is quite
#' computationally and memory demanding, it is advised to perform it only if
#' de novo structure annotation is used later on
#'
#' ZODIAC uses the top X molecular formula candidates for each molecule from
#' SIRIUS to build a similarity network, and uses Bayesian statistics to
#' re-rank those candidates.
#'
#' @param consideredCandidatesAt300Mz An `integer(1)` specifying the maximum
#'        number of candidate molecular formulas considered by ZODIAC for
#'        compounds below 300 m/z. Default is `10`.
#' @param consideredCandidatesAt800Mz An `integer(1)` specifying the maximum
#'        number of candidate molecular formulas considered by ZODIAC for \
#'        compounds above 800 m/z. Default is `50`.
#' @param runInTwoSteps `logical` specifying whether ZODIAC uses a
#'        2-step approach. First running 'good quality compounds' only, and
#'        afterwards including the remaining compounds. Default is `TRUE`.
#' @param edgeFilterThreshold `logical` value specifying whether ZODIAC uses
#'        an edge filter threshold. Default is `TRUE`.
#' @param thresholdFilter `numeric(1)` value specifying the threshold filter.
#'       Default is `0.95`.
#' @param minLocalCandidates `numeric(1)` value specifying the minimum number of
#'        local candidates. Default is `1`.
#' @param minLocalConnections `numeric(1)` value specifying the minimum number of
#'        local connections. Default is `10`.
#' @param gibbsSamplerParameters `logical` value specifying whether ZODIAC
#'        uses Gibbs sampler parameters. Default is `TRUE`.
#' @param iterations `numeric(1)` specifying the number of iterations. Default is
#'       `20000`.
#' @param burnInPeriod `numeric(1)` specifying the burn-in period. Default is
#'       `2000`.
#' @param numberOfMarkovChains `numeric(1)` specifying the number of Markov
#'        chains. Default is `10`.
#'
#' @importFrom methods setClass setClassUnion
#' @importClassesFrom ProtGenerics Param
#'
#' @note
#' For more information, see the Sirius
#' [documentation](https://v6.docs.sirius-ms.io/methods-background).
#'
#' @references reference
#'
#' @return An object of class `zodiacParam`.
#'
#' @examples
#' # Example of setting up the parameter for Zodiac re-ranking
#' param <- zodiacParam(consideredCandidatesAt300Mz = 10,
#'                      consideredCandidatesAt800Mz = 50,
#'                      runInTwoSteps = TRUE,
#'                      edgeFilterThreshold = TRUE,
#'                      thresholdFilter = 0.95,
#'                      minLocalCandidates = 1,
#'                      minLocalConnections = 10,
#'                      gibbsSamplerParameters = TRUE,
#'                      iterations = 20000,
#'                      burnInPeriod = 2000,
#'                      numberOfMarkovChains = 10)
#'
NULL

setClassUnion("listOrLogical", c("list", "logical"))

#' @noRd
setClass("zodiacParam",
         slots = c(consideredCandidatesAt300Mz = "numeric",
                   consideredCandidatesAt800Mz = "numeric",
                   runInTwoSteps = "logical",
                   edgeFilterThreshold = "listOrLogical",
                   thresholdFilter = "numeric",
                   minLocalCandidates = "numeric",
                   minLocalConnections = "numeric",
                   gibbsSamplerParameters = "listOrLogical",
                   iterations = "numeric",
                   burnInPeriod = "numeric",
                   numberOfMarkovChains = "numeric"
         ),
         contains = "Param",
         prototype = prototype(
             consideredCandidatesAt300Mz = 10,
             consideredCandidatesAt800Mz = 50,
             runInTwoSteps = TRUE,
             edgeFilterThreshold = TRUE,
             thresholdFilter = 0.1,
             minLocalCandidates = 1,
             minLocalConnections = 1,
             gibbsSamplerParameters = TRUE,
             iterations = 1000,
             burnInPeriod = 100,
             numberOfMarkovChains = 1),
         validity = function(object) {
             TRUE
         }
)

#' @rdname zodiacParam
#' @export
zodiacParam <- function(consideredCandidatesAt300Mz = 10,
                        consideredCandidatesAt800Mz = 50,
                        runInTwoSteps = TRUE,
                        edgeFilterThreshold = TRUE,
                        thresholdFilter = 0.95,
                        minLocalCandidates = 1,
                        minLocalConnections = 10,
                        gibbsSamplerParameters = TRUE,
                        iterations = 20000,
                        burnInPeriod = 2000,
                        numberOfMarkovChains = 10){
    if (edgeFilterThreshold == TRUE)
        edgeFilterThreshold <- list(
            thresholdFilter = thresholdFilter,
            minLocalCandidates = minLocalCandidates,
            minLocalConnections = minLocalConnections
        )
    else edgeFilterThreshold <- NA
    if (gibbsSamplerParameters == TRUE)
        gibbsSamplerParameters <- list(
            iterations = iterations,
            burnInPeriod = burnInPeriod,
            numberOfMarkovChains = numberOfMarkovChains
        )
    else gibbsSamplerParameters <- NA
    new("zodiacParam",
        consideredCandidatesAt300Mz = consideredCandidatesAt300Mz,
        consideredCandidatesAt800Mz = consideredCandidatesAt800Mz,
        runInTwoSteps = runInTwoSteps,
        edgeFilterThreshold = edgeFilterThreshold,
        gibbsSamplerParameters = gibbsSamplerParameters)
}
