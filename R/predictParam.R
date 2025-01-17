#' @title Predicting FingerPrint and compounds Identifications
#'
#' @description
#' This function creates an object of class `predictParam` that can be used to
#' predict molecular fingerprints and compound identifications using
#' *CSI:FingerID* and *CANOPUS*.
#'
#' CSI:FIngerID identifies the structure of a molecule by predicting its
#' molecular fingerprint and using this fingerprint to search in a molecular
#' structure database.
#'
#' CANOPUS (Dührkop et al.) predicts the presense/absense of more than 2500
#' compound classes. CANOPUS predicts these classes based solely on MS/MS data
#' and without requiring database information. This means it can identify a
#' class even if no molecular structure of that class exists in the molecular
#' structure database.
#'
#' @name predictParam
#'
#'
#' @param useScoreThreshold `logical` whether to use a soft threshold to be
#'        applied to only compute FingerPrints for promising formula candidates.
#'        Enabling is highly recommended. Default is `TRUE`.
#' @param alwaysPredictHighRefMatches `logical` wether to predict
#'       FingerPrint/Classes&Structure for formulas candidates with reference
#'       spectrum similarity > `Sirius.minReferenceMatchScoreToInject` no
#'       matter which score threshold rules apply. Default is `FALSE`.
#'
#' @importFrom methods setClass new
#' @importClassesFrom ProtGenerics Param
#'
#' @note
#' For more information, see the Sirius [documentation](https://v6.docs.sirius-ms.io/methods-background).
#'
#' @references reference
#'
#' @examples
#' # Example of setting up the parameters for the prediction of molecular
#' # fingerprints and compound class
#' param <- predictParam()
#'
NULL

#' @noRd
setClass("predictParam",
         slots =
             c(useScoreThreshold = "logical",
             alwaysPredictHighRefMatches = "logical"),
         contains = "Param",
         prototype = prototype(useScoreThreshold = TRUE,
                               alwaysPredictHighRefMatches = FALSE)
         )

#' @rdname predictParam
#' @export
predictParam <- function(useScoreThreshold = TRUE,
                         alwaysPredictHighRefMatches = FALSE) {
    new("predictParam",
        useScoreThreshold = useScoreThreshold,
        alwaysPredictHighRefMatches = alwaysPredictHighRefMatches)
}

