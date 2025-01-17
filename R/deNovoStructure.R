#' @title de novo structure annotation
#'
#' @name deNovoStructureParam
#'
#' @description
#' This function to set up the parameter for de novo structure annotation using
#' the *MSNovelist* tool.
#'
#' *MSNovelist* generates molecular structures de novo from MS/MS data -
#' without relying on any database. This makes it particularly useful for
#' analyzing poorly represented analyte classes and novel compounds, where
#' traditional database searches may fall short. However, it is not intended
#' to replace database searches altogether, as structural elucidation of small
#' molecules from MS/MS data remains a challenging task, and identifying a
#' structure without database candidates is even more difficult.
#'
#' @param numberOfCandidateToPredict `numeric`, number of structure candidates
#'        to be predicted by MsNovelist. Max Value `128`. Actual number of
#'        returned candidate might be lower du to duplicates being created.
#'        Default is `10`
#'
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
#' # Example of setting up the parameters for de novo structure annotation
#' param <- deNovoStructureParam(numberOfCandidateToPredict = 10)
#'
NULL

#' @noRd
setClass("deNovoStructureParam",
         slots = c(numberOfCandidateToPredict = "numeric"),
         contains = "Param",
         prototype = prototype(c(numberOfCandidateToPredict = 10)),
         validity = function(object) {
           if (object@numberOfCandidateToPredict < 1 || object@numberOfCandidateToPredict > 128)
             stop("numberOfCandidateToPredict must be between 1 and 128.")
           TRUE
         }
)

#' @rdname deNovoStructureParam
#' @export
deNovoStructureParam <- function(numberOfCandidateToPredict = 10) {
  new("deNovoStructureParam",
      numberOfCandidateToPredict = numberOfCandidateToPredict)
}
