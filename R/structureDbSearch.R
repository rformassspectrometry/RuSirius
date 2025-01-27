#' @title Structure Database Search
#'
#' @name structureDbSearchParam
#'
#' @description
#' This function creates an object of class `structureDbSearchParam` that can
#' be used to configure the structure database search in Sirius. The object can
#' be passed to the `runSirius` function to perform the structure database
#' search.
#'
#' By default, *SIRIUS* searches for molecular structures in a biomolecule
#' structure database. It can also search in the (extremely large) PubChem
#' database or in custom “suspect databases” provided by the user.
#'
#' @param structureSearchDbs `character` vector, specifying the id of the
#'        databases to search for structures. Default is `c("BIO", "massbank")`.
#'        If expansive search is enabled this database selection will be
#'        expanded to PubChem if not high confidence hit was found in the
#'        selected databases. The available databases can be found by running
#'        `listDatabases()`.
#'
#' @param tagStructuresWithLipidClass `logical`, specifying whether to tag
#'        structures with lipid class estimated by El Gordo. The lipid class
#'        will only be available if El Gordo predicts that the MS/MS  is a
#'        lipid spectrum. If this parameter is set to `FALSE` El Gordo will
#'        still be executed and e.g. improve the fragmentation tree, but the
#'        matching structure candidates will not be tagged if they match lipid
#'        class.
#'
#' @param expansiveSearchConfidenceMode `character(1)` , specifying the
#'        confidence mode for expansive search. Possible values are
#'        `"APPROXIMATE"`,`"EXACT"`, `"OFF"`. Defaults to `"APPROXIMATE"`.
#'
#' @importFrom methods setClass new
#' @importClassesFrom ProtGenerics Param
#'
#' @return An object of class `structureDbSearchParam`.
#'
#' @note
#' For more information, see the Sirius
#' [documentation](https://v6.docs.sirius-ms.io/methods-background).
#'
#' @references reference
#'
#' @examples
#' # Example of setting up the parameters for structure database search
#' param <- structureDbSearchParam(
#'                  structureSearchDbs = c("BIO", "massbank"),
#'                  tagStructuresWithLipidClass = TRUE,
#'                  expansiveSearchConfidenceMode = "APPROXIMATE")
#'
#'
NULL

#' @noRd
setClass("structureDbSearchParam", slots = c(
    structureSearchDbs = "character",
    tagStructuresWithLipidClass = "logical",
    expansiveSearchConfidenceMode = "character"),
    contains = "Param",
    prototype = prototype(
        structureSearchDbs = c("BIO", "massbank"),
        tagStructuresWithLipidClass = FALSE,
        expansiveSearchConfidenceMode = c("APPROXIMATE","EXACT")),
    validity = function(object) {

    })

#' @rdname structureDbSearchParam
#' @export
structureDbSearchParam <- function(
        structureSearchDbs = c("BIO", "massbank"),
        tagStructuresWithLipidClass = TRUE,
        expansiveSearchConfidenceMode = c("APPROXIMATE", "EXACT", "OFF")
    ) {
    expansiveSearchConfidenceMode <- match.arg(expansiveSearchConfidenceMode)
    new("structureDbSearchParam",
        structureSearchDbs = structureSearchDbs,
        tagStructuresWithLipidClass = tagStructuresWithLipidClass,
        expansiveSearchConfidenceMode = expansiveSearchConfidenceMode)
}

