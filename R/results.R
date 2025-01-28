#' @title Get results from Sirius
#'
#' @name results
#'
#' @description
#' The function below are used to fetch results from the *Sirius* project.
#'
#'
#' @param sirius a `Sirius` object
#'
#' @param deNovo `logical(1)` specifying if the summary results should be fetched
#'        for the de novo candidates. Defaults to `FALSE`.
#'
#' @param features `character()` vector specifying the feature IDs to retrieve
#'       results for. If not provided, all available features will be used.
#'
#' @param return.type `character(1)` specifying the return type. Either
#'        `"data.frame"`, which will return a single `data.frame` with all
#'        results. Or `"list"`, which will return a `list` of `data.frame`
#'        where each element corresponds to the results for a single feature.
#'        Defaults to `"data.frame"`.
#'
#' @param topFormula `numeric(1)` specifying the maximum number of formula
#'       candidates to retrieve for each feature.
#'
#' @param topStructure `numeric(1)` specifying the maximum number of structure
#'      candidates to retrieve for each formula.
#'
#' @param result.type `character(1)` specifying the type of results to retrieve.
#'        Possible values are "formulaId", "structureDb", "compoundClass",
#'        and "deNovo". Defaults to "formulaId". All the possible values will
#'        return  formulaId results anyway.
#'
#'
NULL

#' @rdname results
#' @return a data.frame with the summary of the results.
#'         Important column is the *ApproximateConfidence* column, which give a
#'         score of how possible all the identifications for this feature are.
#'         The *exactConfidence* column is a score of how possible the top
#'         identification is.
#' @export
#' @importFrom dplyr bind_rows
summary <- function(sirius, deNovo = FALSE) {
    if (!deNovo) .fetch_aligned_features(sirius, opt_fields = c("topAnnotations"))
    else  .fetch_aligned_features(sirius, opt_fields = c("topAnnotationsDeNovo"))
}

#Helper for summary
# Generic fetcher for aligned features
.fetch_aligned_features <- function(sirius, opt_fields) {
    res <- sirius@api$features_api$GetAlignedFeatures(sirius@projectId,
                                                      opt_fields = opt_fields)
    flattened <- lapply(res, function(x) as.data.frame(.flatten_list(x$toSimpleType()),
                                                       stringsAsFactors = FALSE))
    return(dplyr::bind_rows(flattened))
}

# Flatten JSON list
.flatten_list <- function(lst) {
    flat <- list()
    for (key in names(lst)) {
        if (is.list(lst[[key]])) {
            flat <- c(flat, .flatten_list(lst[[key]]))
        } else {
            flat[[key]] <- lst[[key]]
        }
    }
    return(flat)
}

#' @rdname results
#' @export
results <- function(sirius,
                    features = character(),
                    result.type = c("formulaId", "structureDb",
                                    "compoundClass", "deNovo"),
                    return.type = c("list", "data.frame"),
                    topFormula = 5,
                    topStructure = 5) {
    result.type <- match.arg(result.type)
    return.type <- match.arg(return.type)
    process_function <- switch(result.type,
                               formulaId = function(feature_id) {
                                   .process_single_feature(sirius, feature_id,
                                                           top = topFormula)
                               },
                               structureDb = function(feature_id) {
                                   form_res <- .process_single_feature(sirius,
                                                                       feature_id,
                                                                       top = topFormula)
                                   .structure_for_one_feature(sirius, feature_id,
                                                              form_res, topStructure)
                               },
                               compoundClass = function(feature_id) {
                                   form_res <- .process_single_feature(sirius,
                                                                       feature_id,
                                                                       top = topFormula)
                                   .compound_for_one_feature(sirius,
                                                             feature_id,
                                                             form_res)
                               },
                               deNovo = function(feature_id) {
                                   form_res <- .process_single_feature(sirius,
                                                                       feature_id,
                                                                       top = topFormula)
                                   .de_novo_for_one_feature(sirius,
                                                            feature_id,
                                                            form_res,
                                                            topStructure)
                               },
                               stop("Invalid result type specified."))
    .process_features(sirius, features, process_function, return.type)
}



# Shared low-level operations
## Process a list of features
.process_features <- function(sirius, features, process_fn, return.type = "list") {
    if (!length(features)) {
        features <- featuresId(sirius)
        if (length(features) == 0) stop("No available features to process.")
    }
    results <- lapply(features, process_fn)
    names(results) <- features
    if (return.type == "list") return(results)
    return(dplyr::bind_rows(lapply(names(results), function(x) {
        results[[x]]$sirius_fts <- x
        results[[x]]
    })))
}

.process_single_feature <- function(sirius, feature_id, top = integer()) {
    candidates <- sirius@api$features_api$GetFormulaCandidates(
        project_id = sirius@projectId,
        aligned_feature_id = feature_id
    )
    if (length(candidates) > top) candidates <- candidates[1:top]
    df <- lapply(candidates, function(c) as.data.frame(c$toSimpleType()))
    results <- if (length(df)) dplyr::bind_rows(df) else data.frame()
    results$xcms_fts <- .map_sirius_to_xcms(sirius, feature_id)
    return(results)
}

.structure_for_one_feature <- function(sirius, fts_id, formula_res, top) {
    all_forms <- lapply(formula_res$formulaId, function(fid) {
        .structure_for_one_formula(sirius, fts_id, fid, top)
    })
    all_forms <- lapply(all_forms, .standardize_columns)
    merged <- merge(formula_res,
                    dplyr::bind_rows(all_forms),
                    by = "formulaId",
                    all.x = TRUE)
    return(merged)
}

.compound_for_one_feature <- function(sirius, fts_id, formula_res) {
    all_forms <- lapply(formula_res$formulaId, function(fid) {
        .compound_for_one_formula(sirius, fts_id, fid)
    })
    merged <- merge(formula_res,
                    dplyr::bind_rows(all_forms),
                    by = "formulaId", all.x = TRUE)
    return(merged)
}

.de_novo_for_one_feature <- function(sirius, fts_id, formula_res, top) {
    all_forms <- lapply(formula_res$formulaId, function(fid) {
        .de_novo_for_one_formula(sirius, fts_id, fid, top)
    })
    all_forms <- lapply(all_forms, .standardize_columns)
    merged <- merge(formula_res, dplyr::bind_rows(all_forms), by = "formulaId", all.x = TRUE)
    return(merged)
}

# Shared Methods for Specific Data Types
.structure_for_one_formula <- function(sirius, fts_id, formula_id, top) {
    structures <- sirius@api$features_api$GetStructureCandidatesByFormula(
        sirius@projectId, fts_id, formula_id, opt_fields = c("libraryMatches")
    )
    candidates <- lapply(structures, function(x) x$toSimpleType())
    if (length(candidates) > top) candidates <- candidates[1:top]
    df <- dplyr::bind_rows(lapply(candidates, as.data.frame))
    df$formulaId <- formula_id
    return(df)
}

.de_novo_for_one_formula <- function(sirius, fts_id, formula_id, top) {
    de_novo_structures <- sirius@api$features_api$GetDeNovoStructureCandidatesByFormula(
        sirius@projectId, fts_id, formula_id, opt_fields = c("libraryMatches")
    )
    candidates <- lapply(de_novo_structures, function(x) x$toSimpleType())
    if (length(candidates) > top) candidates <- candidates[1:top]
    df <- dplyr::bind_rows(lapply(candidates, as.data.frame))
    df$formulaId <- formula_id
    return(df)
}

.compound_for_one_formula <- function(sirius, fts_id, formula_id) {
    tryCatch({
        compound <- sirius@api$features_api$GetBestMatchingCompoundClasses(
              sirius@projectId, fts_id, formula_id
        )
        df <- .process_compound_classes(compound$toSimpleType())
        df$formulaId <- formula_id
        return(df)
    }, error = function(e) {
        warning(paste("Formula:", formula_id, "for feature:", fts_id, "does ",
                      "not have compound classes information."))
        return(data.frame(formulaId = formula_id, stringsAsFactors = FALSE))
    })
}

.process_compound_classes <- function(parsed_json) {
    # Process NPC data
    npc_df <- dplyr::bind_rows(lapply(names(parsed_json)[1:3], function(section) {
        cbind(section = section, as.data.frame(parsed_json[[section]], stringsAsFactors = FALSE))
    }))
    # Process ClassyFire lineage and alternatives
    cf_lineage_df <- .convert_to_dataframe(parsed_json$classyFireLineage, "classyFireLineage")
    cf_alternatives_df <- .convert_to_dataframe(parsed_json$classyFireAlternatives, "classyFireAlternatives")
    cf_df <- dplyr::bind_rows(cf_lineage_df, cf_alternatives_df)
    if ("level" %in% colnames(cf_df)) {
        cf_df$level <- factor(cf_df$level, levels = c("Kingdom", "Superclass", "Class", "Subclass"))
    }
    final_df <- dplyr::bind_rows(npc_df, cf_df)
    return(final_df)
}

.convert_to_dataframe <- function(input_list, section_name) {
    if (is.null(input_list)) return(NULL)
    df <- bind_rows(input_list)
    df$section <- section_name
    return(df)
}

.force_classes_structure <- c(
    inchiKey = "character",
    smiles = "character",
    structureName = "character",
    xlogP = "double",
    mcesDistToTopHit = "double",
    csiScore = "double"
)

#' @importFrom methods as
#' @noRd
.standardize_columns <- function(df) {
    for (col in names(.force_classes_structure)) {
        if (col %in% colnames(df)) {
            df[[col]] <- as(df[[col]], .force_classes_structure[col])
        }
    }
    return(df)
}

.map_sirius_to_xcms <- function(sirius, sirius_id) {
    if (nrow(sirius@featureMap) == 0)
        sirius@featureMap <- mapFeatures(sirius)
    matching_xcms <- sirius@featureMap$fts_xcms[sirius_id == sirius@featureMap$fts_sirius]
    if (length(matching_xcms) == 0) warning("No matching XCMS ID found for the given Sirius ID: ", sirius_id)
    return(matching_xcms)
}
