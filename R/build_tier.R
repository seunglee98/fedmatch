#' Build settings for a tier
#'
#' `build_tier_settings` is a convenient way to make the proper list for the
#' `tier_list` argument of `tier_match` Each vector in build_score_settings
#' should be the same length, and each position (first, second, third, etc.)
#' corresponds to one variable to score on.
#'
#
#' @param by character string. Variables to merge on (common across data 1 and
#'   data 2). See \code{merge}
#' @param by.x character string. Variable to merge on in data1. See \code{merge}
#' @param by.y character string. Variable to merge on in data2. See \code{merge}
#' @param check_merge logical. Checks that your unique_keys are indeed unique.
#' @param match_type string. If 'exact', match is exact, if 'fuzzy', match is
#'   fuzzy. If 'multivar,' match is multivar-based. See \code{multivar_match},
#' @param fuzzy_settings additional arguments for amatch, to be used if match_type
#'   = 'fuzzy'. Suggested defaults provided. (see amatch, method='jw')
#' @param score_settings list. Score settings for post-hoc matchscores.
#' @param clean.args list. Arguments passed to clean.
#' @param clean Function to clean strings prior to match. see \code{clean_strings}.
#' @param multivar_settings list of settings to go to the multivar match if match_type
#' == 'multivar'. See \code{multivar-match}.
#' @param filter function or numeric. Filters a merged data1-data2 dataset. If a
#'   function, should take in a data.frame (data1 and data2 merged by name1 and
#'   name2) and spit out a trimmed verion of the data.frame (fewer rows). Think
#'   of this function as applying other conditions to matches, other than a
#'   match by name. The first argument of filter should be the data.frame. If
#'   numeric, will drop all observations with a matchscore lower than or equal
#'   to filter.
#' @param filter.args list. Arguments passed to filter, if a function
#' @param evaluate Function to evalute merge_plus output.
#' @param evaluate.args ist. Arguments passed to evaluate
#' @param allow.cartesian whether or not to allow many-many matches, see data.table::merge()
#'
#' @export

build_tier <- function(by.x = NULL,
                       by.y = NULL,
                       check_merge = NULL,
                       match_type = NULL,
                       fuzzy_settings = build_fuzzy_settings(),
                       score_settings = NULL,
                       filter = NULL,
                       filter.args = NULL,
                       evaluate = NULL,
                       evaluate.args = NULL,
                       clean.args = NULL,
                       clean = NULL,
                       allow.cartesian = F,
                       multivar_settings = build_multivar_settings()) {
  final_list <- list(
    by.x = by.x,
    by.y = by.y,
    check_merge = check_merge,
    match_type = match_type,
    fuzzy_settings = fuzzy_settings,
    score_settings = score_settings,
    filter = filter,
    filter.args = filter.args,
    evaluate = evaluate,
    evaluate.args = evaluate.args,
    clean.args = clean.args,
    clean = clean,
    allow.cartesian = allow.cartesian,
    multivar_settings = multivar_settings
  )

  return(final_list)
}
