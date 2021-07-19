#' Build settings for multivar matching
#'
#' `build_multivar_settings` is a convenient way to build the list for the multivar settings argument in merge_plus
#'
#' @param logit a glm or lm model as a result from a logit regression on a verified dataset. See details.
#' @param missing boolean T/F, whether or not to treat missing (NA) observations as its own binary column for each column in by. See details.
#' @param wgts rather than a lm model, you can supply weights to calculate matchscore. Can be weights from \code{calculate_weights}.
#' @param compare_type a vector with the same length as "by" that describes how to compare the variables. Options are "in", "indicator", "substr", "difference", "ratio", and "stringdist". See X for details.
#' @param blocks variable present in both data sets to "block" on before computing scores. Matchscores will only be computed for observations that share a block. See details.
#' @param blocks.x name of blocking variables in x. cannot supply both blocks and blocks.x
#' @param blocks.y name of blocking variables in y. cannot supply both blocks and blocks.y
#' @param top integer. Number of matches to return for each observation.
#' @param threshold numeric. Minimum score for a match to be included in the result.
#' @param nthread integer. Number of cores to use when computing all combinations. See \code{parallel::makecluster()}
#' @return a list containing options for the 'multivar_settings' argument of \code{merge_plus}.
#' @export

build_multivar_settings <- function(logit = NULL, missing = FALSE, wgts = NULL,
                                    compare_type = "diff", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
                                    top = 1, threshold = NULL, nthread = 1) {
  final_list <- list(
    logit = logit,
    missing = missing,
    wgts = wgts,
    compare_type = compare_type,
    blocks = blocks,
    blocks.x = blocks.x,
    blocks.y = blocks.y,
    top = top,
    threshold = threshold,
    nthread = nthread
  )
}
