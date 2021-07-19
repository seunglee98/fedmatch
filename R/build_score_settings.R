#' Build settings for scoring
#'
#' `build_score_settings` is a convenient way to make the proper list for the
#' `score_settings` argument of `merge_plus` Each vector in build_score_settings
#' should be the same length, and each position (first, second, third, etc.)
#' corresponds to one variable to score on.
#'
#'
#' @param score_var_x character vector. the variables from the 'x' dataset to score on
#' @param score_var_y character vector. the variables from the 'y' dataset to score on
#' @param score_var_both the variables from both datasets (shared names) to score on, before any prefixes are applied.
#' @param wgts numeric vector. The weights for the linear sum of scores
#' @param score_type character vector.
#' @return a list containing options for the 'score_settings' argument of \code{merge_plus}.
#' @export


build_score_settings <- function(score_var_x = NULL,
                                 score_var_y = NULL,
                                 score_var_both = NULL,
                                 wgts = NULL,
                                 score_type) {
  if ((!is.null(score_var_x) | !is.null(score_var_y)) & !is.null(score_var_both)) {
    stop("Only one of 'score_var_x' or 'score_var_both' is required")
  }
  if (is.null(wgts) & !is.null(score_var_x)) {
    wgts <- rep(1 / length(score_var_x))
  } else if (is.null(wgts) & !is.null(score_var_both)) {
    wgts <- rep(1 / length(score_var_both))
  }
  final_list <- list(
    score_var_x = score_var_x,
    score_var_y = score_var_y,
    score_var_both = score_var_both,
    wgts = wgts,
    score_type = score_type
  )
  return(final_list)
}
