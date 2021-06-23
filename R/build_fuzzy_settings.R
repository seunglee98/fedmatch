#' Build settings for fuzzy matching
#'
#' `build_fuzzy_settings` is a convenient way to build the list for the fuzzy settings argument in merge_plus
#'
#' @param method character vector of length 1. Either one of the methods listed in stringdist::amatch, or our custom method 'wgt_jaccard.' See the vignettes for more details.
#' @param p numeric vector of length 1. See stringdist::amatch()
#' @param maxDist numeric vector of length 1. See stringdist::amatch()
#' @param nthread number of threads to use in the underlying C code.
#' @param matchNA whether or not to match on NAs, see stringdist::amatch()
#' @export


build_fuzzy_settings <- function(method = "jw",
                                 p = .1,
                                 maxDist = .05,
                                 matchNA = FALSE,
                                 nthread = getOption("sd_num_thread")) {
  final_list <- list(
    method = method,
    p = p,
    maxDist = maxDist,
    matchNA = matchNA,
    nthread = nthread
  )
}
