#' Computing Weighted Jaccard Distance
#'
#' #' \code{wgt_jaccard_distance} computes the Weighted Jaccard Distance between
#' two strings. It is vectorized, and accepts only two equal-length string
#' vectors.
#'
#' See the vignette `fuzzy_matching` for details on how the Weighted Jaccard similarity is computed.
#'
#' @param string_1 character vector
#' @param string_2 character vector
#' @param nthreads number of threads to use in the underlying C++ code
#' @param corpus corpus data.table, constructed with
#'   \code{fedmatch::build_corpus}
#'
#' @return numeric vector with the Weighted Jaccard distances for each element
#'   of string_1 and string_2.
#'
#' @export

wgt_jaccard_distance <- function(string_1,
                                 string_2,
                                 corpus,
                                 nthreads = 1
                                 ) {
  if (is.null(corpus$word) |  is.null(corpus$freq) | is.null(corpus$inv_freq)) {
    stop("'corpus' must have 'word', 'freq', and 'inv_freq' columns. Use build_corpus to construct it.")
  }
  split_strings_1 <- stringr::str_split(string_1, "\\s+")
  split_strings_2 <- stringr::str_split(string_2, "\\s+")
  split_strings <- c(unlist(split_strings_1),
                     unlist(split_strings_2)) %>%
    unique()
  if (any(!split_strings %in% corpus$word )) {
    stop("All elements in string_1 and string_2 must be present in the corpus 'word' column. ")
  }
  if (!is.character(string_1) | !is.character(string_2)) {
    stop("string_1 and string_2 must be character vectors.")
  }
  if (length(string_1) != length(string_2) | length(string_1) == 0 | length(string_2) == 0) {
    stop("string_1 and string_2 must be the same non-zero length.")
  }

  scores <- 1- wgt_jaccard_single(corpus$word,
                               corpus$inv_freq,
                               string_1,
                               string_2,
                               nthreads)$similarity
  return(scores)

}
