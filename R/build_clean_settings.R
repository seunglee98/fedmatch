#' Building settings for string cleaning
#'
#' `build_clean_settings` is a convenient way to make the proper list for the
#' `clean_settings` argument of `tier_match`.
#'
#' @param sp_char_words character vector. Data.frame where first column is special characters and second column is full words. The default is
#' @param common_words data.frame. Data.frame where first column is abbreviations and second column is full words.
#' @param remove_char character vector. string of specific characters (for example, "letters") to be removed
#' @param remove_words logical. If TRUE, removes all abbreviations and replacement words in common_words
#' @param stem logical. If TRUE, words are stemmed
#' @return list with settings to pass to \code{clean_strings}
#'
#' @export

build_clean_settings <- function(
                          sp_char_words = fedmatch::sp_char_words,
                          common_words = NULL,
                          remove_char = NULL, remove_words = FALSE,
                          stem = FALSE) {
  final_list <- list(
    sp_char_words = sp_char_words,
    common_words = common_words,
    remove_char = remove_char,
    remove_words = remove_words,
    stem = stem
  )
  return(final_list)
}
