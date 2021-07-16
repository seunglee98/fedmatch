#' Compute frequency of words in a corpus
#'
#' \code{word_frequency} counts the frequency of words in a set of strings.
#' Also does minimal cleaning (removes punctuation and extra spaces). Useful for
#' determining what words are common and may need to be replaced or removed with
#' \code{clean_strings}.
#'
#' @param string character vector
#'
#' @return data.table with word frequency
#' @export
#'

word_frequency <- function(string) {
  # remove multiple spaces and all punctuation
  clean_str <- clean_strings(string)
  # add all the words from each string
  split_strings <- stringr::str_split(clean_str, "\\s+")
  split_strings <- unlist(split_strings)
  # count the frequency of the words
  word_freq <- data.table::data.table(table(split_strings)) # count words
  N <- NULL # due to NSE notes in R CMD check
  word_freq <- word_freq[order(-N)] # order by word count
  data.table::setnames(word_freq, c("Word", "Count"))
  return(word_freq)
}
