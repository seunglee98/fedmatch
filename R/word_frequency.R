#' Compute frequency of words in a corpus
#'
#' \code{word_frequency} counts the frequency of words in a set of strings.
#' Also does minimal cleaning (removes punctuation and extra spaces). Useful for
#' determining what words are common and may need to be replaced or removed with
#' \code{clean_strings}.
#'
#' @param string character vector
#'
#' @return data frame with word frequency
#' @export
#'

word_frequency <- function(string) {

  # remove multiple spaces and all punctuation
  clean_str <- gsub("[[:punct:]]", " ", tolower(string))
  clean_str <- gsub("[[:space:]]", " ", clean_str)
  clean_str <- gsub("\\s+", " ", clean_str)

  # create an empty vector
  word_list <- c()

  # add all the words from each string
  for (i in 1:length(clean_str)) {
    words <- strsplit(clean_str[i], "\\s+")
    word_list <- c(word_list, words, recursive = TRUE)
  }

  # count the frequency of the words
  word_freq <- data.frame(table(word_list)) # count words
  word_freq <- word_freq[order(-word_freq$Freq), ] # order by word count
  names(word_freq) <- c("word", "count")
  return(word_freq)
}
