#' String cleaning for easier matching
#'
#' \code{clean_strings} takes a string vector and cleans it according to user-given options.
#'
#' This function takes a variety of options, each of which changes the behavior.
#' Without the default settings, \code{clean_strings} will do the following:
#' make the string lowercase; replace special characters &, $, \%, @ , with their
#' names ("and", "dollar", "percent", "at"); convert tabs to spaces and removes extra spaces.
#' This default cleaning puts the strings in a standard format to allow for easier matching.
#'
#' The other options allow for the removal or replacement of other words or characters.
#' @param string character or character vector of strings
#' @param sp_char_words character vector. Data.frame where first column is special characters and second column is full words. The default is
#' @param common_words data.frame. Data.frame where first column is abbreviations and second column is full words.
#' @param remove_char character vector. string of specific characters (for example, "letters") to be removed
#' @param remove_words logical. If TRUE, removes all abbreviations and replacement words in common_words
#' @param stem logical. If TRUE, words are stemmed
#' @param replace_null character vector. If not NULL, the value with which to replace empty or blank strings.
#' @return cleaned strings
#'
#' @export

clean_strings <- function(string,
                          sp_char_words = data.table::data.table(
                            "character" = c("\\&", "\\$", "\\%", "\\@"),
                            "replacement" = c("and", "dollar", "percent", "at")
                          ),
                          common_words = NULL,
                          remove_char = NULL, remove_words = FALSE, stem = FALSE, replace_null = NULL) {
  string_table <- data.table::data.table(string_orig = string)
  # define the default inputs;
  ## special character to words default
  if (is.null(sp_char_words)) {
    sp_char_words <- data.table::data.table("character" = c("\\&", "\\$", "\\%", "\\@"), "replacement" = c("and", "dollar", "percent", "at"))
  } else {
    sp_char_words <- data.table::data.table(sp_char_words)
  }
  replacement <- NULL # due to NSE notes in R CMD check
  sp_char_words_vec <- sp_char_words[, replacement]
  sp_char_words_vec <- stats::setNames(sp_char_words_vec, sp_char_words[, character])

  ## if remove_words is TRUE, set replacement for common words to '' instead of using replacement word
  if (!is.null(common_words)) {

    ## convert to data.table
    if (class(common_words)[1] != "data.table") {
      common_words <- data.table::data.table(common_words)
      if (ncol(common_words) == 1) {
        if (remove_words == F) {
          stop("Error: must supply words to replace if remove_words = F")
        } else if (remove_words == TRUE) {
          common_words[, replacement := ""]
        }
      }
    }
    # set the names of the common words table
    setnames(common_words, c("words", "replacement"))
    if (remove_words == TRUE) {
      common_words[, replacement := ""]
    }
    if (remove_words != TRUE & dim(common_words)[2] < 2) {
      print("Common word dimension too small, add replacement column.")
    }
    # make common words into a named vector for easy replacement
    replacement <- words <- . <- NULL # due to NSE notes in R CMD check
    common_words_vec <- common_words[, replacement] %>%
      stringr::str_to_lower()
    common_words_vec <- stats::setNames(common_words_vec, common_words[, words] %>%
      stringr::str_to_lower() %>%
      stringr::str_c("\\b", ., "\\b"))
  }

  # basic cleaning 1;
  ## lowcase the strings
  clean_str <- sting_orig <- NULL # due to NSE notes in R CMD check
  string_table[, clean_str := stringr::str_to_lower(string_orig)]

  ## replace commonly abbreviated words
  if (!is.null(common_words)) {

    ### replace words with named vector
    string_table[, clean_str := stringr::str_replace_all(clean_str, common_words_vec)]
  }
  ## drop specific characters
  if (!is.null(remove_char)) {
    remove_char_regex <- paste0("(", paste(remove_char, collapse = "|"), ")")
    string_table[, clean_str := stringr::str_replace_all(clean_str, remove_char_regex, " ")]
  }
  ## replace special characters with words
  string_table[, clean_str := stringr::str_replace_all(clean_str, sp_char_words_vec)]

  ## replace remaining special characters with no spaces
  # string_table[, clean_str := str_replace_all(clean_str, "[:punct:]", " ")]
  string_table[, clean_str := stringr::str_replace_all(clean_str, stringr::regex("[^a-zA-Z0-9 ]"), " ")]



  # basic cleaning 2;

  ## reduce all spaces to single spaces;
  string_table[, clean_str := stringr::str_replace_all(clean_str, "[:space:]", " ")]

  ## remove leading and trailing blanks, along with newlines
  string_table[, clean_str := stringr::str_squish(clean_str)]

  # stemming
  if (stem == TRUE) {
    string_table[, clean_str := SnowballC::wordStem(clean_str, "english")]
  }
  # replace NULL strings;

  if (!is.null(replace_null)) {
    string_table[!is.na(clean_str) & (clean_str %in% c("", " ") | is.null(clean_str)), clean_str := replace_null]
  }

  ## Return a vector
  string <- as.character(string_table[, clean_str])

  return(string)
}
