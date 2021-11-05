#' Calculate word corpus for weighted jaccard matching
#'
#' @param namelist1 character vector of names from dataset 1
#' @param namelist2 character vector of names from dataset 2
#' @return a data.table with columns for frequency, inverse frequency, and log inverse frequency for each word in the two strings.

#' @export
build_corpus <- function(namelist1, namelist2) {
  inv_freq <- freq <- log_inv_freq <- NULL # due to NSE notes in R CMD check
  names <- c(namelist1, namelist2)
  df <- data.table::data.table(word = unlist(strsplit(names, " ")))
  df <- df[, list(freq = .N / nrow(df)), by = "word"]
  df[, inv_freq := 1 / freq]
  df[, log_inv_freq := log(inv_freq)]
  return(df)
}
