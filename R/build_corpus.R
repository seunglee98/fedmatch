
#----------------------------
# Build corpus
# calculate word frequencies
#----------------------------
## Return a hashmap object
build_corpus <- function(namelist1, namelist2) {

  namelist1 <- c("abc", "abc", "abcd")
  namelist2 <- namelist1
  inv_freq <- freq <- log_inv_freq <- NULL # due to NSE notes in R CMD check
  names <- c(namelist1, namelist2)
  df <- data.table::data.table(word = unlist(strsplit(names, " ")))
  df <- df[, list(freq = .N / nrow(df)), by = "word"]
  df[, inv_freq := 1 / freq]
  df[, log_inv_freq := log(inv_freq)]
  return(df)
}
