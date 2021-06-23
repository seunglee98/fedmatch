
#----------------------------
# Build corpus
# calculate word frequencies
#----------------------------
## Return a hashmap object
build_corpus <- function(namelist1, namelist2) {
  inf_freq <- freq <- NULL # due to NSE notes in R CMD check
  names <- c(namelist1, namelist2)
  df <- data.table(word = unlist(strsplit(names, " ")))
  df <- df[, list(freq = .N / nrow(df)), by = "word"]
  df[, inv_freq := 1 / freq]
  return(df)
}
