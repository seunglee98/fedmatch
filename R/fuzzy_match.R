#' Use string distances to match on names
#'
#' Use the \code{stringdist} package to perform a fuzzy match on two datasets.
#'
#' \code{stringdist} \code{amatch} computes string distances between every
#' pair of strings in two vectors, then picks the closest string pair for each
#' observation in the dataset. This is used by \code{fuzzy_match} to perform
#' a string distance-based match between two datasets. This process can take quite a long time,
#' for quicker matches try adjusting the \code{nthread} argument in \code{fuzzy_settings}.
#' The default fuzzy_settings are sensible starting points for company name matching,
#' but adjusting these can greatly change how the match performs.
#'
#' @param data1 data.frame. First to-merge dataset.
#' @param data2 data.frame. Second to-merge dataset.
#' @param by character string. Variables to merge on (common across data 1 and data 2). See \code{merge}
#' @param by.x character string. Variable to merge on in data1. See \code{merge}
#' @param by.y character string. Variable to merge on in data2. See \code{merge}
#' @param suffixes character vector with length==2. Suffix to add to like named variables after the merge. See \code{merge}
#' @param unique_key_1 character vector. Primary key of data1 that uniquely identifies each row (can be multiple fields)
#' @param unique_key_2 character vector. Primary key of data2 that uniquely identifies each row (can be multiple fields)
#' @param fuzzy_settings list of arguments to pass to to the fuzzy matching function. See \code{amatch}.
#' @return a data.table, the resultant merged data set, including all columns from both data sets.
#' @import data.table

fuzzy_match <- function(data1,
                        data2,
                        by = NULL,
                        by.x = NULL,
                        by.y = NULL,
                        suffixes,
                        unique_key_1,
                        unique_key_2,
                        fuzzy_settings = list(
                          method = "jw", p = 0.1, maxDist = 0.05,
                          matchNA = FALSE, nthread = getOption("sd_num_thread")
                        )) {
  # check that id's aren't the same
  if (unique_key_1 == unique_key_2) {
    stop("Error: unique_key_1 must not equal unique_key_2")
  }
  if (is.null(by) & (is.null(by.x) | is.null(by.y))) {
    stop("Error: either 'by' or both of 'by.x' and 'by.y' must not be NULL")
  }
  if (!is.null(by) & (!is.null(by.x) | !is.null(by.y))) {
    stop("both 'by' and ('by.x' or 'by.y') were supplied, this is not allowed")
  }
  if (!is.null(by)) {
    by.x <- by
    by.y <- by
  }

  # make sure data1 and data2 are data.table's and not data.frames
  data1 <- data.table(data1)
  data2 <- data.table(data2)
  # the amatch options
  if (fuzzy_settings[["method"]] %in% c(
    "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine",
    "jaccard", "jw", "soundex"
  )) {
    match_indices <- do.call(stringdist::amatch, c(list(data1[[by.x]], data2[[by.y]]), fuzzy_settings))
    # our special weighted jaccard method
  } else if (fuzzy_settings[["method"]] == "wgt_jaccard") {
    corpus <- build_corpus(data1[[by.x]], data2[[by.y]])

    jaccard_result <- wgt_jaccard(
      corpus$word, corpus$log_inv_freq,
      data1[[by.x]], data2[[by.y]], fuzzy_settings[["nthread"]]
    )
    # filter out matches that are under the minimum threshold
    jaccard_index <- jaccard_result$index
    jaccard_sim <- jaccard_result$similarity
    jaccard_index[which(jaccard_sim < 1 - fuzzy_settings[["maxDist"]])] <- NA
    # print(jaccard_index[!is.na(jaccard_index)][1:10])
    # jaccard_index <- jaccard_index[which(jaccard_sim >= 1 - fuzzy_settings[["maxDist"]])]
    # print(all(is.na(jaccard_sim)))
    # print(quantile(jaccard_sim, c(.01, .5, .99)))
    jaccard_sim <- jaccard_sim[which(jaccard_sim >= 1 - fuzzy_settings[["maxDist"]])]


    # this is done in C, so add 1 to the index to make it match
    match_indices <- jaccard_index + 1
    # print(match_indices)
    score_table <- data.table(wgt_jaccard_sim = jaccard_sim)
  } else {
    stop("Fuzzy 'method' must be one of those in stringdist::amatch's 'method' argument, or 'wgt_jaccard'.")
  }

  # the x-indices which are matched to y
  x_index <- seq(1, length(match_indices))[!is.na(match_indices)]
  y_index <- match_indices[!is.na(match_indices)]

  rows_from_1 <- copy(data1[x_index, c(by.x, unique_key_1), with = F])
  rows_from_2 <- copy(data2[y_index, c(by.y, unique_key_2), with = F])

  if (by.x == by.y) {
    setnames(rows_from_1, c(paste0(by.x, suffixes[[1]]), unique_key_1))
    setnames(rows_from_2, c(paste0(by.x, suffixes[[2]]), unique_key_2))
  } else {
    setnames(rows_from_1, c(by.x, unique_key_1))
    setnames(rows_from_2, c(by.y, unique_key_2))
  }
  matched_and_ids <- cbind(rows_from_1, rows_from_2)
  if (fuzzy_settings[["method"]] == "wgt_jaccard") {
    matched_and_ids <- cbind(matched_and_ids, score_table)
  }

  data1[, c(by.x) := NULL]
  data2[, c(by.y) := NULL]

  first_merge <- merge(data1, matched_and_ids, by = unique_key_1)

  final_merge <- merge(first_merge, data2, by = unique_key_2, suffixes = suffixes)

  return(final_merge)
}
