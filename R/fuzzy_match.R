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

  # FOR TESTING
  # data1 <- data.table(
  #   name = c("abaf", "abe", "abgegh"),
  #   id1 = c(1:3)
  # )
  # data2 <- data.table(
  #   name = c("abc"),
  #   id2 = c(1)
  # )
  # by <- NULL
  # by.x <- "name"
  # by.y <- "name"
  # suffixes <- c("_1", "_2")
  # unique_key_1 <- "id1"
  # unique_key_2 <- "id2"
  # fuzzy_settings <- list(
  #   method = "jw", p = 0.1, maxDist = 0.9,
  #   matchNA = FALSE, nthread = getOption("sd_num_thread")
  # )
  # FOR TESTING
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
  if (any(data1[[by.x]] %in% data2[[by.y]])) {
    message("Fuzzy matching despite exact matches in the data. Consider removing these exact matches to dramatically speed up the matching process and improve consistency.")
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

    # jaccard_index <- jaccard_index[which(jaccard_sim >= 1 - fuzzy_settings[["maxDist"]])]
    jaccard_sim <- jaccard_sim[which(jaccard_sim >= 1 - fuzzy_settings[["maxDist"]])]


    # this is done in C, so add 1 to the index to make it match
    match_indices <- jaccard_index + 1
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

  # data1[, c(by.x) := NULL]
  # data2[, c(by.y) := NULL]

  first_merge <- merge(data1[, !c(by.x), with = F], matched_and_ids, by = unique_key_1)

  final_merge <- merge(first_merge, data2[, !c(by.y), with = F], by = unique_key_2, suffixes = suffixes)
  # dedupe
  keys_to_calc <- final_merge[, .N, c(unique_key_2)][N > 1][[unique_key_2]]
  # do we need to dedupe?
  if (length(keys_to_calc) != 0) {
    # include suffixes
    if (by.x == by.y) {
      if (fuzzy_settings[["method"]] != "wgt_jaccard") {
        setkeyv(final_merge, c(unique_key_2))
        no_compare <- final_merge[!data.table(keys_to_calc)]
        to_compare <- final_merge[data.table(keys_to_calc)]
        to_compare[, sim := 1 - do.call(
          stringdist::stringdist,
          c(
            list(to_compare[[paste0(by.x, suffixes[[1]])]]),
            list(to_compare[[paste0(by.y, suffixes[[2]])]]),
            fuzzy_settings[c("method", "p")]
          )
        )]
        to_compare <- to_compare[order(-sim), .SD[1], c(unique_key_2)]
        to_compare[, sim := NULL]
        final_merge <- rbind(to_compare, no_compare)
      } else {
        setkeyv(final_merge, c(unique_key_2))
        no_compare <- final_merge[!data.table(keys_to_calc)]
        to_compare <- final_merge[data.table(keys_to_calc)]
        rm(final_merge)
        corpus <- build_corpus(data1[[by.x]], data2[[by.y]])
        to_compare[, sim := 1 - wgt_jaccard_distance(to_compare[[paste0(by.x, suffixes[[1]])]],
          to_compare[[paste0(by.y, suffixes[[2]])]],
          corpus = corpus, nthreads = fuzzy_settings$nthread
        )]

        to_compare <- to_compare[order(-sim), .SD[1], c(unique_key_2)]
        to_compare[, sim := NULL]
        final_merge <- rbind(to_compare, no_compare)
      }
    } else if (by.x != by.y) {
      # no suffixes needed
      keys_to_calc <- final_merge[, .N, c(unique_key_2)][N > 1][[unique_key_2]]
      if (fuzzy_settings[["method"]] != "wgt_jaccard") {
        setkeyv(final_merge, c(unique_key_2))
        no_compare <- final_merge[!data.table(keys_to_calc)]
        to_compare <- final_merge[data.table(keys_to_calc)]
        to_compare[, sim := 1 - do.call(
          stringdist::stringdist,
          c(
            list(to_compare[[by.x]]),
            list(to_compare[[by.y]]),
            fuzzy_settings[c("method", "p")]
          )
        )]
        to_compare <- to_compare[order(-sim), .SD[1], c(unique_key_2)]
        to_compare[, sim := NULL]
        final_merge <- rbind(to_compare, no_compare)
      } else {
        setkeyv(final_merge, c(unique_key_2))
        no_compare <- final_merge[!data.table(keys_to_calc)]
        to_compare <- final_merge[data.table(keys_to_calc)]
        corpus <- build_corpus(data1[[by.x]], data2[[by.y]])
        to_compare[, sim := 1 - wgt_jaccard_distance(to_compare[[by.x]],
          to_compare[[by.y]],
          corpus = corpus, nthreads = fuzzy_settings$nthread
        )]
        to_compare <- to_compare[order(-sim), .SD[1], c(unique_key_2)]
        to_compare[, sim := NULL]
        final_merge <- rbind(to_compare, no_compare)
      }
    }
  }
  return(final_merge)
}
