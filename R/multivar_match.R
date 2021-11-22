#' Matching by computing multivar_scores based on several variables
#'
#' \code{multivar_match} computes a multivar_score between each pair of observations between
#' datasets x and y using several variables, then executes a merge by picking the
#' highest multivar_score pair for each observation in x.
#'
#' The best way to understand this function is to see the vignette 'Multivar_matching'.
#'
#' There are two ways of performing this match: either with or without a pre-trained logit.
#' To use a logit, you must have a verified set of matches. The names of the variables
#' in this set must match the names of the variables in the data you pass into \code{multivar_match}.
#' Without a pre-trained logit, you must have a set of weights for each variable that you
#' want in the comparison. These can either be made up ahead of time, or you can
#' use a verified set of matches and \code{calculate_weights}.
#'
#'
#' @param data1 data.frame. First to-merge dataset.
#' @param data2 data.frame. Second to-merge dataset.
#' @param by character string. Variables to merge on (common across data 1 and data 2). See \code{merge}
#' @param by.x character string. Variable to merge on in data1. See \code{merge}
#' @param by.y character string. Variable to merge on in data2. See \code{merge}
#' @param suffixes see \code{merge}
#' @param unique_key_1 character vector. Primary key of data1 that uniquely identifies each row (can be multiple fields)
#' @param unique_key_2 character vector. Primary key of data2 that uniquely identifies each row (can be multiple fields)
#' @param logit a glm or lm model as a result from a logit regression on a verified dataset. See details.
#' @param missing boolean T/F, whether or not to treat missing (NA) observations as its own binary column for each column in by. See details.
#' @param wgts rather than a lm model, you can supply weights to calculate multivar_score. Can be weights from \code{calculate_weights}.
#' @param compare_type a vector with the same length as "by" that describes how to compare the variables. Options are "in", "indicator", "substr", "difference", "ratio", and "stringdist". See X for details.
#' @param blocks variable present in both data sets to "block" on before computing scores. multivar_scores will only be computed for observations that share a block. See details.
#' @param blocks.x name of blocking variables in x. cannot supply both blocks and blocks.x
#' @param blocks.y name of blocking variables in y. cannot supply both blocks and blocks.y
#' @param top integer. Number of matches to return for each observation.
#' @param threshold numeric. Minimum score for a match to be included in the result.
#' @param nthread integer. Number of cores to use when computing all combinations. See \code{parallel::makecluster()}
#' @return a data.table, the resultant match, including columns from both data sets.
multivar_match <- function(data1, data2,
                           by = NULL, by.x = NULL, by.y = NULL,
                           unique_key_1, unique_key_2,
                           logit = NULL, missing = FALSE, wgts = NULL, compare_type = "diff",
                           blocks = NULL, blocks.x = NULL, blocks.y = NULL,
                           nthread = 1,
                           top = 1, threshold = NULL, suffixes = c("_1", "_2")) {
  #----------------------------------
  # check that all inputs are valid
  #----------------------------------
  if (!is.null(by)) {
    by.x <- by
    by.y <- by
  }
  ## check that comparison vectors are similarly sized
  if (length(by.x) != length(by.y)) {
    stop("Variable comparison lengths do not match!")
  }

  ## check that primary keys are correct
  if (any(duplicated(data1[, unique_key_1, with = F]))) {
    stop("unique_key_1 does not specify unique obs in ds1")
  }

  if (any(duplicated(data2[, unique_key_2, with = F]))) {
    stop("unique_key_2 does not specify unique obs in ds2")
  }

  ## check by variable lengths are comparable
  k <- length(by.x)
  if (length(by.y) != k | (!is.null(wgts) & length(wgts) != k) | length(compare_type) != k) {
    writeLines(paste0("ERROR: Length of by.x, by.y, non-null weights, and compare_type  must be equal!\n", k, " variables specified in by.x"))
    stop()
  }

  ## check that logit is a regression item
  if (!is.null(logit) & !any(class(logit) %in% c("glm", "lm"))) {
    writeLines("ERROR: Logit must be a regression output")
    stop()
  }

  #------------------------------------
  # Convert data inputs to data.tables
  #------------------------------------
  setDT(data1)
  setDT(data2)

  #-------------------------------------------
  # Determine number of comparison variables
  #-------------------------------------------
  k <- length(by.x)

  #------------------------------------
  # set equal weights if not specified
  #------------------------------------
  # if (is.null(wgts)) {
  #   wgts <- rep(1 / k, k)
  # }

  #------------------------
  # set comparison methods
  #------------------------
  c <- length(compare_type)
  last_c <- utils::tail(compare_type, 1)
  if (k > c) {
    compare_type <- c(compare_type, rep(last_c, k - c))
  }

  #--------------
  # check blocks
  #--------------
  if (any(blocks.x %in% by.x) | any(blocks.y %in% by.y)) {
    stop("Block fields cannot also be 'by' fields")
  }
  if (is.null(blocks.x) & is.null(blocks.y) & !is.null(blocks)) {
    blocks.x <- blocks
    blocks.y <- blocks
  } else if (is.null(blocks.x) & !is.null(blocks.y)) {
    stop("blocks.x must be specified with blocks.y")
  } else if (!is.null(blocks.x) & is.null(blocks.y)) {
    stop("blocks.y must be specified with blocks.x")
  }
  #------------------------------------------
  # Check threshold and override top default
  #------------------------------------------
  if (!is.numeric(threshold) & !is.null(threshold)) {
    stop("Threshold must be numeric")
  } else if (!is.null(threshold)) {
    top <- NULL
  }

  #-----------
  # Check top
  #-----------
  if (!is.null(top)) {

    ## check top type
    if (!is.numeric(top)) {
      stop("Top must be numeric")
    }
    ## check top is valid size
    if (top > nrow(data2)) {
      stop("Top is larger than comparison dataset")
    }
  }
  if (!is.null(logit)) {
    logit_stripped <- stripGlmLR(logit)
  } else {
    logit_stripped <- NULL
  }
  # if one of the comparison types is stringdist_wgt_jaccard, then we need a corpus of
  # words
  corpus <- vector(mode = "list", length = k)
  for (i in which(compare_type == "wgt_jaccard_dist")) {
    corpus[[i]] <- build_corpus(data1[[by.x[[i]]]],
                                        data2[[by.y[[i]]]])
  }

  #-----------------------
  # Calculate multivar_scores
  #-----------------------
  # generate a list that holds each 1-row data.table to iterate over
  get_row <- function(i, dt) {
    return(dt[i])
  }
  # print("success")
  data1_row_list <- purrr::map(1:nrow(data1), get_row, dt = data1)
  if (nthread > 1) {
    cl <- parallel::makeCluster(nthread)
    multivar_score_list <- parallel::parLapply(
      cl = cl,
      X = data1_row_list,
      fun = compare_row,
      data2 = copy(data2),
      by.x = by.x, by.y = by.y,
      logit = logit_stripped, missing = missing, wgts = wgts, compare_type = compare_type,
      blocks.x = blocks.x, blocks.y = blocks.y,
      top = top, threshold = threshold, suffixes = suffixes, k = k, unique_key_1 = unique_key_1,
      corpus = corpus
    )
    parallel::stopCluster(cl)
  } else if (nthread == 1) {
    multivar_score_list <- lapply(
      X = data1_row_list,
      FUN = compare_row,
      data2 = copy(data2),
      by.x = by.x, by.y = by.y,
      logit = logit_stripped, missing = missing, wgts = wgts, compare_type = compare_type,
      blocks.x = blocks.x, blocks.y = blocks.y,
      top = top, threshold = threshold, suffixes = suffixes, k = k, unique_key_1 = unique_key_1,
      corpus = corpus
    )
  }

  multivar_score_df <- rbindlist(multivar_score_list, use.names = TRUE)

  #----------------------------------
  # Return the multivar_score data.table
  #----------------------------------

  return(multivar_score_df)
}
