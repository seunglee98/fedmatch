
compare_row <- function(row_x, data2,
                        by.x, by.y,
                        logit = NULL, missing = FALSE, wgts = NULL, compare_type = "diff",
                        blocks.x = NULL, blocks.y = NULL,
                        top = NULL, threshold = NULL, suffixes = c("_1", "_2"), k, unique_key_1,
                        corpus = NULL) {

  #----------------------
  # Rename with suffixes
  #----------------------
  # in case there's any shared non-matching vars
  # first create copies, see ?setnames
  row_x <- copy(row_x)
  data2 <- copy(data2)

  #------------------------------------------------------------
  ## keep only rows that match blocked variables if applicable
  #------------------------------------------------------------
  if (!is.null(blocks.x) & !is.null(blocks.y)) {
    df_c <- merge(row_x, data2, by.x = blocks.x, by.y = blocks.y, suffixes = suffixes)
    ## otherwise build the cartesian product
  } else if (is.null(blocks.x) & is.null(blocks.y)) {
    row_x$mergetemp <- 1
    data2$mergetemp <- 1
    df_c <- merge(row_x, data2, by = "mergetemp", allow.cartesian = TRUE, suffixes = suffixes)
    mergetemp <- NULL # due to NSE notes in R CMD check
    df_c[, mergetemp := NULL]
  } ## end block loop

  #------------------------
  ## generate comparisons
  #------------------------
  # note that this requi
  variables_to_compare <- paste0(by.x, "_compare")

  for (i in 1:k) {

    ## create variable names to compare
    if (by.x[i] == by.y[i]) {
      variable_x <- paste0(by.x[i], suffixes[1])
      variable_y <- paste0(by.y[i], suffixes[2])
    } else {
      variable_x <- by.x[i]
      variable_y <- by.y[i]
    }

    variable_compare <- variables_to_compare[i]
    compare_type_i <- compare_type[i]

    if (compare_type_i == "in") {
      df_c[, c(variable_compare) := fifelse(stringr::str_detect(get(variable_y), get(variable_x)), 1, 0)]
    }
    if (compare_type_i == "indicator") {
      df_c[, c(variable_compare) := fifelse(get(variable_x) == get(variable_y) & !is.na(get(variable_x)) & !is.na(get(variable_y)), 1, 0)]
    }
    if (compare_type_i == "substr") {
      df_c[, c(variable_compare) := fifelse(get(variable_x) == substr(get(variable_y), 1, nchar(get(variable_x))), 1, 0)]
    }
    if (compare_type_i == "difference") {
      df_c[, c(variable_compare) := scales::rescale(get(variable_x) - get(variable_y))]
    }
    if (compare_type_i == "ratio") {
      df_c[, c(variable_compare) := scales::rescale(get(variable_x) / get(variable_y))]
    }
    if (compare_type_i == "stringdist") {
      df_c[, c(variable_compare) := 1 - stringdist::stringdist(get(variable_x), get(variable_y), method = "jw", p = 0.1)]
    }
    if (compare_type_i == "wgt_jaccard_dist") {

      df_c[, c(variable_compare) := 1 - wgt_jaccard_distance(get(variable_x), get(variable_y), corpus = corpus[[i]])]

    }
  } ## end comparison calculations

  #-----------------------
  # define missing values
  #-----------------------

  if (missing == TRUE) {
    variables_to_missing <- paste0(by.x, "_missing")

    for (i in 1:k) {
      variable_x <- paste0(by.x[i], suffixes[1])
      variable_y <- paste0(by.y[i], suffixes[2])
      variable_missing <- variables_to_missing[i]

      if (any(is.na(df_c[, get(variable_x)])) | any(is.null(df_c[, get(variable_x)])) | any(which(df_c[, trimws(get(variable_x))] == "")) |
        any(is.na(df_c[, get(variable_y)])) | any(is.null(df_c[, get(variable_y)])) | any(which(df_c[, trimws(get(variable_y))] == ""))

      ) {
        df_c[, c(variable_missing) := ifelse(is.null(get(variable_x)) | is.na(get(variable_x)) | trimws(get(variable_x)) == "" |
          is.null(get(variable_y)) | is.na(get(variable_y)) | trimws(get(variable_y)) == "", 1, 0)]
      }
    }
  } ## end missing variable loop

  #-------------------------
  # generate the multivar_score
  #-------------------------
  multivar_score <- NULL # due to NSE notes in R CMD check
  if (!is.null(wgts)) {
    df_c[, multivar_score := as.matrix(df_c[, variables_to_compare, with = F]) %*% as.matrix(wgts)]
  } else if (!is.null(logit) & any(class(logit) %in% c("glm", "lm"))) {
    df_c[, multivar_score := stats::predict(logit, df_c, type = "response")]
  } else {
    stop("multivar_match requires either a vector of weights or a logit model to compute multivar_score.")
  }

  #---------------------------
  # Keep obs within threshold
  #---------------------------
  if (!is.null(threshold)) {
    df_c <- df_c[which(multivar_score >= threshold)]
  }

  #-----------------------------------------
  # Keep the top N multivar_scores as requested
  # (How do we break ties?)
  #---------------------------------

  if (!is.null(top)) {
    df_c <- df_c[order(-multivar_score), utils::head(.SD, top), by = unique_key_1]
  }

  #-----------------------
  # return the multivar_score
  #-----------------------
  if (by.x[1] != by.y[1]) {
    df_c <- copy(df_c)
    setnames(df_c, paste0(by.x, suffixes[1]), by.x, skip_absent = TRUE)
  }

  return(df_c)
}

stripGlmLR <- function(cm) {
  cm$y <- c()
  cm$model <- c()

  cm$residuals <- c()
  cm$fitted.values <- c()
  cm$effects <- c()
  cm$qr$qr <- c()
  cm$linear.predictors <- c()
  cm$weights <- c()
  cm$prior.weights <- c()
  cm$data <- c()


  cm$family$variance <- c()
  cm$family$dev.resids <- c()
  cm$family$aic <- c()
  cm$family$validmu <- c()
  cm$family$simulate <- c()
  attr(cm$terms, ".Environment") <- c()
  attr(cm$formula, ".Environment") <- c()

  cm
}
