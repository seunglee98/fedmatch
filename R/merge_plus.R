#' Merge two datasets either by exact, fuzzy, or multivar-based matching
#'
#' \code{merge_plus} is a wrapper for a standard merge, a fuzzy string match,
#' and a a ``multivar'' match based on several columns of the data. Parameters allow
#' for control for fine-tuning of the match. This is primarily used as the
#' workhorse for the \code{tier_match} function.
#'
#'
#' @param data1 data.frame. First to-merge dataset.
#' @param data2 data.frame. Second to-merge dataset.
#' @param by character string. Variables to merge on (common across data 1 and
#'   data 2). See \code{merge}
#' @param by.x length-1 character vector. Variable to merge on in data1. See \code{merge}
#' @param by.y length-1 character vector. Variable to merge on in data2. See \code{merge}
#' @param suffixes character vector with length==2. Suffix to add to like named
#'   variables after the merge. See \code{merge}
#' @param unique_key_1 character vector. Primary key of data1 that uniquely
#'   identifies each row (can be multiple fields)
#' @param unique_key_2 character vector. Primary key of data2 that uniquely
#'   identifies each row (can be multiple fields)
#' @param check_merge logical. Checks that your unique_keys are indeed unique.
#' @param match_type string. If 'exact', match is exact, if 'fuzzy', match is
#'   fuzzy. If 'multivar,' match is multivar-based. See \code{multivar_match},
#' @param fuzzy_settings additional arguments for amatch, to be used if match_type
#'   = 'fuzzy'. Suggested defaults provided. See \code{build_fuzzy_settings}.
#' @param score_settings list. Score settings for post-hoc matchscores. See \code{build_score_settings}

#' @param multivar_settings list of settings to go to the multivar match if match_type
#' == 'multivar'. See \code{multivar-match} and \code{build_multivar_settings}.
#' @param filter function or numeric. Filters a merged data1-data2 dataset. If a
#'   function, should take in a data.frame (data1 and data2 merged by name1 and
#'   name2) and spit out a trimmed verion of the data.frame (fewer rows). Think
#'   of this function as applying other conditions to matches, other than a
#'   match by name. The first argument of filter should be the data.frame. If
#'   numeric, will drop all observations with a matchscore lower than or equal
#'   to filter.
#' @param filter.args list. Arguments passed to filter, if a function
#' @param evaluate Function to evalute merge_plus output.
#' @param evaluate.args list. Arguments passed to evaluate
#' @param allow.cartesian whether or not to allow many-many matches, see data.table::merge()
#' @return list with matches, filtered matches (if applicable), data1 and data2
#'   minus matches, and match evaluation
#'
#' @seealso match_evaluate
#'
#' @export merge_plus

merge_plus <- function(data1, data2, by = NULL, by.x = NULL, by.y = NULL,
                       suffixes = c("_1", "_2"),
                       check_merge = TRUE, unique_key_1, unique_key_2,
                       match_type = "exact",
                       fuzzy_settings = build_fuzzy_settings(),
                       score_settings = NULL, filter = NULL, filter.args = list(),
                       evaluate = match_evaluate, evaluate.args = list(), allow.cartesian = FALSE,
                       multivar_settings = build_multivar_settings()) {
  data1 <- data.table(data1)
  data2 <- data.table(data2)
  # argument checks
  if (length(unique_key_1) > 1 | length(unique_key_2) > 1 | !is.character(unique_key_1) | !is.character(unique_key_2)) {
    stop("Unique key must be a length-1 character vector")
  }
  if (unique_key_1 == unique_key_2) {
    stop("Unique keys must be different names.")
  }

  if (!is.null(by) & (!is.null(by.x) | !is.null(by.y))) {
    stop("both 'by' and ('by.x' or 'by.y') were supplied, this is not allowed")
  }
  if (is.null(by) & (is.null(by.x) | is.null(by.y))) {
    stop("either 'by' or ('by.x' and 'by.y') must not be NULL.")
  }
  if (!is.null(by)) {
    by.x <- by
    by.y <- by
  }

  ### checking that unique_keys are indeed unique

  if (data1[, .N] != uniqueN(data1, by = unique_key_1)) {
    stop("unique_key_1 does not uniquely identify data1")
  }
  if (data2[, .N] != uniqueN(data2, by = unique_key_2)) {
    stop("unique_key_2 does not uniquely identify data2")
  }

  ## fixing unique_keys in case they have the same name
  ## i.e. give them suffixes
  unique_key_1_matches <- unique_key_1
  unique_key_2_matches <- unique_key_2
  for (i in 1:pmax(length(unique_key_1), length(unique_key_2))) {
    if (unique_key_1[i] %in% names(data2) & !(unique_key_1[i] %in% c(by, by.x, by.y))) {
      unique_key_1_matches[i] <- paste0(unique_key_1[i], suffixes[1])
    }
    if (unique_key_2[i] %in% names(data1) & !(unique_key_2[i] %in% c(by, by.x, by.y))) {
      unique_key_2_matches[i] <- paste0(unique_key_2[i], suffixes[2])
    }
  }

  if (match_type == "exact") {

    if (length(by.x) > 1 | length(by.y) > 1 | length(by) > 1) {
      stop("For exact matching, 'by'(or 'by.x', and 'by.y') must be length-1 character vectors. If desired, try combining columns with paste0.")
    }
    if (any(is.na(data1[[by.x]]))) {
      n_init <- data1[, .N]
      data1_nas <- data1[is.na(get(by.x))]
      data1 <- data1[!is.na(get(by.x))]
      n_final <- data1[, .N]
      message(stringr::str_c("Removing ",n_init - n_final," NA observations in by.x. These will be re-inserted for subsequent tiers."))
    } else {
      data1_nas <- NULL
    }
    if (any(is.na(data2[[by.y]]))) {
      n_init <- data2[, .N]
      data2_nas <- data2[is.na(get(by.y))]
      data2 <- data2[!is.na(get(by.y))]
      n_final <- data2[, .N]
      message(stringr::str_c("Removing ",n_init - n_final," NA observations in by.y. These will be re-inserted for subsequent tiers."))
    } else {
      data2_nas <- NULL
    }
    matches <- merge(data1, data2,
      by.x = by.x,
      by.y = by.y, suffixes = suffixes, allow.cartesian = allow.cartesian
    )
    if (by.x == by.y) {
      matches[[stringr::str_c(by.x, suffixes[1])]] <- matches[[by.x]]
      matches[[stringr::str_c(by.y, suffixes[2])]] <- matches[[by.y]]
      matches <- matches[, !c(by.x), with = F]
    } else if (by.x != by.y) {
      matches[[by.y]] <- matches[[by.x]]
    }
  } else if (match_type == "fuzzy") {
    if (length(by.x) > 1 | length(by.y) > 1 | length(by) > 1) {
      stop("For fuzzy matching, 'by'(or 'by.x', and 'by.y') must be length-1 character vectors. If desired, try combining columns with paste0.")
    }
    if (any(is.na(data1[[by.x]]))) {
      n_init <- data1[, .N]
      data1_nas <- data1[is.na(get(by.x))]
      data1 <- data1[!is.na(get(by.x))]
      n_final <- data1[, .N]
      message(stringr::str_c("Removing ",n_init - n_final," NA observations in by.x. These will be re-inserted for subsequent tiers."))
    } else {
      data1_nas <- NULL
    }
    if (any(is.na(data2[[by.y]]))) {
      n_init <- data2[, .N]
      data2_nas <- data2[is.na(get(by.y))]
      data2 <- data2[!is.na(get(by.y))]
      n_final <- data2[, .N]
      message(stringr::str_c("Removing ",n_init - n_final," NA observations in by.y. These will be re-inserted for subsequent tiers."))
    } else {
      data2_nas <- NULL
    }
    matches <- fuzzy_match(data1, data2,
      by.x = by.x, by.y = by.y, suffixes = suffixes,
      unique_key_1 = unique_key_1, unique_key_2 = unique_key_2,
      fuzzy_settings = fuzzy_settings
    )
  } else if (match_type == "multivar") {
    multivar_settings <- c(
      list(
        data1 = data1, data2 = data2, by.x = by.x, by.y = by.y,
        unique_key_1 = unique_key_1, unique_key_2 = unique_key_2,
        suffixes = suffixes
      ),
      multivar_settings
    )
    matches <- do.call(multivar_match, multivar_settings)
    data1_nas <- NULL
    data2_nas <- NULL
  }

  ## checking matches
  if (nrow(matches) == 0) {
    message(paste("Merge returned no matches"))
    matches <- NULL
    matches_filter <- NULL
    data1_nomatch <- rbind(data1, data1_nas, fill = T)
    data2_nomatch <- rbind(data2, data2_nas, fill = T)
    match_evaluation <- NULL
  } else { # this bracket is closed after #Evaluate

    ## remove obs from data1 and data2 that are in the matched dataset
    setkeyv(data1, unique_key_1)
    setkeyv(matches, c(unique_key_1))
    data1_nomatch <- data1[!matches]
    data1_nomatch <- rbind(data1_nomatch, data1_nas, fill = T)
    # use DT's anti-join syntax
    setkeyv(data2, unique_key_2)
    setkeyv(matches, c(unique_key_2))
    data2_nomatch <- data2[!matches]
    data2_nomatch <- rbind(data2_nomatch, data2_nas, fill = T)
    if (!is.null(score_settings)) {
      if (!is.null(score_settings[["score_var_both"]])) {
        score.x <- score_settings[["score_var_both"]]
        score.y <- score_settings[["score_var_both"]]
      } else {
        score.x <- score_settings[["score_var_x"]]
        score.y <- score_settings[["score_var_y"]]
      }
      variables_to_score <- paste0(score.x, "_score")
      matchscore <- NULL # due to NSE notes in R CMD check
      matches$matchscore <- 0
      for (i in 1:length(score_settings[["wgts"]])) {
        if (score.x[i] == score.y[i]) {
          variable_x <- paste0(score.x[i], suffixes[1])
          variable_y <- paste0(score.y[i], suffixes[2])
        } else {
          variable_x <- score.x[i]
          variable_y <- score.y[i]
        }

        variable_score <- variables_to_score[i]
        score_type_i <- score_settings[["score_type"]][i]
        if (score_type_i == "in") {
          matches[, c(variable_score) := fifelse(stringr::str_detect(get(variable_y), get(variable_x)), 1, 0)]
        }
        if (score_type_i == "indicator") {
          matches[, c(variable_score) := fifelse(get(variable_x) == get(variable_y) & !is.na(get(variable_x)) & !is.na(get(variable_y)), 1, 0)]
        }
        if (score_type_i == "substr") {
          matches[, c(variable_score) := fifelse(get(variable_x) == substr(get(variable_y), 1, nchar(get(variable_x))), 1, 0)]
        }
        if (score_type_i == "difference") {
          matches[, c(variable_score) := scales::rescale(get(variable_x) - get(variable_y))]
        }
        if (score_type_i == "ratio") {
          matches[, c(variable_score) := scales::rescale(get(variable_x) / get(variable_y))]
        }
        if (score_type_i == "stringdist") {
          matches[, c(variable_score) := 1 - stringdist::stringdist(get(variable_x), get(variable_y), method = "jw", p = 0.1)]
        }
      }
      matches[, matchscore := as.matrix(matches[, variables_to_score, with = F]) %*% as.matrix(score_settings[["wgts"]])]
    }

    if (!is.null(filter)) {

      ## filter data
      if (is.function(filter)) {
        matches_filter <- do.call(filter, c(list(matches), filter.args))
      }
      if (is.numeric(filter)) {
        matches_filter <- matches[matchscore >= filter]
        # class(matches_filter) <- "data.frame"
      }
      ## redefine no matches to include only those in the filtered dataset
      setkey(data1, unique_key_1)
      setkey(matches_filter, unique_key_1)
      data1_nomatch <- data1[!matches_filter]
      setkey(data2, unique_key_2)
      setkey(matches_filter, unique_key_2)
      data2_nomatch <- data2[!matches_filter]

      ## checking results
      if (nrow(matches_filter) == 0) {
        message(paste("There are no matches after filtering"))
        matches_filter <- NULL
      }
    } else {
      matches_filter <- NULL
    }
    setDT(matches)
    setDT(matches_filter)
    setDT(data1)
    setDT(data2)

    if (is.null(matches)) {
      match_evaluation <- NULL
    } else {
      if (!is.null(evaluate)) {
        match_evaluation <- do.call(evaluate, c(list(matches = matches, data1 = data1, data2 = data2, unique_key_1 = unique_key_1, unique_key_2 = unique_key_2, suffixes = suffixes, tier = NULL), evaluate.args))
      } else {
        match_evaluation <- NULL
      }
    }
  } # closes the else from ##checking matches

  # Return
  return(list("matches" = matches, "matches_filter" = matches_filter, "data1_nomatch" = data1_nomatch, "data2_nomatch" = data2_nomatch, "match_evaluation" = match_evaluation))
}
