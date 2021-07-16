#' Perform an iterative match by tier
#'
#' Constructs a tier_match by running \code{merge_plus} with different parameters sequentially
#' on the same data. Allows for sequential removal of observations after each tier.
#'
#' See the tier match vignette to get a clear understanding of the tier_match syntax.
#'
#' @param data1 data.frame. First to-merge dataset.
#' @param data2 data.frame. Second to-merge dataset.
#' @param by character string. Variables to merge on (common across data 1 and data 2). See \code{merge}
#' @param by.x character string. Variable to merge on in data1. See \code{merge}
#' @param by.y character string. Variable to merge on in data2. See \code{merge}
#' @param suffixes see \code{merge}
#' @param check_merge logical. Checks that your unique_keys are indeed unique, and prevents merge from running if merge would result in data.frames larger than 5 million rows
#' @param unique_key_1 character vector. Primary key of data1 that uniquely identifies each row (can be multiple fields)
#' @param unique_key_2 character vector. Primary key of data2 that uniquely identifies each row (can be multiple fields)
#' @param tiers list(). tier is a list of lists, where each list holds the parameters for creating that tier. All arguments to tier_match listed after this argument can either be supplied directly to tier_match, or indirectly via tiers.
#' @param match_type string. If 'exact', match is exact, if 'fuzzy', match is fuzzy.
#' @param fuzzy_settings additional arguments for amatch, to be used if match_type = 'fuzzy'. Suggested defaults provided. (see amatch, method='jw')
#' @param clean Function to clean strings prior to match. see \code{clean_strings}.
#' @param clean_settings list. Arguments passed to clean.
#' @param score_settings list. Settings for post-hoc matchscoring. See \code{build_score_settings}.
#' @param filter function or numeric. Filters a merged data1-data2 dataset. If a function, should take in
#'       a data.frame (data1 and data2 merged by name1 and name2) and spit out a trimmed verion
#'       of the data.frame (fewer rows). Think of this function as applying other conditions
#'       to matches, other than a match by name. The first argument of filter should be the data.frame.
#'       If numeric, will drop all observations with a matchscore lower than or equal to filter.
#' @param filter.args list. Arguments passed to filter, if a function
#' @param multivar_settings list of settings to go to the multivar match if match_type
#' == 'multivar'. See \code{multivar-match}.
#' @param evaluate Function to evalute merge_plus output. see \code{evaluate_match}.
#' @param evaluate.args list. Arguments passed to function specified by evaluate
#' @param takeout character vector, either 'data1', 'data2', 'both', or 'neither'. Removes observations after each tier from the selected dataset.
#' @param verbose boolean, whether or not to print tier names and time to match each tier as the matching happens.
#' @param allow.cartesian whether or not to allow many-many matches, see data.table::merge()
#' @return list with matches, data1 and data2 minus matches, and match evaluation
#'
#' @export
#'
#' @seealso merge_plus clean_strings
#'
tier_match <- function(data1, data2, by = NULL, by.x = NULL, by.y = NULL, suffixes = c("_1", "_2"),
                       check_merge = TRUE, unique_key_1, unique_key_2,
                       tiers = list(), takeout = "both",
                       match_type = "exact",
                       clean = NULL, clean_settings = list(),
                       score_settings = NULL, filter = NULL, filter.args = list(),
                       evaluate = match_evaluate, evaluate.args = list(),
                       allow.cartesian = T,
                       fuzzy_settings = build_fuzzy_settings(),
                       multivar_settings = build_multivar_settings(),
                       verbose = F) {
  # create make sure data are data.tables
  data1 <- data.table(data1)
  data2 <- data.table(data2)
  data1_save <- copy(data1)
  data2_save <- copy(data2)
  if (!is.null(by) & (!is.null(by.x) | !is.null(by.y))) {
    stop("both 'by' and ('by.x' or 'by.y') were supplied, this is not allowed")
  }
  if (!is.null(by)) {
    by.x <- by
    by.y <- by
  }

  if (unique_key_1 == unique_key_2) {
    stop("Unique keys must have different names.")
  }

  # expand tiers
  for (tier in names(tiers)) {
    if (!is.null(tiers[[tier]][["sequential_words"]])) {
      subtiers <- list()
      for (j in 1:nrow(tiers[[tier]][["sequential_words"]])) {
        subtiers[[paste(names(tiers[tier]), j, sep = ".")]] <- tiers[[tier]]
        subtiers[[paste(names(tiers[tier]), j, sep = ".")]][["clean_settings"]][["common_words"]] <- rbind(subtiers[[paste(names(tiers[tier]), j, sep = ".")]][["clean_settings"]][["common_words"]], subtiers[[paste(names(tiers[tier]), j, sep = ".")]][["sequential_words"]][1:j, ])
      }
      # put in the subtiers in between the tiers, or at the end, or at the front
      if (which(names(tiers) == tier) == length(tiers)) {
        tiers_before <- tiers[1:(which(names(tiers) == tier) - 1)]
        tiers <- c(tiers_before, subtiers)
      } else if (which(names(tiers) == tier) == 1) {
        tiers_after <- tiers[(which(names(tiers) == tier) + 1):length(tiers)]
        tiers <- c(subtiers, tiers_after)
      } else {
        tiers_before <- tiers[1:(which(names(tiers) == tier) - 1)]
        tiers_after <- tiers[(which(names(tiers) == tier) + 1):length(tiers)]
        tiers <- c(tiers_before, subtiers, tiers_after)
      }
    }
  }

  matches <- NULL
  tier_counter <- 0
  data1_keys_remove <- NULL
  data2_keys_remove <- NULL
  for (tier_name in names(tiers)) {
    if (verbose == T) {
      message("Matching tier '", tier_name, "'...")
      start_time <- Sys.time()
    }

    # we need the original data so we don't mess up the clean names,
    # but we also need to drop things after each tier if necessary
    if (is.null(data1_keys_remove)) {
      data1 <- copy(data1_save)
    } else {
      setkeyv(data1_save, unique_key_1)
      data1 <- copy(data1_save[!data1_keys_remove])
    }

    if (is.null(data2_keys_remove)) {
      data2 <- copy(data2_save)
    } else {
      setkeyv(data2_save, unique_key_2)
      data2 <- copy(data2_save[!data2_keys_remove])
    }

    tier_counter <- tier_counter + 1
    # cat("Tier ", tier_counter, ": ", tier_name, "\n")
    tier_settings <- tiers[[tier_name]]
    # assign by.x and by.y explicitly if they're not
    for (param in c("by.x", "by.y")) {
      if (is.null(tier_settings[[param]])) {
        tier_settings[[param]] <- tier_settings[["by"]]
      }
    }
    for (param in c(
      "by.x", "by.y", "check_merge", "match_type",
      "fuzzy_settings",
      # "fuzzy_method", "fuzzy_p", "fuzzy_maxdist", "fuzzy_matchna", "fuzzy_nthread",
      "score_settings", "filter", "filter.args", "evaluate",
      "evaluate.args", "clean_settings", "allow.cartesian", "multivar_settings", "clean"
    )) {
      # assign parameters to defaults if they're not in the tier
      if (is.null(tier_settings[[param]])) {
        if (!is.null(get(param))) {
          tier_settings[[param]] <- get(param)
        } else {
          tier_settings[[param]] <- NULL
        }
      }
    }
    # clean the by's

    if (is.function(tier_settings[["clean"]]) & length(tier_settings[["by.x"]]) == 1) {
      data1[[tier_settings[["by.x"]]]] <- do.call(tier_settings[["clean"]], c(list(string = data1[[tier_settings[["by.x"]]]]), tier_settings[["clean_settings"]]))
      data2[[tier_settings[["by.y"]]]] <- do.call(tier_settings[["clean"]], c(list(string = data2[[tier_settings[["by.y"]]]]), tier_settings[["clean_settings"]]))
    }
    # run the match
    tier_result <- merge_plus(
      data1 = data1, data2 = data2,
      by.x = tier_settings[["by.x"]], by.y = tier_settings[["by.y"]],
      suffixes = suffixes, check_merge = check_merge,
      unique_key_1 = unique_key_1,
      unique_key_2 = unique_key_2,
      match_type = tier_settings[["match_type"]],
      fuzzy_settings = tier_settings[["fuzzy_settings"]],
      score_settings = tier_settings[["score_settings"]],
      filter = tier_settings[["filter"]],
      filter.args = tier_settings[["filter.args"]],
      evaluate = NULL,
      evaluate.args = NULL,
      allow.cartesian = tier_settings[["allow.cartesian"]],
      multivar_settings = tier_settings[["multivar_settings"]]
    )

    if (is.null(tier_settings[["filter"]])) {
      newmatches <- tier_result[["matches"]]
    } else {
      newmatches <- tier_result[["matches_filter"]]
    }

    if (!is.null(newmatches)) {
      # assign the name of the tier
      newmatches[, tier := tier_name]
      # in case we merge on the same variable
      if (length(tier_settings[["by.y"]]) == 1 & !(tier_settings[["by.y"]][1] %in% names(newmatches)) & tier_settings[["by.y"]][1] != tier_settings[["by.x"]][1]) {
        newmatches[[by.y]] <- newmatches[[by.x]]
      }
      # else if (tier_settings[["by.y"]]) {
      #   newmatches[[str_c(by.y, suffixes[2])]] <- newmatches[[by.x]]
      #   newmatches[[str_c(by.x, suffixes[1])]] <- newmatches[[by.x]]
      #   newmatches[, c(by.x) := NULL]
      # }
      # add to the total matches
      if (is.null(matches)) {
        matches <- copy(newmatches)
      } else {
        matches <- rbind(matches, newmatches, fill = T)
      }
      if (takeout %in% c("data2", "both")) {
        if (is.null(data2_keys_remove)) {
          data2_keys_remove <- data.table(a = tier_result[["matches"]][[unique_key_2]])
          setnames(data2_keys_remove, unique_key_2)
        } else {
          data2_keys_remove_new <- data.table(a = tier_result[["matches"]][[unique_key_2]])
          setnames(data2_keys_remove_new, unique_key_2)
          data2_keys_remove <- rbind(data2_keys_remove, data2_keys_remove_new)
        }
      }
      if (takeout %in% c("data1", "both")) {
        if (is.null(data1_keys_remove)) {
          data1_keys_remove <- data.table(a = tier_result[["matches"]][[unique_key_1]])
          setnames(data1_keys_remove, unique_key_1)
        } else if (takeout == "neither") {
          data1_keys_remove_new <- data.table(a = tier_result[["matches"]][[unique_key_1]])
          setnames(data1_keys_remove_new, unique_key_1)
          data1_keys_remove <- rbind(data1_keys_remove, data1_keys_remove_new)
        } else {
          stop("'takeout' must be one of 'data1', 'data2', 'both', or 'neither'.")
        }
      }
    }
    if (verbose == T) {
      end_time <- Sys.time()
      diff <- end_time - start_time
      diff_num <- as.numeric(diff) %>% round(2)
      diff_units <- attr(diff, "units")
      message("Time elapsed: ", diff_num, " ", diff_units, ".")
    }
  } # close off tier loop



  ## evaluating
  if (is.null(matches)) {
    match_evaluation <- NULL
  } else {
    if (!is.null(evaluate)) {
      match_evaluation <- do.call(evaluate, c(list(matches = matches, data1 = data1_save, data2 = data2_save, unique_key_1 = unique_key_1, unique_key_2 = unique_key_2, suffixes = suffixes, tier = "tier"), evaluate.args))
    } else {
      match_evaluation <- NULL
    }
  }
  if (is.null(matches)) {
    data1_nomatch <- data1
    data2_nomatch <- data2
  } else {
    setkeyv(data1, unique_key_1)
    setkeyv(data2, unique_key_2)
    setkeyv(matches, c(unique_key_1, unique_key_2))
    # use DT's anti-join syntax
    data1_nomatch <- data1[!matches]
    data2_nomatch <- data2[!matches]
  }



  return(list("matches" = matches, "data1_nomatch" = data1_nomatch, "data2_nomatch" = data2_nomatch, "match_evaluation" = match_evaluation))
}
