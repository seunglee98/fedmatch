#' evaluate a matched dataset
#'
#' \code{match_evaluate} takes in matches and outputs summary statistics for those matches, including
#' the number of matches in each tier and the percent matched from each dataset.
#'
#' The most straightforward way to use \code{match_evaluate} is to pass it to the \code{evaluate}
#' argument of \code{tier_match} or \code{merge_plus}. This will have \code{merge_plus}
#' return a data.table with the evaluation information, alongside the matches themselves.
#'
#' I
#'
#' \code{match_evaluate} returns the number of matches in each tier, the number of
#' unique matches in each tier, and the percent matched for each dataset. If no tiers are supplied,
#' the entire dataset will be used as one "tier."
#' The argument \code{quality_vars} allows for the calculation of averages of any columns in the dataset, by tier.
#' The most straightforward case would be a matchscore, which can again all be done
#' in \code{merge_plus} with the scoring argument. This lets you see the average matchscore by tier.
#'
#' @param matches data.frame. Merged dataset.
#' @param data1 data.frame. First to-merge dataset.
#' @param data2 data.frame. Second to-merge dataset.
#' @param unique_key_1 character vector. Primary key of data1 that uniquely identifies each row (can be multiple fields)
#' @param unique_key_2 character vector. Primary key of data2 that uniquely identifies each row (can be multiple fields)
#' @param suffixes character vector. Mnemonics associated data1 and data2.
#' @param tier character vector. Default=NULL. The variable that defines a tier.
#' @param tier_order character vector. Default= "tier". Variable that defines the order of tiers, if needed.
#' @param quality_vars character vector. Variables you want to use to calculate the quality of each tier. Calculates mean.
#'
#' @return data.table. Table describing each tier according to aggregate_by variables and quality_vars variables.
#'
#' @seealso merge_plus
#'
#' @export
#'

match_evaluate <- function(matches, data1, data2, unique_key_1, unique_key_2, suffixes = c(".x", ".y"),
                           tier = "tier", tier_order = NULL,
                           quality_vars = NULL) {
  setDT(matches)
  # print(unique(matches[, .(tier)]))
  if (!is.null(tier_order)) {
    matches[, tier := forcats::fct_relevel(tier, tier_order)]
    matches <- matches[order(tier)]
  } else if (!is.null(tier)) {
    tier_order <- matches[, unique(tier)]
    matches[, tier := forcats::fct_relevel(tier, tier_order)]
    matches <- matches[order(tier)]
  }

  if (is.null(tier)) {
    matches[, tier := "all"]
  }
  # create copies before changing names, see ?setnames
  data1 <- copy(data1)
  data2 <- copy(data2)

  match_evaluation <- matches[
    , .(
      matches = .N,
      in_tier_unique_1 = uniqueN(.SD[[..unique_key_1]]),
      in_tier_unique_2 = uniqueN(.SD[[..unique_key_2]])
    ),
    .(tier = tier)
  ]

  match_evaluation[, `:=`(
    pct_matched_1 = in_tier_unique_1 / uniqueN(data1[[unique_key_1]]),
    pct_matched_2 = in_tier_unique_2 / uniqueN(data2[[unique_key_2]])
  )]
  # find how many matches are unique to that tier

  prev_tiers <- NULL
  for (tiername in matches[, unique(tier)]) {
    temp_new_unique_1 <- setdiff(
      matches[tier == tiername][[unique_key_1]] %>% unique(),
      matches[tier %in% prev_tiers][[unique_key_1]] %>% unique()
    ) %>% length()

    temp_new_unique_2 <- setdiff(
      matches[tier == tiername][[unique_key_2]] %>% unique(),
      matches[tier %in% prev_tiers][[unique_key_2]] %>% unique()
    ) %>% length()

    match_evaluation[tier == tiername, `:=`(
      new_unique_1 = temp_new_unique_1,
      new_unique_2 = temp_new_unique_2
    )]
    prev_tiers <- c(prev_tiers, tiername)
  }

  # now make total evaluation
  if (uniqueN(matches[, tier]) > 1) {
    match_evaluation_total <- matches[, .(
      matches = .N,
      in_tier_unique_1 = uniqueN(.SD[[..unique_key_1]]),
      in_tier_unique_2 = uniqueN(.SD[[..unique_key_2]]),
      tier = "all"
    )]

    match_evaluation_total[, `:=`(
      pct_matched_1 = in_tier_unique_1 / uniqueN(data1[[unique_key_1]]),
      pct_matched_2 = in_tier_unique_2 / uniqueN(data2[[unique_key_2]])
    )]
    if (!is.null(quality_vars)) {
      quality_dt_total <- matches[, lapply(.SD, mean, na.rm = T), .SDcols = quality_vars]
      quality_dt_total[, tier := "all"]
      match_evaluation_total <- merge(match_evaluation_total, quality_dt_total, by = tier)
    }
    match_evaluation <- rbind(match_evaluation, match_evaluation_total, fill = T)
    match_evaluation <- unique(match_evaluation)
  }
  if (!is.null(quality_vars)) {
    quality_dt <- matches[, lapply(.SD, mean, na.rm = T), by = .(tier), .SDcols = quality_vars]
    match_evaluation <- merge(match_evaluation, quality_dt, by = "tier")
  }


  return(match_evaluation)
}
