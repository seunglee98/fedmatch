## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fedmatch)
library(data.table)
data("corp_data1", package = "fedmatch")
data("corp_data2", package = "fedmatch")
data.table::setDT(corp_data1)
data.table::setDT(corp_data2)

## -----------------------------------------------------------------------------
tier_list <- list(
  a = list(match_type = "exact"),
  b = list(match_type = "fuzzy"),
  c = list(match_type = "multivar", multivar_settings = list(
    logit = NULL, missing = F, wgts = 1,
    compare_type = "stringdist", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
    top = 1, threshold = NULL
  ))
)

## -----------------------------------------------------------------------------
tier_list_v2 <- list(
  a = list(match_type = "exact", clean = clean_strings),
  b = list(match_type = "fuzzy", clean = clean_strings,
           fuzzy_settings = list(method = "wgt_jaccard",
                                 maxDist = .7,
                                 nthread = 1)),
  c = list(match_type = "multivar", multivar_settings = list(
    logit = NULL, missing = F, wgts = 1,
    compare_type = "stringdist", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
    top = 1, threshold = NULL
  ))
)

## -----------------------------------------------------------------------------
# corp_data1
# corp_data2[, Nam]
result <- tier_match(corp_data1, corp_data2,
  by.x = "Company", by.y = "Name",
  unique_key_1 = "unique_key_1", unique_key_2 = "unique_key_2",
  tiers = tier_list_v2, takeout = "neither", verbose = T,
  score_settings = build_score_settings(score_var_x = "Company",
                                        score_var_y = "Name",
                                        wgts = 1,
                                        score_type = "stringdist")
)
# result$matches

## -----------------------------------------------------------------------------
result$matches

## -----------------------------------------------------------------------------
result$match_evaluation

