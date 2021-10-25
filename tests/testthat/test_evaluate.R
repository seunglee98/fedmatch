
test_that("match_evaluate returns matches that are correct number", {
  corp_data1 <- copy(fedmatch::corp_data1)
  corp_data2 <- copy(fedmatch::corp_data2)
  tier_list <- list(
    a = list(match_type = "exact"),
    b = list(match_type = "fuzzy"),
    c = list(match_type = "multivar", multivar_settings = list(
      logit = NULL, missing = FALSE, wgts = 1,
      compare_type = "stringdist", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
      top = 1, threshold = NULL
    ))
  )
  # tier_list <- list(a = list(match_type = "exact"))
  result <- tier_match(corp_data1, corp_data2,
    by.x = "Company", by.y = "Name",
    unique_key_1 = "unique_key_1", unique_key_2 = "unique_key_2",
    tiers = tier_list, takeout = "neither"
  )
  result
  # result$matches
  # result$match_evaluation
  pct_matched <- result$match_evaluation[2, 5, with = F]
  expect_false(pct_matched == 1)
})
test_that("match_evaluate returns matches that are correct, different unique key names", {
  corp_data1 <- copy(fedmatch::corp_data1)
  corp_data2 <- copy(fedmatch::corp_data2)
  tier_list <- list(
    a = list(match_type = "exact"),
    b = list(match_type = "fuzzy"),
    c = list(match_type = "multivar", multivar_settings = list(
      logit = NULL, missing = FALSE, wgts = c(1),
      compare_type = "stringdist", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
      top = 1, threshold = NULL
    )),
    d = list(match_type = "exact", clean_settings = list(remove_words = TRUE))
  )
  corp_data1[, unique_k_1 := unique_key_1][, unique_key_1 := NULL]
  corp_data2[, unique_k_2 := unique_key_2][, unique_key_2 := NULL]
  # unique_key_1 <- "unique_key_1"
  # corp_data1[, .(test = uniqueN(.SD[[..unique_key_1]])), Country]
  # corp_data1[[c("unique_key_1", "Company")]]
  # corp_data2
  # corp_data1
  # tier_list <- list(a = list(match_type = "exact"))

  result <- tier_match(corp_data1, corp_data2,
    by.x = "Company", by.y = "Name",
    unique_key_1 = "unique_k_1", unique_key_2 = "unique_k_2",
    tiers = tier_list, takeout = "neither"
  )
  result
  # fsetdiff(c(1, 2, 3), c(1))
  # result
  # result$matches[tier == "d"]
  # result$matches
  # result$match_evaluation
  pct_matched <- result$match_evaluation[2, 5, with = F]
  expect_false(pct_matched == 1)
})
test_that("match_evaluate returns new unique matches", {
  corp_data1 <- copy(fedmatch::corp_data1)
  corp_data2 <- copy(fedmatch::corp_data2)
  tier_list <- list(
    a = list(match_type = "exact"),
    b = list(match_type = "fuzzy"),
    c = list(match_type = "multivar", multivar_settings = list(
      logit = NULL, missing = FALSE, wgts = c(1),
      compare_type = "stringdist", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
      top = 1, threshold = NULL
    )),
    d = list(match_type = "exact", clean_settings = list(remove_words = TRUE))
  )
  corp_data1[, unique_k_1 := unique_key_1][, unique_key_1 := NULL]
  corp_data2[, unique_k_2 := unique_key_2][, unique_key_2 := NULL]
  # unique_key_1 <- "unique_key_1"
  # corp_data1[, .(test = uniqueN(.SD[[..unique_key_1]])), Country]
  # corp_data1[[c("unique_key_1", "Company")]]
  # corp_data2
  # corp_data1
  # tier_list <- list(a = list(match_type = "exact"))
  result <- tier_match(corp_data1, corp_data2,
    by.x = "Company", by.y = "Name",
    unique_key_1 = "unique_k_1", unique_key_2 = "unique_k_2",
    tiers = tier_list, takeout = "neither"
  )
  # result$match_evaluation
  # fsetdiff(c(1, 2, 3), c(1))
  # result
  # result$matches[tier == "d"]
  # result$matches
  # result$match_evaluation
  new_matches <- result$match_evaluation[1, 7, with = F]
  expect_false(new_matches == 1)
})
