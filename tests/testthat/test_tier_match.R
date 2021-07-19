
test_that("tier_match can handle unique keys being named unique_key_1 and unique_key_2", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2
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
  corp_data1
  result <- tier_match(corp_data1, corp_data2,
    by.x = "Company", by.y = "Name",
    unique_key_1 = "unique_key_1", unique_key_2 = "unique_key_2",
    tiers = tier_list, takeout = "neither", verbose = TRUE
  )


  # result$match_evaluation
  expect_true(is.data.table(result$matches))
})
test_that("tier_match can handle unique keys being NOT named unique_key_1 and unique_key_2", {
  test_func <- function() {
    corp_data1 <- fedmatch::corp_data1
    corp_data2 <- fedmatch::corp_data2
    # corp_data1
    # corp_data1
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
    # corp_data2
    # corp_data1
    # tier_list <- list(a = list(match_type = "exact"))
    result <- tier_match(corp_data1, corp_data2,
      by.x = "Company", by.y = "Name",
      unique_key_1 = "unique_k_1", unique_key_2 = "unique_k_2",
      tiers = tier_list, takeout = "neither"
    )
    result
  }
  # result
  # result$matches[tier == "d"]
  # result$matches
  # result$match_evaluation
  expect_true(is.list(test_func()))
})
test_that("tier_match can handle by's being the same", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2
  corp_data1
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
  # tier_list <- list(a = list(match_type = "exact"))
  # tier_list <- list(a = list(match_type = "fuzzy"),
  #                   by = list(match_type = "exact"))
  corp_data1[, unique_k_1 := unique_key_1][, unique_key_1 := NULL]
  corp_data2[, unique_k_2 := unique_key_2][, unique_key_2 := NULL]
  corp_data1[, name := Company]
  corp_data2[, name := Name]
  # corp_data2
  # corp_data1
  corp_data1
  # tier_list <- list(a = list(match_type = "exact"))
  result <- tier_match(corp_data1, corp_data2,
    by.x = "name", by.y = "name",
    unique_key_1 = "unique_k_1", unique_key_2 = "unique_k_2",
    tiers = tier_list, takeout = "neither",
    suffixes = c("_1", "_2")
  )
  result
  result$matches[tier == "b"]

  # result
  # result$matches[tier == "d"]
  # result$matches
  # result$match_evaluation
  expect_true(is.data.table(result$matches))
})
test_that("tier_match can handle by's being the same with scoring", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2
  corp_data1
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
  # tier_list <- list(a = list(match_type = "exact"))
  # tier_list <- list(a = list(match_type = "fuzzy"),
  #                   by = list(match_type = "exact"))
  corp_data1[, unique_k_1 := unique_key_1][, unique_key_1 := NULL]
  corp_data2[, unique_k_2 := unique_key_2][, unique_key_2 := NULL]
  corp_data1[, name := Company]
  corp_data2[, name := Name]

  score_settings <- build_score_settings(
    wgts = 1,
    score_type = "stringdist",
    score_var_both = "name"
  )
  # tier_list <- list(a = list(match_type = "exact"))
  result <- tier_match(corp_data1, corp_data2,
    by.x = "name", by.y = "name",
    unique_key_1 = "unique_k_1", unique_key_2 = "unique_k_2",
    tiers = tier_list, takeout = "neither",
    suffixes = c("_1", "_2"),
    score_settings = score_settings
  )
  result
  result$matches[, .(name_compare, name_score)] %>% rowSums(na.rm = TRUE)
  result$matches[tier == "b"]

  # result
  # result$matches[tier == "d"]
  # result$matches
  # result$match_evaluation
  expect_true(is.data.table(result$matches))
})
test_that("sequential word dropping works", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2
  sequential_words_df <- data.frame(
    words = c("word1", "word2"),
    replace = c("", "")
  )
  tier_list <- list(
    a = list(match_type = "exact"),
    b = list(match_type = "fuzzy"),
    c = list(match_type = "multivar", multivar_settings = list(
      logit = NULL, missing = FALSE, wgts = c(1),
      compare_type = "stringdist", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
      top = 1, threshold = NULL
    )),
    d = list(
      match_type = "exact", clean_settings = list(remove_words = TRUE),
      sequential_words = sequential_words_df
    )
  )
  corp_data1[, unique_k_1 := unique_key_1][, unique_key_1 := NULL]
  corp_data2[, unique_k_2 := unique_key_2][, unique_key_2 := NULL]
  corp_data1[, name := Company]
  corp_data2[, name := Name]

  score_settings <- build_score_settings(
    wgts = 1,
    score_type = "stringdist",
    score_var_both = "name"
  )
  # tier_list <- list(a = list(match_type = "exact"))
  result <- tier_match(corp_data1, corp_data2,
    by.x = "name", by.y = "name",
    unique_key_1 = "unique_k_1", unique_key_2 = "unique_k_2",
    tiers = tier_list, takeout = "neither",
    suffixes = c("_1", "_2"),
    score_settings = score_settings
  )
  result$matches[, .(name_compare, name_score)] %>% rowSums(na.rm = TRUE)
  result$matches[tier == "b"]
  result$matches
  expect_true(is.data.table(result$matches))
})
test_that("sequential word dropping doesn't mess up future tiers", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2
  sequential_words_df <- data.frame(
    words = c("Company"),
    replace = c("")
  )
  # corp_data1 <- rbind(corp_data1, data.table(Company = "Ford Motor", unique_key_1 = 11), fill = TRUE)
  corp_data1
  corp_data2 <- rbind(corp_data2, data.table(Name = "Ford Motor Company", unique_key_2 = 11), fill = TRUE)
  corp_data1
  corp_data2
  tier_list <- list(
    a = list(
      match_type = "exact",
      clean_settings = list(remove_words = TRUE),
      sequential_words = sequential_words_df
    ),
    b = list(match_type = "exact")
  )
  corp_data1[, unique_k_1 := unique_key_1][, unique_key_1 := NULL]
  corp_data2[, unique_k_2 := unique_key_2][, unique_key_2 := NULL]
  corp_data1[, name := Company]
  corp_data2[, name := Name]

  score_settings <- build_score_settings(
    wgts = 1,
    score_type = "stringdist",
    score_var_both = "name"
  )
  # tier_list <- list(a = list(match_type = "exact"))
  result <- tier_match(corp_data1, corp_data2,
    by.x = "name", by.y = "name",
    unique_key_1 = "unique_k_1", unique_key_2 = "unique_k_2",
    tiers = tier_list, takeout = "neither",
    suffixes = c("_1", "_2"),
    score_settings = score_settings
  )
  result$matches

  expect_true(is.data.table(result$matches))
})
test_that("dropping observations between tiers works", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2
  sequential_words_df <- data.frame(
    words = c("Company"),
    replace = c("")
  )
  # corp_data1 <- rbind(corp_data1, data.table(Company = "Ford Motor", unique_key_1 = 11), fill = TRUE)
  # corp_data1
  corp_data2 <- rbind(corp_data2, data.table(Name = "Ford Motor Company", unique_key_2 = 11), fill = TRUE)
  # corp_data1
  # corp_data2
  tier_list <- list(
    a = list(
      match_type = "exact"
    ),
    b = list(match_type = "exact")
  )
  corp_data1[, unique_k_1 := unique_key_1][, unique_key_1 := NULL]
  corp_data2[, unique_k_2 := unique_key_2][, unique_key_2 := NULL]
  corp_data1[, name := Company]
  corp_data2[, name := Name]
  score_settings <- build_score_settings(
    wgts = 1,
    score_type = "stringdist",
    score_var_both = "name"
  )
  # tier_list <- list(a = list(match_type = "exact"))
  result <- tier_match(corp_data1, corp_data2,
    by.x = "name", by.y = "name",
    unique_key_1 = "unique_k_1", unique_key_2 = "unique_k_2",
    tiers = tier_list, takeout = "both",
    suffixes = c("_1", "_2"),
    score_settings = score_settings
  )
  result$matches

  expect_true(result$matches[tier == "b", .N] == 0)
})
