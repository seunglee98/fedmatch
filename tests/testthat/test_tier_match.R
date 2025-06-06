
test_that("tier_match can handle unique keys being named unique_key_1 and unique_key_2", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2
  tier_list <- list(
    a = list(match_type = "exact"),
    b = list(match_type = "fuzzy", fuzzy_settings = build_fuzzy_settings(nthread = 1)),
    c = list(match_type = "multivar", multivar_settings = list(
      logit = NULL, missing = FALSE, wgts = 1,
      compare_type = "stringdist", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
      top = 1, threshold = NULL, nthread = 1
    ))
  )
  # tier_list <- list(a = list(match_type = "exact"))
  corp_data1
  result <- tier_match(corp_data1, corp_data2,
    by.x = "Company", by.y = "Name",
    unique_key_1 = "unique_key_1", unique_key_2 = "unique_key_2",
    tiers = tier_list, takeout = "neither", verbose = TRUE
  )

  # result
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
      b = list(match_type = "fuzzy", fuzzy_settings = build_fuzzy_settings(nthread = 1)),
      c = list(match_type = "multivar", multivar_settings = list(
        logit = NULL, missing = FALSE, wgts = c(1),
        compare_type = "stringdist", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
        top = 1, threshold = NULL, nthread = 1
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
    b = list(match_type = "fuzzy", fuzzy_settings = build_fuzzy_settings(nthread = 1)),
    c = list(match_type = "multivar", multivar_settings = list(
      logit = NULL, missing = FALSE, wgts = c(1),
      compare_type = "stringdist", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
      top = 1, threshold = NULL, nthread = 1
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
    b = list(match_type = "fuzzy", fuzzy_settings = build_fuzzy_settings(nthread = 1)),
    c = list(match_type = "multivar", multivar_settings = build_multivar_settings(
      logit = NULL, missing = FALSE, wgts = c(1),
      compare_type = "stringdist", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
      top = 1, threshold = NULL, nthread = 1
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
    score_settings = score_settings, verbose = T
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
    b = list(match_type = "fuzzy", fuzzy_settings = build_fuzzy_settings(nthread = 1)),
    c = build_tier(match_type = "multivar", multivar_settings = list(
      logit = NULL, missing = FALSE, wgts = c(1),
      compare_type = "stringdist", blocks = NULL, blocks.x = NULL, blocks.y = NULL,
      top = 1, threshold = NULL, nthread = 1
    )),
    d = build_tier(
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
    score_settings = score_settings, verbose = T
  )
  result$matches

  expect_true(result$matches[tier == "b", .N] == 0)
})
test_that("fuzzy match works with different by's in tier_match", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2
  # rbind(corp_data2, data.table(Name = "Ford Motor Company", unique_key_2 = 11), fill = TRUE)
  # corp_data1
  # corp_data2
  tier_list <- list(
    a = list(
      match_type = "fuzzy",
      by.x = "name",
      by.y = "name",
      fuzzy_settings = build_fuzzy_settings(nthread = 1)
    ),
    a = list(
      match_type = "fuzzy",
      by.x = "name2",
      by.y = "name",
      fuzzy_settings = build_fuzzy_settings(nthread = 1)
    )
  )
  corp_data1[, unique_k_1 := unique_key_1][, unique_key_1 := NULL]
  corp_data2[, unique_k_2 := unique_key_2][, unique_key_2 := NULL]
  corp_data1[, name := Company]
  corp_data2[, name := Name]
  corp_data1[, name2 := Company]
  # corp_data1[, name := NULL]
  corp_data1
  result <- tier_match(corp_data1, corp_data2,
                       unique_key_1 = "unique_k_1", unique_key_2 = "unique_k_2",
                       tiers = tier_list, takeout = "neither",
                       suffixes = c("_1", "_2"), verbose = T
  )
  expect_true(result$matches[tier == "a", .N] == 6)
})
test_that("cleaning on different tiers isn't preserved", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2
  # rbind(corp_data2, data.table(Name = "Ford Motor Company", unique_key_2 = 11), fill = TRUE)
  # corp_data1
  # corp_data2
  tier_list <- list(
    a = list(
      match_type = "exact",
      by.x = "name",
      by.y = "name",
      clean = T
    ),
    b = list(
      match_type = "exact",
      by.x = "name",
      by.y = "name"
    )
  )
  corp_data1[, unique_k_1 := unique_key_1][, unique_key_1 := NULL]
  corp_data2[, unique_k_2 := unique_key_2][, unique_key_2 := NULL]
  corp_data1[, name := Company]
  corp_data2[, name := Name]
  corp_data1[, name2 := Company]
  # corp_data1[, name := NULL]
  corp_data1
  result <- tier_match(corp_data1, corp_data2,
                       unique_key_1 = "unique_k_1", unique_key_2 = "unique_k_2",
                       tiers = tier_list, takeout = "neither",
                       suffixes = c("_1", "_2"), verbose = T
  )
  # make sure that the two names matched in each aren't the same, that is,
  # the second tier isn't being cleaned.
  expect_true(result$matches$name_1[[1]] != result$matches$name_1[[2]] )
})
