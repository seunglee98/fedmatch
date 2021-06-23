context("getting multivar to work with multiple variables")
testthat::test_that("multivar works at all with multiple variables", {
  corp_data1_test <- data.table(fedmatch::corp_data1)
  corp_data2_test <- data.table(fedmatch::corp_data2)
  corp_data1_test[, id_1 := seq(1, .N)]
  corp_data2_test[, id_2 := seq(1, .N)]
  # corp_data1_test
  corp_data2_test[, Country := country]
  corp_data2_test[, Company := Name]
  set.seed(111)
  fake_result_table <- data.table(
    match = sample(c(1, 0, 1), 1e5, replace = T),
    Company_compare = runif(1e5),
    Country_compare = sample(c(1, 0), 1e5, replace = T)
  )

  logit_model <- glm(match ~ Company_compare + Country_compare,
    family = "binomial",
    data = fake_result_table
  )
  # corp_data2_test
  # fake_result_table
  summary(logit_model)
  # class(logit_model)
  result <- multivar_match(corp_data1_test, corp_data2_test,
    by = c("Country", "Company"), unique_key_1 = "id_1",
    unique_key_2 = "id_2", logit = logit_model, compare_type = c("indicator", "stringdist"),
    wgts = NULL, nthread = 1,
    suffixes = c("_1", "_2")
  )
  result
  expect_is(result, "data.table")
})
testthat::test_that("multivar works as part of a tier match", {
  corp_data1_test <- data.table(fedmatch::corp_data1)
  corp_data2_test <- data.table(fedmatch::corp_data2)
  corp_data1_test[, id_1 := seq(1, .N)]
  corp_data2_test[, id_2 := seq(1, .N)]
  # corp_data1_test
  corp_data2_test[, Country := country]
  corp_data2_test[, Company := Name]
  set.seed(111)
  fake_result_table <- data.table(
    match = sample(c(1, 0, 1), 1e5, replace = T),
    Company_compare = runif(1e5),
    Country_compare = sample(c(1, 0), 1e5, replace = T)
  )

  logit_model <- glm(match ~ Company_compare + Country_compare,
    family = "binomial",
    data = fake_result_table
  )

  tier_list <- list(
    a = list(match_type = "exact"),
    b = list(match_type = "fuzzy"),
    c = list(
      match_type = "multivar",
      by.x = c("Company", "Country"),
      by.y = c("Company", "Country"),
      multivar_settings = list(
        logit = logit_model, missing = F,
        compare_type = c("stringdist", "indicator"), blocks = NULL, blocks.x = NULL, blocks.y = NULL,
        top = 1, threshold = NULL
      )
    ),
    d = list(match_type = "exact", clean.args = list(remove_words = T))
  )
  # tier_list[["c"]][["by.y"]]
  corp_data1
  result <- tier_match(corp_data1_test, corp_data2_test,
    by.x = "Company", by.y = "Name",
    unique_key_1 = "id_1", unique_key_2 = "id_2",
    tiers = tier_list, takeout = "neither",
    suffixes = c("_1", "_2")
  )
  result$matches[tier == "c"]
  expect_is(result$matches, "data.table")
})
