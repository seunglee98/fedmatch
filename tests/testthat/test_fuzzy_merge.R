context("testing various aspects of fuzzy merging")
testthat::test_that("fuzzy_match returns a data.table", {
  corp_data1_test <- data.table(fedmatch::corp_data1)
  corp_data2_test <- data.table(fedmatch::corp_data2)
  corp_data1_test[, id_1 := seq(1, .N)]
  corp_data2_test[, id_2 := seq(1, .N)]
  result <- fuzzy_match(
    data1 = corp_data1_test,
    data2 = corp_data2_test, by.x = "Company", by.y = "Name",
    unique_key_1 = "id_1", unique_key_2 = "id_2", suffixes = c("_1", "_2")
  )
  expect_is(result, "data.table")
})

testthat::test_that("fuzzy_match returns a non-empty table", {
  corp_data1_test <- data.table(fedmatch::corp_data1)
  corp_data2_test <- data.table(fedmatch::corp_data2)
  corp_data1_test[, id_1 := seq(1, .N)]
  corp_data2_test[, id_2 := seq(1, .N)]
  result <- fuzzy_match(
    data1 = corp_data1_test,
    data2 = corp_data2_test, by.x = "Company", by.y = "Name",
    unique_key_1 = "id_1", unique_key_2 = "id_2", suffixes = c("_1", "_2")
  )
  expect_true(result[, .N] != 0)
})

testthat::test_that("fuzzy_match can handle two ids of the same name", {
  test_func <- function() {
    corp_data1_test <- data.table(fedmatch::corp_data1)
    corp_data2_test <- data.table(fedmatch::corp_data2)
    corp_data1_test[, id := seq(1, .N)]
    corp_data2_test[, id := seq(1, .N)]
    result <- fuzzy_match(
      data1 = corp_data1_test,
      data2 = corp_data2_test, by.x = "Company", by.y = "Name",
      unique_key_1 = "id", unique_key_2 = "id", suffixes = c("_1", "_2")
    )
  }

  expect_error(test_func(), regexp = "unique_key_1")
})

testthat::test_that("fuzzy_match returns proper colnames", {
  corp_data1_test <- data.table(fedmatch::corp_data1)
  corp_data2_test <- data.table(fedmatch::corp_data2)
  corp_data1_test[, id_1 := seq(1, .N)]
  corp_data2_test[, id_2 := seq(1, .N)]
  result <- fuzzy_match(
    data1 = corp_data1_test,
    data2 = corp_data2_test, by.x = "Company", by.y = "Name",
    unique_key_1 = "id_1", unique_key_2 = "id_2", suffixes = c("_1", "_2")
  )

  find_names <- c("Company", "Name", "state_code", "State", "SIC_code", "id_2", "id_1") %in% names(result)
  expect_true(all(find_names))
})
testthat::test_that("weighted jaccard match works", {
  corp_data1_test <- data.table(fedmatch::corp_data1)
  corp_data2_test <- data.table(fedmatch::corp_data2)
  corp_data1_test[, id_1 := seq(1, .N)]
  corp_data2_test[, id_2 := seq(1, .N)]
  setnames(corp_data2_test, "Name", "Company")
  result <- fuzzy_match(
    data1 = corp_data1_test,
    data2 = corp_data2_test, by = "Company",
    unique_key_1 = "id_1", unique_key_2 = "id_2",
    suffixes = c("_1", "_2"),
    fuzzy_settings = list(
      method = "wgt_jaccard", p = 0.1, maxDist = 0.5,
      matchNA = FALSE, nthread = 1
    )
  )
  expect_true(is.data.table(result))
})
testthat::test_that("fuzzy matching works with builder", {
  corp_data1_test <- data.table(fedmatch::corp_data1)
  corp_data2_test <- data.table(fedmatch::corp_data2)
  corp_data1_test[, id_1 := seq(1, .N)]
  corp_data2_test[, id_2 := seq(1, .N)]
  setnames(corp_data2_test, "Name", "Company")
  fuzzy_settings <- build_fuzzy_settings(method = "wgt_jaccard")
  result <- merge_plus(
    data1 = corp_data1_test,
    data2 = corp_data2_test, by = "Company",
    unique_key_1 = "id_1", unique_key_2 = "id_2",
    suffixes = c("_1", "_2"),
    fuzzy_settings = fuzzy_settings
  )
  expect_true(is.data.table(result$matches))
})
