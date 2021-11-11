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
  result <- merge_plus(
    data1 = corp_data1_test,
    data2 = corp_data2_test, by = "Company",
    match_type = "fuzzy",
    unique_key_1 = "id_1", unique_key_2 = "id_2",
    suffixes = c("_1", "_2"),
    fuzzy_settings = build_fuzzy_settings(
      method = "wgt_jaccard", p = 0.1, maxDist = 0.5,
      matchNA = FALSE, nthread = 2
    )
  )
  expect_true(is.data.table(result$matches))
})
testthat::test_that("fuzzy matching works with builder", {
  corp_data1_test <- data.table(fedmatch::corp_data1)
  corp_data2_test <- data.table(fedmatch::corp_data2)
  # corp_data1_test <- corp_data1_test[sample(1:.N, 1e7, replace = T)]
  corp_data1_test[, id_1 := seq(1, .N)]
  corp_data2_test[, id_2 := seq(1, .N)]
  setnames(corp_data2_test, "Name", "Company")
  fuzzy_settings <- build_fuzzy_settings(
    method = "wgt_jaccard",
    nthread = 2
  )
  result <- merge_plus(
    data1 = corp_data1_test,
    match_type = "fuzzy",
    data2 = corp_data2_test, by = "Company",
    unique_key_1 = "id_1", unique_key_2 = "id_2",
    suffixes = c("_1", "_2"),
    fuzzy_settings = fuzzy_settings
  )
  expect_true(is.data.table(result$matches))
})
testthat::test_that("fuzzy matching works with unique key names", {
  corp_data1_test <- data.table(fedmatch::corp_data1)
  corp_data2_test <- data.table(fedmatch::corp_data2)
  # corp_data1_test <- corp_data1_test[sample(1:.N, 1e7, replace = T)]
  corp_data1_test[, unique_key_1 := seq(1, .N)]
  corp_data2_test[, unique_key_2 := seq(1, .N)]
  setnames(corp_data2_test, "Name", "Company")

  result <- fedmatch::merge_plus(
    data1 = corp_data1_test,
    match_type = "fuzzy",
    data2 = corp_data2_test, by.x = "Company", by.y = "Company",
    unique_key_1 = "unique_key_1", unique_key_2 = "unique_key_2",
    suffixes = c("_1", "_2")
  )
  expect_true(is.data.table(result$matches))
})
testthat::test_that("fuzzy matching doesn't work the same if data is inverted", {
  dummy_data1 <- data.table(id1 = 1:10,
                            name1 = c(rep("abc",5), rep( "abd", 5)))
  dummy_data2 <- data.table(id2 = 1,
                            name2 = "abc")

  result1 <- fedmatch::merge_plus(
    data1 = dummy_data1,
    match_type = "fuzzy",
    data2 = dummy_data2, by.x = "name1", by.y = "name2",
    unique_key_1 = "id1", unique_key_2 = "id2",
    fuzzy_settings = build_fuzzy_settings(maxDist = .75),
    suffixes = c("_1", "_2"))
  result2 <- fedmatch::merge_plus(
    data2 = dummy_data1,
    match_type = "fuzzy",
    data1 = dummy_data2, by.x = "name2", by.y = "name1",
    unique_key_1 = "id2", unique_key_2 = "id1",
    fuzzy_settings = build_fuzzy_settings(maxDist = .75),
    suffixes = c("_1", "_2"))

  expect_true(result1$matches[, .N] != result2$matches[, .N])
})

testthat::test_that("fuzzy matching doesn't work wgt jaccard the same if data is inverted", {
  dummy_data1 <- data.table(id1 = 1:10,
                            name = "abc")
  dummy_data2 <- data.table(id2 = 1,
                            name = "abc")
  result1 <- fedmatch::merge_plus(
    data1 = dummy_data1,
    match_type = "fuzzy",
    data2 = dummy_data2, by.x = "name", by.y = "name",
    unique_key_1 = "id1", unique_key_2 = "id2",
    suffixes = c("_1", "_2"),
    fuzzy_settings = build_fuzzy_settings(method = "wgt_jaccard"))
  result2 <- fedmatch::merge_plus(
    data2 = dummy_data1,
    match_type = "fuzzy",
    data1 = dummy_data2, by.x = "name", by.y = "name",
    unique_key_1 = "id2", unique_key_2 = "id1",
    suffixes = c("_1", "_2"),
    fuzzy_settings = build_fuzzy_settings(method = "wgt_jaccard"))

  expect_true(result1$matches[, .N] != result2$matches[, .N])
})
