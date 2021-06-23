context("scoring testing")
test_that("scoring works at all", {
  corp_data1_test <- data.table(fedmatch::corp_data1)
  corp_data2_test <- data.table(fedmatch::corp_data2)
  corp_data1_test[, id_1 := seq(1, .N)]
  corp_data2_test[, id_2 := seq(1, .N)]
  result <- merge_plus(
    match_type = "fuzzy",
    data1 = corp_data1_test,
    data2 = corp_data2_test, by.x = "Company", by.y = "Name",
    unique_key_1 = "id_1", unique_key_2 = "id_2", suffixes = c("_1", "_2"),
    score_settings = build_score_settings(
      score_var_x = c("Company", "Country"),
      score_var_y = c("Name", "country"),
      wgts = c(.5, .5),
      score_type = c("stringdist", "indicator")
    ),
    filter = .995
  )
  result
  expect_is(result$matches, "data.table")
})
