context("Whether NAs correctly removed")
testthat::test_that("NAs removed in merge_plus", {
  corp_data1_test <- copy(fedmatch::corp_data1)
  corp_data2_test <- copy(fedmatch::corp_data2)
  corp_data1_test[1:5, Company := NA]
  corp_data1_test[, id_1 := seq(1, .N)]
  corp_data2_test[, id_2 := seq(1, .N)]
  corp_data2_test[1:5, Company := NA]
  result <- merge_plus(
    data1 = corp_data1_test,
    data2 = corp_data2_test, by.x = "Company", by.y = "Name",
    unique_key_1 = "id_1", unique_key_2 = "id_2", suffixes = c("_1", "_2")
  )
  result
  expect_is(result$matches, "data.table")
})

testthat::test_that("removing NAs and putting them back works in tier match", {
  corp_data1 <- copy(fedmatch::corp_data1)
  corp_data2 <- copy(fedmatch::corp_data2)
  corp_data1[, Company_v2 := copy(Company)]
  corp_data1[1:5, Company := NA]
  corp_data1[6:10, Company_v2 := NA]

  tier_list <- list(
    a = list(by.x = "Company", by.y = "Name",
             match_type = "exact"),
    b = list(by.x = "Company_v2", by.y = "Name",
             match_type = "exact")
  )
  # tier_list <- list(a = list(match_type = "exact"))
  corp_data1
  result <- tier_match(corp_data1, corp_data2,
                       unique_key_1 = "unique_key_1", unique_key_2 = "unique_key_2",
                       tiers = tier_list, takeout = "neither", verbose = TRUE
  )
  result$matches
  expect_is(result$matches, "data.table")
})
